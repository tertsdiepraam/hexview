use std::{
    error::Error,
    io::{self, Read},
    path::PathBuf,
    rc::Rc,
    time::Duration,
};

use clap::Parser;
use ratatui::{
    backend::{Backend, CrosstermBackend},
    crossterm::{
        event::{
            self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyModifiers,
            MouseButton, MouseEvent, MouseEventKind,
        },
        execute,
        terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    },
    layout::{
        Constraint::{Length, Min},
        Layout, Margin, Position, Rect,
    },
    style::{Color, Style, Stylize},
    terminal::{Frame, Terminal},
    text::{Line, Span, Text},
    widgets::{
        Block, Borders, Clear, Padding, Paragraph, Row, Scrollbar, ScrollbarOrientation,
        ScrollbarState, Table,
    },
};

#[derive(Parser)]
#[command(version, author, about, long_about = None)]
struct Args {
    /// The file to view
    file: PathBuf,
    #[arg(short, long, default_value_t = 8)]
    bytes: usize,
    #[arg(short, long, default_value_t = 2)]
    groups: usize,
    #[arg(short, long, default_value_t = true)]
    color: bool,
    #[arg(short, long, default_value_t = false)]
    spacing: bool,
}

#[derive(PartialEq, Eq)]
enum SearchBuffer {
    HexSearch,
    AsciiSearch,
    Goto,
}

struct App {
    view: usize,
    spacing: bool,
    color: bool,
    cursor: usize,
    buffer: Vec<u8>,
    path: PathBuf,
    current_search_buffer: Option<SearchBuffer>,
    hex_search_buffer: EditBuffer,
    ascii_search_buffer: EditBuffer,
    goto_buffer: EditBuffer,
    help_popup: bool,
    show_inspector: bool,
    hex_area: Rect,
    ascii_area: Rect,
    bytes_per_group: usize,
    groups: usize,
}

impl App {
    fn move_cursor(&mut self, delta: isize) {
        let offset = self.cursor.saturating_add_signed(delta);
        self.set_cursor(offset)
    }

    fn set_cursor(&mut self, offset: usize) {
        self.cursor = offset.min(self.buffer.len() - 1);
        self.scroll_to_cursor();
    }

    fn scroll(&mut self, lines: isize) {
        self.set_view(
            self.view
                .saturating_add_signed(lines * self.bytes_per_line() as isize),
        )
    }

    fn scroll_and_move_cursor(&mut self, lines: isize) {
        let old_view = self.view;
        self.scroll(lines);
        let diff = self.view as isize - old_view as isize;
        self.move_cursor(diff);
    }

    fn bytes_per_line(&self) -> usize {
        self.bytes_per_group * self.groups
    }

    fn set_view(&mut self, offset: usize) {
        self.view = offset.min(self.buffer.len());
        self.view -= self.view % self.bytes_per_line();
    }

    fn lines_displayed(&self) -> usize {
        self.hex_area.height as usize
    }

    fn scroll_to_cursor(&mut self) {
        if self.cursor < self.view {
            self.view = self.cursor_line_offset();
        } else if self.cursor >= self.view + (self.bytes_per_line() * self.lines_displayed()) {
            self.view =
                self.cursor_line_offset() - self.bytes_per_line() * (self.lines_displayed() - 1);
        }
    }

    fn cursor_line_offset(&self) -> usize {
        self.cursor - (self.cursor % self.bytes_per_line())
    }

    fn find(&mut self, pattern: &[u8]) {
        let mut first = None;
        for i in 0..(self.buffer.len() - pattern.len()) {
            if pattern == &self.buffer[i..(i + pattern.len())] {
                if i > self.cursor {
                    first = Some(i);
                    break;
                }
                if first.is_none() {
                    first = Some(i);
                }
            }
        }

        if let Some(first) = first {
            self.set_cursor(first);
        }
    }
}

#[derive(Clone)]
struct EditBuffer {
    buffer: Vec<char>,
    cursor_position: usize,
    name: &'static str,
    placeholder_text: &'static str,
    #[allow(clippy::type_complexity)]
    action: Rc<dyn Fn(&EditBuffer, &mut App)>,
}

impl EditBuffer {
    fn new(
        name: &'static str,
        bottom_text: &'static str,
        action: impl Fn(&EditBuffer, &mut App) + 'static,
    ) -> EditBuffer {
        EditBuffer {
            buffer: Vec::new(),
            cursor_position: 0,
            name,
            placeholder_text: bottom_text,
            action: Rc::new(action),
        }
    }
}

impl App {
    fn current_buffer(&self) -> Option<&EditBuffer> {
        match self.current_search_buffer {
            None => None,
            Some(SearchBuffer::HexSearch) => Some(&self.hex_search_buffer),
            Some(SearchBuffer::AsciiSearch) => Some(&self.ascii_search_buffer),
            Some(SearchBuffer::Goto) => Some(&self.goto_buffer),
        }
    }

    fn current_buffer_mut(&mut self) -> Option<&mut EditBuffer> {
        match self.current_search_buffer {
            None => None,
            Some(SearchBuffer::HexSearch) => Some(&mut self.hex_search_buffer),
            Some(SearchBuffer::AsciiSearch) => Some(&mut self.ascii_search_buffer),
            Some(SearchBuffer::Goto) => Some(&mut self.goto_buffer),
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let mut file = std::fs::File::open(&args.file).unwrap();
    let mut buffer: Vec<u8> = Vec::new();
    file.read_to_end(&mut buffer).unwrap();

    // setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let app = App {
        view: 0,
        color: args.color,
        spacing: args.spacing,
        cursor: 0,
        bytes_per_group: args.bytes,
        groups: args.groups,
        buffer,
        current_search_buffer: None,
        path: args.file,
        help_popup: false,
        show_inspector: false,
        hex_area: Rect::new(0, 0, 0, 0),
        ascii_area: Rect::new(0, 0, 0, 0),
        hex_search_buffer: EditBuffer::new(
            "HEX SEARCH",
            "Enter a byte pattern to search for",
            |buf, app| {
                if !buf
                    .buffer
                    .iter()
                    .all(|c| c.is_ascii_hexdigit() || *c == ' ')
                {
                    return;
                }

                let chars: String = buf.buffer.iter().filter(|x| **x != ' ').collect();

                if chars.len() % 2 > 0 {
                    return;
                }

                let mut bytes: Vec<u8> = Vec::new();
                for i in (0..chars.len()).step_by(2) {
                    bytes.push(u8::from_str_radix(&chars[i..(i + 2)], 16).unwrap())
                }

                app.find(&bytes);
            },
        ),
        ascii_search_buffer: EditBuffer::new(
            "ASCII SEARCH",
            "Enter a text string to search for",
            |buf, app| app.find(buf.buffer.iter().collect::<String>().as_bytes()),
        ),
        goto_buffer: EditBuffer::new(
            "GOTO",
            "Go to an offset, or a percentage with '%'.",
            |buf, app| {
                let s = buf.buffer.iter().collect::<String>();
                if let Some(rest) = s.strip_suffix('%') {
                    let Ok(x) = rest.parse::<usize>() else {
                        return;
                    };
                    if x > 100 {
                        return;
                    }
                    app.set_cursor(x * (app.buffer.len() - 1) / 100)
                }
                let Ok(x) = usize::from_str_radix(&s, 16) else {
                    return;
                };

                if x < app.buffer.len() {
                    app.set_cursor(x)
                }
            },
        ),
    };

    // create app and run it
    let res = run_app(&mut terminal, app);

    // restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{err:?}");
    }

    Ok(())
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> io::Result<()> {
    loop {
        terminal.draw(|f| ui(&mut app, f))?;
        while event::poll(Duration::ZERO)? {
            let event = event::read()?;

            // We always allow quitting with ctrl+c
            if let Event::Key(key) = event {
                if key.modifiers == KeyModifiers::CONTROL && key.code == KeyCode::Char('c') {
                    return Ok(());
                }
            }

            // The help popup takes precedence over everything else
            if app.help_popup {
                if let Event::Key(key) = event {
                    if let KeyCode::Char('?' | 'q') | KeyCode::Esc = key.code {
                        app.help_popup = false;
                    }
                }
                // We ignore everything until the popup is gone
                continue;
            }

            // Now we do our normal event handling
            if let Event::Key(key) = event {
                if key.code == KeyCode::Up {
                    app.move_cursor(-(app.bytes_per_line() as isize));
                }

                if key.code == KeyCode::Down {
                    app.move_cursor(app.bytes_per_line() as isize);
                }

                if key.code == KeyCode::PageDown {
                    app.scroll_and_move_cursor(app.lines_displayed() as isize);
                }

                if key.code == KeyCode::PageUp {
                    app.scroll_and_move_cursor(-(app.lines_displayed() as isize));
                }

                if let Some(buf) = app.current_buffer_mut() {
                    if key.code == KeyCode::Right {
                        buf.cursor_position += 1;
                        buf.cursor_position = buf.cursor_position.min(buf.buffer.len());
                    }

                    if key.code == KeyCode::Left {
                        buf.cursor_position = buf.cursor_position.saturating_sub(1);
                    }

                    if let KeyCode::Char(c) = key.code {
                        buf.buffer.insert(buf.cursor_position, c);
                        buf.cursor_position += 1;
                    }

                    if key.code == KeyCode::Home {
                        buf.cursor_position = 0;
                    }

                    if key.code == KeyCode::End {
                        buf.cursor_position = buf.buffer.len();
                    }

                    if key.code == KeyCode::Backspace && buf.cursor_position > 0 {
                        buf.buffer.remove(buf.cursor_position - 1);
                        buf.cursor_position = buf.cursor_position.saturating_sub(1);
                        buf.cursor_position = buf.cursor_position.min(buf.buffer.len());
                    }

                    if key.code == KeyCode::Delete && buf.cursor_position < buf.buffer.len() {
                        buf.buffer.remove(buf.cursor_position);
                    }

                    if key.code == KeyCode::Enter {
                        let buf = buf.clone();
                        (buf.action)(&buf, &mut app)
                    }
                }

                // Vim-like controls when not typing in a buffer
                if app.current_search_buffer.is_none() {
                    if key.code == KeyCode::Right {
                        app.move_cursor(1);
                    }
                    if key.code == KeyCode::Left {
                        app.move_cursor(-1);
                    }
                    if key.code == KeyCode::Char('q') {
                        return Ok(());
                    }
                    if key.code == KeyCode::Char('j') {
                        app.move_cursor(app.bytes_per_line() as isize);
                    }
                    if key.code == KeyCode::Char('k') {
                        app.move_cursor(-(app.bytes_per_line() as isize));
                    }
                    if key.code == KeyCode::Char('h') {
                        app.move_cursor(-1);
                    }
                    if key.code == KeyCode::Char('l') {
                        app.move_cursor(1);
                    }
                    if key.code == KeyCode::Char('g') {
                        app.current_search_buffer = Some(SearchBuffer::Goto);
                    }
                    if key.code == KeyCode::Char('s') {
                        app.current_search_buffer = Some(SearchBuffer::AsciiSearch);
                    }
                    if key.code == KeyCode::Char('/') {
                        app.current_search_buffer = Some(SearchBuffer::HexSearch);
                    }
                    if key.code == KeyCode::Char('i') {
                        app.show_inspector = !app.show_inspector;
                    }
                    if key.code == KeyCode::Char('?') {
                        app.help_popup = true;
                    }
                }

                if key.modifiers.contains(KeyModifiers::CONTROL) && key.code == KeyCode::Char('d') {
                    app.scroll_and_move_cursor(app.lines_displayed() as isize);
                }
                if key.modifiers.contains(KeyModifiers::CONTROL) && key.code == KeyCode::Char('u') {
                    app.scroll_and_move_cursor(-(app.lines_displayed() as isize));
                }
                if key.code == KeyCode::Esc {
                    app.current_search_buffer = None;
                }
            }

            if let Event::Mouse(MouseEvent {
                kind: MouseEventKind::ScrollDown,
                ..
            }) = event
            {
                app.scroll(1);
            }

            if let Event::Mouse(MouseEvent {
                kind: MouseEventKind::ScrollUp,
                ..
            }) = event
            {
                app.scroll(-1);
            }

            if let Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column,
                row,
                modifiers: _,
            }) = event
            {
                if app.hex_area.contains(Position::new(column, row)) {
                    let x = (column - app.hex_area.x - 8) / 3;
                    let y = row - app.hex_area.y;
                    app.set_cursor(app.view + y as usize * app.bytes_per_line() + x as usize);
                }
                if app.ascii_area.contains(Position::new(column, row)) {
                    let x = column - app.ascii_area.x;
                    let y = row - app.ascii_area.y;
                    app.set_cursor(app.view + y as usize * app.bytes_per_line() + x as usize);
                }
            }
        }
    }
}

fn ui(app: &mut App, frame: &mut Frame) {
    let size = match app.buffer.len() {
        s if s < 1024 => format!("{s} B"),
        s if s < 1024usize.pow(2) => format!("{:.1} KB", s as f64 / 1024.0),
        s if s < 1024usize.pow(3) => format!("{:.1} MB", s as f64 / 1024.0f64.powf(2.0)),
        s => format!("{:.1} GB", s as f64 / 1024.0f64.powf(3.0)),
    };
    let offset = app.cursor;
    let percentage = (app.cursor as f64 * 100.0) / app.buffer.len() as f64;
    let main_block = Block::new().title_bottom(format!(
        "Press `?` for help. Viewing: {}, file size: {size} ({offset:0>8x}, {percentage:.2}%)",
        app.path.display(),
    ));

    let spacing = app.spacing as usize;
    let hex_width = 2 // borders
        + 8 // offset
        + 2 * spacing // padding of block
        + 2 * spacing * app.groups // padding of columns 
        + (3 * app.bytes_per_group * app.groups); // bytes

    let ascii_width = 2 + (app.bytes_per_group * app.groups) + if app.spacing { 2 } else { 0 };
    let width = hex_width + ascii_width + 1; // scrollbar

    let mut frame_size = frame.size();
    frame_size.width = frame_size.width.min(width as u16);

    frame.render_widget(&main_block, frame_size);
    let main = main_block.inner(frame_size);

    let [top_area, bottom_area, inspector_area] = Layout::vertical([
        Min(0),
        Length(if app.current_search_buffer.is_some() {
            3
        } else {
            0
        }),
        Length(if app.show_inspector { 7 } else { 0 }),
    ])
    .areas(main);

    if let Some(buf) = &app.current_buffer() {
        let popup_block = Block::bordered()
            .title(buf.name)
            .padding(Padding::horizontal(1));

        frame.render_widget(&popup_block, bottom_area);
        let search_area = popup_block.inner(bottom_area);
        let s = buf.buffer.iter().collect::<String>();
        let text = Paragraph::new(Text::from(if s.is_empty() {
            buf.placeholder_text.fg(Color::DarkGray)
        } else {
            s.into()
        }));
        frame.render_widget(&text, search_area);
        let x = search_area.x + buf.cursor_position as u16;
        frame.set_cursor(x, search_area.y);
    }

    if app.show_inspector {
        let inspector_block = Block::bordered().title("Inspector (press `I` to hide)");

        let x = app.buffer[app.cursor];
        let x8 = u8::from_le_bytes(chunk(&app.buffer, app.cursor));
        let x16 = u16::from_le_bytes(chunk(&app.buffer, app.cursor));
        let x32 = u32::from_le_bytes(chunk(&app.buffer, app.cursor));
        let x64 = u64::from_le_bytes(chunk(&app.buffer, app.cursor));

        let inspector_text = Paragraph::new(vec![
            Line::from(format!(
                "Hexadecimal: {x:>2x}, Octal: {x:>3o}, Decimal: {x:>3}, Binary: {:0>4b} {:0>4b}",
                x >> 4,
                x & 0xf
            )),
            Line::from(format!(
                "u8: {}, u16: {}, u32: {}, u64: {}",
                x8, x16, x32, x64
            )),
            Line::from(format!(
                "i8: {}, i16: {}, i32: {}, i64: {}",
                x8 as i8, x16 as i16, x32 as i32, x64 as i64
            )),
            Line::from(format!("f32: {}", f32::from_bits(x32),)),
            Line::from(format!("f64: {}", f64::from_bits(x64),)),
        ]);
        frame.render_widget(&inspector_block, inspector_area);
        frame.render_widget(&inspector_text, inspector_block.inner(inspector_area));
    }

    let [hex_area, ascii_area, scrollbar_area, _] = Layout::horizontal([
        Length(hex_width as u16),
        Length(ascii_width as u16),
        Length(1),
        Min(0),
    ])
    .areas(top_area);

    let block = Block::new()
        .title("HEX")
        .padding(Padding::uniform(if app.spacing { 1 } else { 0 }))
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::DarkGray));

    frame.render_widget(&block, hex_area);
    let hex_area = block.inner(hex_area);

    let block = Block::new()
        .title("ASCII")
        .padding(Padding::uniform(if app.spacing { 1 } else { 0 }))
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::DarkGray));

    frame.render_widget(&block, ascii_area);
    let ascii_area = block.inner(ascii_area);

    // Store the rects to properly handle mouse input
    app.hex_area = hex_area;
    app.ascii_area = ascii_area;

    let lines_to_draw = hex_area.height as usize;

    let mut scrollbar_state = ScrollbarState::new(app.buffer.len())
        .position(app.cursor)
        .viewport_content_length(lines_to_draw);
    frame.render_stateful_widget(
        Scrollbar::new(ScrollbarOrientation::VerticalRight).track_symbol(Some("│")),
        scrollbar_area,
        &mut scrollbar_state,
    );

    let mut lines = Vec::new();
    'outer: for i in 0..lines_to_draw {
        let line_offset = app.view + i * app.bytes_per_line();

        // Write the offset at the start of the line
        let mut spans = Vec::new();
        let offset_string = format!("{:x}", line_offset);
        spans.push(Span::styled(
            "0".repeat(8 - offset_string.len()),
            Style::default().fg(Color::DarkGray),
        ));
        spans.push(Span::raw(offset_string));

        for byte_offset in line_offset..(line_offset + app.bytes_per_line()) {
            if byte_offset % app.bytes_per_group == 0 {
                if app.spacing {
                    spans.push(" │ ".fg(Color::DarkGray));
                } else {
                    spans.push("│".fg(Color::DarkGray));
                }
            } else {
                spans.push(Span::raw(" "));
            }
            match app.buffer.get(byte_offset) {
                Some(byte) => spans.push(write_byte(app, byte_offset, *byte)),
                None => {
                    lines.push(Line::from(spans));
                    break 'outer;
                }
            };
        }

        lines.push(Line::from(spans))
    }

    frame.render_widget(Paragraph::new(Text::from(lines)), hex_area);

    let mut lines = Vec::new();
    'outer: for i in 0..lines_to_draw {
        // Write the offset at the start of the line
        let line_offset = app.view + i * app.bytes_per_line();

        let mut spans = Vec::new();
        for byte_offset in line_offset..(line_offset + app.bytes_per_line()) {
            match app.buffer.get(byte_offset) {
                Some(byte) => spans.push(render_ascii_char(app, byte_offset, *byte)),
                None => {
                    lines.push(Line::from(spans));
                    break 'outer;
                }
            };
        }

        lines.push(Line::from(spans))
    }

    frame.render_widget(Paragraph::new(Text::from(lines)), ascii_area);

    if app.help_popup {
        let help_block = Block::bordered().title("Help (press `q` or `?` to dismiss)");
        let area = centered_rect(60, 20, frame_size);
        frame.render_widget(Clear, area);
        let area = area.inner(Margin::new(1, 1));
        frame.render_widget(&help_block, area);

        let keys = vec![
            (vec!["?"], "Toggle help"),
            (vec!["Q", "CTRL+C"], "Quit"),
            (vec!["UP", "K"], "Move cursor up"),
            (vec!["DOWN", "J"], "Move cursor down"),
            (vec!["LEFT", "H"], "Move cursor left"),
            (vec!["RIGHT", "L"], "Move cursor right"),
            (vec!["PAGE UP", "CTRL+U"], "Page up"),
            (vec!["PAGE DOWN", "CTRL+D"], "Page down"),
            (vec!["G"], "Goto offset"),
            (vec!["S"], "Search ASCII string"),
            (vec!["/"], "Search HEX string"),
            (vec!["I"], "Toggle inspector"),
        ];

        let table = Table::new(
            keys.iter().map(|(keys, action)| {
                let mut key_line = vec![keys[0].bold()];
                for key in &keys[1..] {
                    key_line.push(", ".into());
                    key_line.push(key.bold());
                }
                Row::new(vec![Line::from(key_line), Line::from(*action)])
            }),
            vec![Length(20), Min(0)],
        )
        .header(Row::new(vec!["Key", "Action"]).underlined());

        frame.render_widget(table, help_block.inner(area).inner(Margin::new(1, 1)));
    }
}

enum ByteCategory {
    Null,
    Printable,
    Whitespace,
    Other,
    NonAscii,
}

fn category(byte: u8) -> ByteCategory {
    if byte == 0 {
        ByteCategory::Null
    } else if byte.is_ascii_graphic() {
        ByteCategory::Printable
    } else if byte.is_ascii_whitespace() {
        ByteCategory::Whitespace
    } else if byte.is_ascii() {
        ByteCategory::Other
    } else {
        ByteCategory::NonAscii
    }
}

fn byte_color(byte: u8) -> Color {
    match category(byte) {
        ByteCategory::Null => Color::DarkGray,
        ByteCategory::Printable => Color::Cyan,
        ByteCategory::Whitespace => Color::Green,
        ByteCategory::Other => Color::Yellow,
        ByteCategory::NonAscii => Color::Red,
    }
}

fn write_byte(app: &App, offset: usize, byte: u8) -> Span<'static> {
    let s = format!("{:0>2x}", byte);
    let mut style = Style::default();
    if app.color {
        style = style.fg(byte_color(byte));
    }
    if app.cursor == offset {
        style = style.fg(Color::White).bold().underlined();
    }
    Span::styled(s, style)
}

fn render_ascii_char(app: &App, offset: usize, byte: u8) -> Span<'static> {
    let c = match byte {
        0..=31 => '.',
        byte @ (b' '..=b'~') => byte as char,
        127 => '.',
        128..=u8::MAX => '.',
    };
    let mut style = Style::default();

    if app.cursor == offset {
        if app.color {
            style = style.fg(Color::White);
        }
        style = style.bold().underlined();
    } else if app.color {
        style = style.fg(byte_color(byte));
    };

    Span::styled(String::from(c), style)
}

fn centered_rect(width: u16, height: u16, r: Rect) -> Rect {
    let popup_layout = Layout::vertical([Min(0), Length(height), Min(0)]).split(r);

    Layout::horizontal([Min(0), Length(width), Min(0)]).split(popup_layout[1])[1]
}

fn chunk<const N: usize>(arr: &[u8], offset: usize) -> [u8; N]
where
    [u8; N]: Default,
{
    match arr[offset..].first_chunk::<N>() {
        Some(chunk) => *chunk,
        None => Default::default(),
    }
}
