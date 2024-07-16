use std::{
    error::Error,
    io::{self, Read},
    ops::ControlFlow,
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
    /// The number of bytes per group
    bytes: usize,
    #[arg(short, long, default_value_t = 2)]
    /// The number of groups (columns) to show
    groups: usize,
    #[arg(short, long, default_value_t = true)]
    color: bool,
    /// Add a bit of additional padding
    #[arg(short, long, default_value_t = false)]
    spacing: bool,
}

#[derive(PartialEq, Eq)]
enum SearchBufferType {
    HexSearch,
    AsciiSearch,
    Goto,
}

struct App {
    /// Offset of the byte in the top-left corner
    view: usize,
    /// Whether to render with some additional padding
    spacing: bool,
    /// Whether to enable colors
    color: bool,
    /// Position and length of the cursor
    cursor: (usize, usize),
    buffer: Rc<Vec<u8>>,
    path: PathBuf,
    search_str_len: usize,
    /// Last occurrence we jumped to
    occurrence: usize,
    /// Whether to restrict search to aligned sequences
    search_aligned: bool,
    positions: Vec<usize>,
    alignment: Alignment,
    current_search_buffer: Option<SearchBufferType>,
    hex_search_buffer: SearchBuffer,
    ascii_search_buffer: SearchBuffer,
    goto_buffer: SearchBuffer,
    help_popup: bool,
    show_inspector: bool,
    hex_area: Rect,
    ascii_area: Rect,
    bytes_per_group: usize,
    groups: usize,
}

impl App {
    fn move_cursor(&mut self, delta: isize) {
        let offset = self.cursor.0.saturating_add_signed(delta);
        self.set_cursor(offset, self.cursor.1)
    }

    fn set_cursor(&mut self, offset: usize, len: usize) {
        let position = offset.min(self.buffer.len() - 1);
        self.cursor = (position, len);
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
        let start = self.cursor.0;
        let end = start + self.cursor.1;
        if self.cursor.0 < self.view {
            self.view = self.line_offset(start);
        } else if end >= self.view + (self.bytes_per_line() * self.lines_displayed()) {
            self.view =
                self.line_offset(end) - self.bytes_per_line() * (self.lines_displayed() - 1);
        }
    }

    fn line_offset(&self, offset: usize) -> usize {
        offset - (offset % self.bytes_per_line())
    }
}

#[derive(Clone)]
struct SearchBuffer {
    buffer: Vec<char>,
    cursor_position: usize,
    name: &'static str,
    placeholder_text: &'static str,
    #[allow(clippy::type_complexity)]
    action: Rc<dyn Fn(&mut SearchBuffer, &[u8], usize) -> (usize, Vec<usize>)>,
}

impl SearchBuffer {
    fn new(
        name: &'static str,
        bottom_text: &'static str,
        action: impl Fn(&mut SearchBuffer, &[u8], usize) -> (usize, Vec<usize>) + 'static,
    ) -> SearchBuffer {
        SearchBuffer {
            buffer: Vec::new(),
            cursor_position: 0,
            name,
            placeholder_text: bottom_text,
            action: Rc::new(action),
        }
    }
}

impl App {
    fn current_buffer(&self) -> Option<&SearchBuffer> {
        match self.current_search_buffer {
            None => None,
            Some(SearchBufferType::HexSearch) => Some(&self.hex_search_buffer),
            Some(SearchBufferType::AsciiSearch) => Some(&self.ascii_search_buffer),
            Some(SearchBufferType::Goto) => Some(&self.goto_buffer),
        }
    }

    fn current_buffer_mut(&mut self) -> Option<&mut SearchBuffer> {
        match self.current_search_buffer {
            None => None,
            Some(SearchBufferType::HexSearch) => Some(&mut self.hex_search_buffer),
            Some(SearchBufferType::AsciiSearch) => Some(&mut self.ascii_search_buffer),
            Some(SearchBufferType::Goto) => Some(&mut self.goto_buffer),
        }
    }
}

#[derive(Clone, Copy)]
enum Alignment {
    B1,
    B2,
    B4,
    B8,
    B16,
    Natural,
}

impl Alignment {
    fn bytes(self, cursor_size: usize) -> usize {
        match self {
            Alignment::B1 => 1,
            Alignment::B2 => 2,
            Alignment::B4 => 4,
            Alignment::B8 => 8,
            Alignment::B16 => 16,
            Alignment::Natural => cursor_size,
        }
    }

    fn next(self) -> Self {
        use Alignment::*;
        match self {
            B1 => B2,
            B2 => B4,
            B4 => B8,
            B8 => B16,
            B16 => Natural,
            Natural => B1,
        }
    }
}

impl std::fmt::Display for Alignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:>3}",
            match self {
                Alignment::B1 => "1",
                Alignment::B2 => "2",
                Alignment::B4 => "4",
                Alignment::B8 => "8",
                Alignment::B16 => "16",
                Alignment::Natural => "NAT",
            }
        )
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
        cursor: (0, 1),
        occurrence: 0,
        search_str_len: 0,
        positions: Vec::new(),
        bytes_per_group: args.bytes,
        groups: args.groups,
        buffer: Rc::new(buffer),
        current_search_buffer: None,
        search_aligned: false,
        path: args.file,
        help_popup: false,
        show_inspector: false,
        hex_area: Rect::new(0, 0, 0, 0),
        ascii_area: Rect::new(0, 0, 0, 0),
        alignment: Alignment::Natural,
        hex_search_buffer: SearchBuffer::new(
            "HEX SEARCH",
            "Enter a byte pattern to search for",
            |buf, haystack, alignment| {
                if !buf
                    .buffer
                    .iter()
                    .all(|c| c.is_ascii_hexdigit() || *c == ' ')
                {
                    return (0, Vec::new());
                }

                let chars: String = buf.buffer.iter().filter(|x| **x != ' ').collect();

                if chars.len() % 2 > 0 {
                    return (0, Vec::new());
                }

                let mut bytes: Vec<u8> = Vec::new();
                for i in (0..chars.len()).step_by(2) {
                    bytes.push(u8::from_str_radix(&chars[i..(i + 2)], 16).unwrap())
                }

                let vec = if alignment == 1 {
                    memchr::memmem::find_iter(haystack, &bytes).collect()
                } else {
                    let mut vec = Vec::new();
                    for i in (0..haystack.len()).step_by(alignment) {
                        if haystack[i..].starts_with(&bytes) {
                            vec.push(i);
                        }
                    }
                    vec
                };
                (bytes.len(), vec)
            },
        ),
        ascii_search_buffer: SearchBuffer::new(
            "ASCII SEARCH",
            "Enter a text string to search for",
            |buf, haystack, alignment| {
                let bytes = buf.buffer.iter().collect::<String>().into_bytes();
                let vec = if alignment == 1 {
                    memchr::memmem::find_iter(haystack, &bytes).collect()
                } else {
                    let mut vec = Vec::new();
                    for i in (0..haystack.len()).step_by(alignment) {
                        if haystack[i..].starts_with(&bytes) {
                            vec.push(i);
                        }
                    }
                    vec
                };
                (bytes.len(), vec)
            },
        ),
        goto_buffer: SearchBuffer::new(
            "GOTO",
            "Go to an offset, or a percentage with '%'.",
            |buf, haystack, _alignment| {
                let s = buf.buffer.iter().collect::<String>();

                if let Some(rest) = s.strip_suffix('%') {
                    let Ok(x) = rest.parse::<usize>() else {
                        return (0, Vec::new());
                    };
                    if x > 100 {
                        return (0, Vec::new());
                    }
                    return (1, vec![x * (haystack.len() - 1) / 100]);
                }

                let Ok(x) = usize::from_str_radix(&s, 16) else {
                    return (0, vec![]);
                };

                if x < haystack.len() {
                    (1, vec![x])
                } else {
                    (0, Vec::new())
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

        // We wait for one event and then quickly handle all others if there
        // are any to make the application more responsive.
        let e = event::read()?;
        if let ControlFlow::Break(()) = handle_event(&mut app, e) {
            return Ok(());
        }
        while event::poll(Duration::ZERO)? {
            let e = event::read()?;
            if let ControlFlow::Break(()) = handle_event(&mut app, e) {
                return Ok(());
            }
        }
    }
}

fn handle_event(app: &mut App, event: Event) -> ControlFlow<(), ()> {
    let haystack = app.buffer.clone();
    let alignment_bytes = app.alignment.bytes(app.cursor.1);
    let search_alignment_bytes = if app.search_aligned {
        alignment_bytes
    } else {
        1
    };

    // We always allow quitting with ctrl+c
    if let Event::Key(key) = event {
        if key.modifiers == KeyModifiers::CONTROL && key.code == KeyCode::Char('c') {
            return ControlFlow::Break(());
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
        return ControlFlow::Continue(());
    }

    // Now we do our normal event handling
    if let Event::Key(key) = event {
        if key.code == KeyCode::Up {
            if key.modifiers == KeyModifiers::CONTROL {
                app.scroll(-1);
            } else {
                app.move_cursor(-(app.bytes_per_line() as isize));
            }
        }

        if key.code == KeyCode::Down {
            if key.modifiers == KeyModifiers::CONTROL {
                app.scroll(1);
            } else {
                app.move_cursor(app.bytes_per_line() as isize);
            }
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
                if !key.modifiers.contains(KeyModifiers::CONTROL) {
                    buf.buffer.insert(buf.cursor_position, c);
                    buf.cursor_position += 1;
                }
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
                let action = buf.action.clone();
                (app.search_str_len, app.positions) = (action)(buf, &haystack, search_alignment_bytes);

                if !app.positions.is_empty() {
                    let i = match app.positions.binary_search(&app.cursor.0) {
                        Ok(i) | Err(i) => i,
                    };
                    let i = i % app.positions.len();
                    app.occurrence = i + 1;
                    app.set_cursor(app.positions[i], app.search_str_len)
                }
                app.current_search_buffer = None;
            }

            if key.code == KeyCode::Char('a') && key.modifiers == KeyModifiers::CONTROL {
                app.search_aligned = !app.search_aligned;
            }
        }

        // Vim-like controls when not typing in a buffer
        if app.current_search_buffer.is_none() {
            let bytes = app.alignment.bytes(app.cursor.1) as isize;
            if key.code == KeyCode::Right {
                if key.modifiers == KeyModifiers::SHIFT {
                    app.cursor.1 += 1;
                } else if key.modifiers == KeyModifiers::CONTROL {
                    app.move_cursor(bytes);
                } else {
                    app.move_cursor(1);
                }
            }
            if key.code == KeyCode::Left {
                if key.modifiers == KeyModifiers::SHIFT {
                    app.cursor.1 = app.cursor.1.saturating_sub(1).max(1);
                } else if key.modifiers == KeyModifiers::CONTROL {
                    app.move_cursor(-bytes);
                } else {
                    app.move_cursor(-1);
                }
            }
            if key.code == KeyCode::Char('q') {
                return ControlFlow::Break(());
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
                app.current_search_buffer = Some(SearchBufferType::Goto);
            }
            if key.code == KeyCode::Char('s') {
                app.current_search_buffer = Some(SearchBufferType::AsciiSearch);
            }
            if key.code == KeyCode::Char('/') {
                app.current_search_buffer = Some(SearchBufferType::HexSearch);
            }
            if key.code == KeyCode::Char('i') {
                app.show_inspector = !app.show_inspector;
            }
            if key.code == KeyCode::Char('?') {
                app.help_popup = true;
            }
            if key.code == KeyCode::Char(';') {
                app.cursor.1 = 1;
            }
            if let KeyCode::Char('n' | 'N') = key.code {
                if !app.positions.is_empty() {
                    let res = app.positions.binary_search(&app.cursor.0);
                    let i = if key.modifiers == KeyModifiers::SHIFT {
                        match res {
                            Ok(i) | Err(i) => i + app.positions.len() - 1,
                        }
                    } else {
                        match res {
                            Ok(i) => i,
                            Err(i) => i + 1,
                        }
                    };
                    let i = i % app.positions.len();
                    app.occurrence = i + 1;
                    app.set_cursor(app.positions[i], app.search_str_len);
                }
            }
            if key.code == KeyCode::Char('a') && key.modifiers == KeyModifiers::CONTROL {
                app.alignment = app.alignment.next();
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
            app.set_cursor(app.view + y as usize * app.bytes_per_line() + x as usize, 1);
        }
        if app.ascii_area.contains(Position::new(column, row)) {
            let x = column - app.ascii_area.x;
            let y = row - app.ascii_area.y;
            app.set_cursor(app.view + y as usize * app.bytes_per_line() + x as usize, 1);
        }
    }

    ControlFlow::Continue(())
}

fn ui(app: &mut App, frame: &mut Frame) {
    let size = match app.buffer.len() {
        s if s < 1024 => format!("{s} B"),
        s if s < 1024usize.pow(2) => format!("{:.1} KB", s as f64 / 1024.0),
        s if s < 1024usize.pow(3) => format!("{:.1} MB", s as f64 / 1024.0f64.powf(2.0)),
        s => format!("{:.1} GB", s as f64 / 1024.0f64.powf(3.0)),
    };
    let offset = app.cursor.0;
    let percentage = (offset as f64 * 100.0) / app.buffer.len() as f64;
    let main_block = Block::new().title_bottom(format!(
        "Press `?` for help. Viewing: {}, file size: {size} ({offset:0>8x}, {percentage:.2}%), Occurrence: {}/{}",
        app.path.display(),
        app.occurrence,
        app.positions.len(),
    ));

    let spacing = app.spacing as usize;
    let hex_width = 2 // borders
        + 8 // offset
        + 2 * spacing // padding of block
        + 2 * spacing * app.groups // padding of columns 
        + (3 * app.bytes_per_group * app.groups); // bytes

    let ascii_width = 2 + (app.bytes_per_group * app.groups) + if app.spacing { 2 } else { 0 };
    let width = hex_width + ascii_width + 1; // scrollbar

    let frame_size = frame.size();

    frame.render_widget(&main_block, frame_size);
    let mut main = main_block.inner(frame_size);
    main.width = main.width.min(width as u16);

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
        let aligned = "Ctrl+A: Search Aligned";
        let popup_block = Block::bordered()
            .title(buf.name)
            .title_bottom(if app.search_aligned {
                aligned.bg(Color::Green).fg(Color::Black)
            } else {
                aligned.into()
            })
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

        let offset = app.cursor.0;
        let x = app.buffer[offset];
        let x8 = u8::from_le_bytes(chunk(&app.buffer, offset));
        let x16 = u16::from_le_bytes(chunk(&app.buffer, offset));
        let x32 = u32::from_le_bytes(chunk(&app.buffer, offset));
        let x64 = u64::from_le_bytes(chunk(&app.buffer, offset));

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
        .title_bottom(format!("Align: {}", app.alignment).fg(Color::White))
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
        .position(offset)
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
        let area = centered_rect(60, 30, frame_size);
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
            (vec!["ENTER"], "Confirm search"),
            (vec!["N"], "Next search occurrence"),
            (vec!["SHIFT+N"], "Previous search occurrence"),
            (vec!["I"], "Toggle inspector"),
            (vec!["CTRL+A"], "Change alignment"),
            (vec!["CTRL+LEFT"], "Move cursor left by alignment"),
            (vec!["CTRL+RIGHT"], "Move cursor right by alignment"),
            (vec!["CTRL+UP", "MOUSE UP"], "Scroll up"),
            (vec!["CTRL+DOWN", "MOUSE DOWN"], "Scroll down"),
            (vec!["SHIFT+RIGHT"], "Extend cursor"),
            (vec!["SHIFT+LEFT"], "Shrink cursor"),
            (vec![";"], "Collapse cursor to 1 byte"),
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
            vec![Length(22), Min(0)],
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

fn style_for_byte(app: &App, offset: usize, byte: u8) -> Style {
    let mut style = Style::default();

    if (app.cursor.0..(app.cursor.0 + app.cursor.1)).contains(&offset) {
        style = style.black().on_white().bold().underlined();
    } else if !app.positions.is_empty() {
        let i = match app.positions.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => (i + app.positions.len() - 1) % app.positions.len(),
        };
        let pos = app.positions[i];
        if (pos..(pos + app.search_str_len)).contains(&offset) {
            style = style.white().on_blue();
        }
    }
    if app.color && style.fg.is_none() {
        style = style.fg(byte_color(byte));
    }
    style
}

fn write_byte(app: &App, offset: usize, byte: u8) -> Span<'static> {
    let s = format!("{:0>2x}", byte);
    let style = style_for_byte(app, offset, byte);
    Span::styled(s, style)
}

fn render_ascii_char(app: &App, offset: usize, byte: u8) -> Span<'static> {
    let c = match byte {
        0..=31 => '.',
        byte @ (b' '..=b'~') => byte as char,
        127 => '.',
        128..=u8::MAX => '.',
    };
    let style = style_for_byte(app, offset, byte);
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
