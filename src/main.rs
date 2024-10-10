use std::{
    error::Error,
    io::{self, Read},
    num::NonZeroUsize,
    ops::ControlFlow,
    path::PathBuf,
    time::Duration,
};

use alignment::Alignment;
use clap::Parser;
use data_type::Endianness;
use goto::Goto;
use ratatui::{
    backend::{Backend, CrosstermBackend},
    crossterm::{
        event::{
            self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent, KeyEventKind,
            KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
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
use search::Search;

mod alignment;
mod data_type;
mod goto;
mod line_editor;
mod search;

#[derive(Parser)]
#[command(version, author, about, long_about = None)]
pub struct Args {
    /// The file to view
    file: PathBuf,
    #[arg(short, long, default_value_t = 8)]
    /// The number of bytes per group
    bytes: usize,
    #[arg(short, long, default_value_t = 2)]
    /// The number of groups (columns) to show
    groups: usize,
    /// Disable colored output
    #[arg(short, long)]
    no_color: bool,
    /// Add a bit of additional padding
    #[arg(short, long, default_value_t = false)]
    spacing: bool,
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
enum Format {
    #[default]
    LowerHex,
    UpperHex,
    Octal,
    Binary,
}

impl Format {
    fn next(self) -> Self {
        use Format::*;
        match self {
            LowerHex => UpperHex,
            UpperHex => Octal,
            Octal => Binary,
            Binary => LowerHex,
        }
    }

    fn width(self) -> usize {
        match self {
            Format::LowerHex | Format::UpperHex => 2,
            Format::Octal => 3,
            Format::Binary => 8,
        }
    }

    fn format_byte(self, byte: u8) -> String {
        match self {
            Format::LowerHex => format!("{byte:0>2x}"),
            Format::UpperHex => format!("{byte:0>2X}"),
            Format::Octal => format!("{byte:0>3o}"),
            Format::Binary => format!("{byte:0>8b}"),
        }
    }
}

#[derive(Default)]
pub struct AppState<'a> {
    /// Offset of the byte in the top-left corner
    view: usize,
    /// Whether to render with some additional padding
    spacing: bool,
    /// Whether to enable colors
    color: bool,
    /// Position and length of the cursor
    cursor: Cursor,
    /// Bytes of the file
    buffer: &'a [u8],
    /// Path of the file we're looking at
    path: PathBuf,
    /// Search string
    searched: Vec<u8>,
    /// Last occurrence we jumped to
    occurrence: usize,
    /// All occurrences of the searched string
    occurrence_positions: Vec<usize>,
    /// The alignment settings
    alignment: Alignment,
    /// Whether the help popup is shown
    help_popup: bool,
    /// Whether the inspector is shown
    show_inspector: bool,
    /// How many bytes are displayed per group
    bytes_per_group: usize,
    /// How many groups are displayed
    groups: usize,
    /// The endianness of the data for searching
    endianness: Endianness,
    /// Area where the hex view was last drawn
    hex_area: Rect,
    /// Area where the ASCII view was last drawn
    ascii_area: Rect,
    /// Format to view
    format: Format,
}

#[derive(Default)]
struct App<'a> {
    state: AppState<'a>,
    search: Search,
    goto: Goto,
}

struct Cursor {
    position: usize,
    length: NonZeroUsize,
}

impl Default for Cursor {
    fn default() -> Self {
        Self {
            position: 0,
            length: 1.try_into().unwrap(),
        }
    }
}

impl Cursor {
    fn range(&self) -> std::ops::Range<usize> {
        let start = self.position;
        let end = self.position + self.length.get();
        start..end
    }

    fn contains(&self, offset: usize) -> bool {
        self.range().contains(&offset)
    }
}

impl<'a> App<'a> {
    fn new(buffer: &'a [u8], args: Args) -> Self {
        let Args {
            file: path,
            bytes: bytes_per_group,
            groups,
            no_color,
            spacing,
        } = args;
        App {
            state: AppState {
                path,
                buffer,
                spacing,
                color: !no_color,
                bytes_per_group,
                groups,
                ..Default::default()
            },
            ..Default::default()
        }
    }
}

impl AppState<'_> {
    fn move_cursor(&mut self, delta: isize) {
        let offset = self.cursor.position.saturating_add_signed(delta);
        self.set_cursor(offset, self.cursor.length)
    }

    fn extend_cursor(&mut self, delta: isize) {
        let length = self.cursor.length.get().saturating_add_signed(delta);
        self.cursor.length = NonZeroUsize::new(length).unwrap_or(NonZeroUsize::MIN);
    }

    fn set_cursor(&mut self, offset: usize, length: NonZeroUsize) {
        let position = offset.min(self.buffer.len() - 1);
        self.cursor = Cursor { position, length };
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
        let offset = offset.min(self.buffer.len());
        self.view = self.line_offset(offset);
    }

    fn scroll_to_cursor(&mut self) {
        let start = self.cursor.position;
        let end = self.cursor.length.get().saturating_add(start);
        if self.cursor.position < self.view {
            self.view = self.line_offset(start);
        } else if end >= self.view + (self.bytes_per_line() * self.lines_displayed()) {
            self.view =
                self.line_offset(end) - self.bytes_per_line() * (self.lines_displayed() - 1);
        }
    }

    fn goto_occurrence(&mut self, previous: bool) {
        if !self.occurrence_positions.is_empty() {
            let res = self
                .occurrence_positions
                .binary_search(&self.cursor.position);
            let i = if previous {
                match res {
                    Ok(i) | Err(i) => i + self.occurrence_positions.len() - 1,
                }
            } else {
                match res {
                    Ok(i) => i + 1,
                    Err(i) => i,
                }
            };
            let i = i % self.occurrence_positions.len();
            self.occurrence = i + 1;
            self.set_cursor(
                self.occurrence_positions[i],
                NonZeroUsize::try_from(self.searched.len()).unwrap(),
            );
        }
    }

    fn line_offset(&self, offset: usize) -> usize {
        offset - (offset % self.bytes_per_line())
    }

    fn lines_displayed(&self) -> usize {
        self.hex_area.height as usize
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

    let app = App::new(&buffer, args);

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
    let alignment_bytes = app.state.alignment.bytes(app.state.cursor.length.get());

    if let Event::Key(KeyEvent {
        kind: KeyEventKind::Release,
        ..
    }) = event
    {
        return ControlFlow::Continue(());
    }

    // We always allow quitting with ctrl+c
    if let Event::Key(KeyEvent {
        code: KeyCode::Char('c'),
        modifiers: KeyModifiers::CONTROL,
        ..
    }) = event
    {
        return ControlFlow::Break(());
    }

    // The help popup takes precedence over everything else
    if app.state.help_popup {
        if let Event::Key(KeyEvent {
            code: KeyCode::Char('?' | 'q') | KeyCode::Esc | KeyCode::Enter,
            ..
        }) = event
        {
            app.state.help_popup = false;
        }
        // We ignore everything until the popup is gone
        return ControlFlow::Continue(());
    }

    if let Event::Mouse(MouseEvent {
        kind,
        column,
        row,
        modifiers: _,
    }) = event
    {
        match kind {
            MouseEventKind::ScrollDown => app.state.scroll(1),
            MouseEventKind::ScrollUp => app.state.scroll(-1),
            MouseEventKind::Down(MouseButton::Left) => {
                if app.state.hex_area.contains(Position::new(column, row)) {
                    let x = (column - app.state.hex_area.x - 8) / 3;
                    let y = row - app.state.hex_area.y;
                    app.state.set_cursor(
                        app.state.view + y as usize * app.state.bytes_per_line() + x as usize,
                        NonZeroUsize::new(1).unwrap(),
                    );
                }
                if app.state.ascii_area.contains(Position::new(column, row)) {
                    let x = column - app.state.ascii_area.x;
                    let y = row - app.state.ascii_area.y;
                    app.state.set_cursor(
                        app.state.view + y as usize * app.state.bytes_per_line() + x as usize,
                        NonZeroUsize::new(1).unwrap(),
                    );
                }
            }
            _ => {}
        }
    }

    // Now we do our normal event handling
    if let Event::Key(key) = event {
        let ctrl = key.modifiers == KeyModifiers::CONTROL;

        match key.code {
            KeyCode::Up if ctrl => {
                app.state.scroll(-1);
            }
            KeyCode::Up => {
                app.state
                    .move_cursor(-(app.state.bytes_per_line() as isize));
            }
            KeyCode::Down if ctrl => {
                app.state.scroll(1);
            }
            KeyCode::Down => {
                app.state.move_cursor(app.state.bytes_per_line() as isize);
            }
            KeyCode::PageDown => {
                app.state
                    .scroll_and_move_cursor(app.state.lines_displayed() as isize);
            }
            KeyCode::PageUp => {
                app.state
                    .scroll_and_move_cursor(-(app.state.lines_displayed() as isize));
            }
            KeyCode::Char('d') if ctrl => {
                app.state
                    .scroll_and_move_cursor(app.state.lines_displayed() as isize);
            }
            KeyCode::Char('u') if ctrl => {
                app.state
                    .scroll_and_move_cursor(-(app.state.lines_displayed() as isize));
            }
            KeyCode::Char('s') if ctrl => app.state.endianness = app.state.endianness.flip(),
            KeyCode::Esc => {
                app.search.visible = false;
                app.goto.visible = false;
            }
            _ => {}
        }
    }

    if app.search.visible {
        app.search.input(&mut app.state, event);
        return ControlFlow::Continue(());
    }

    if app.goto.visible {
        app.goto.input(&mut app.state, event);
        return ControlFlow::Continue(());
    }

    // Vim-like controls when not typing in a buffer
    if let Event::Key(key) = event {
        let shift = key.modifiers == KeyModifiers::SHIFT;
        let ctrl = key.modifiers == KeyModifiers::CONTROL;

        match key.code {
            KeyCode::Right if shift => {
                app.state.extend_cursor(1);
            }
            KeyCode::Right if ctrl => {
                app.state.move_cursor(alignment_bytes as isize);
            }
            KeyCode::Right => {
                app.state.move_cursor(1);
            }
            KeyCode::Left if shift => {
                app.state.extend_cursor(-1);
            }
            KeyCode::Left if ctrl => {
                app.state.move_cursor(-(alignment_bytes as isize));
            }
            KeyCode::Left => {
                app.state.move_cursor(-1);
            }
            KeyCode::Char('q') => {
                return ControlFlow::Break(());
            }
            KeyCode::Char('j') => {
                app.state.move_cursor(app.state.bytes_per_line() as isize);
            }
            KeyCode::Char('k') => {
                app.state
                    .move_cursor(-(app.state.bytes_per_line() as isize));
            }
            KeyCode::Char('h') => {
                app.state.move_cursor(-1);
            }
            KeyCode::Char('l') => {
                app.state.move_cursor(1);
            }
            KeyCode::Char('g') => {
                app.goto.visible = true;
            }
            KeyCode::Char('f') if ctrl => {
                app.search.visible = true;
            }
            KeyCode::Char('/') => {
                app.search.visible = true;
            }
            KeyCode::Char('i') => {
                app.state.show_inspector = !app.state.show_inspector;
            }
            KeyCode::Char('?') => {
                app.state.help_popup = true;
            }
            KeyCode::Char(';') => {
                app.state.cursor.length = NonZeroUsize::new(1).unwrap();
            }
            KeyCode::Char('n' | 'N') | KeyCode::Enter => app.state.goto_occurrence(shift),
            KeyCode::Char('a') if ctrl => {
                app.state.alignment = app.state.alignment.next();
            }
            KeyCode::Char('o') => app.state.format = app.state.format.next(),
            _ => {}
        }
    }

    ControlFlow::Continue(())
}

fn ui(app: &mut App, frame: &mut Frame) {
    let size = match app.state.buffer.len() {
        s if s < 1024 => format!("{s} B"),
        s if s < 1024usize.pow(2) => format!("{:.1} KB", s as f64 / 1024.0),
        s if s < 1024usize.pow(3) => format!("{:.1} MB", s as f64 / 1024.0f64.powf(2.0)),
        s => format!("{:.1} GB", s as f64 / 1024.0f64.powf(3.0)),
    };
    let offset = app.state.cursor.position;
    let percentage = (offset as f64 * 100.0) / app.state.buffer.len() as f64;
    let main_block = Block::new().title_bottom(format!(
        "Press `?` for help. Viewing: {}, file size: {size} ({offset:0>8x}, {percentage:.2}%), Occurrence: {}/{}",
        app.state.path.display(),
        app.state.occurrence,
        app.state.occurrence_positions.len(),
    ));

    let spacing = app.state.spacing as usize;
    let hex_width = 2 // borders
        + 8 // offset
        + 2 * spacing // padding of block
        + 2 * spacing * app.state.groups // padding of columns 
        + ((1 + app.state.format.width()) * app.state.bytes_per_group * app.state.groups); // bytes

    let ascii_width =
        2 + (app.state.bytes_per_group * app.state.groups) + if app.state.spacing { 2 } else { 0 };
    let width = hex_width + ascii_width + 1; // scrollbar

    let frame_size = frame.size();

    frame.render_widget(&main_block, frame_size);
    let mut main = main_block.inner(frame_size);
    main.width = main.width.min(width as u16);

    let [top_area, search_area, goto_area, inspector_area] = Layout::vertical([
        Min(0),
        Length(app.search.height()),
        Length(app.goto.height()),
        Length(if app.state.show_inspector { 7 } else { 0 }),
    ])
    .areas(main);

    if app.search.visible {
        frame.render_widget(&app.search, search_area)
    }

    if app.goto.visible {
        frame.render_widget(&app.goto, goto_area)
    }

    if app.state.show_inspector {
        let inspector_block = Block::bordered().title("Inspector (press `I` to hide)");

        let offset = app.state.cursor.position;
        let x = app.state.buffer[offset];
        let (x8, x16, x32, x64) = if app.state.endianness == Endianness::Little {
            (
                u8::from_le_bytes(chunk(app.state.buffer, offset)),
                u16::from_le_bytes(chunk(app.state.buffer, offset)),
                u32::from_le_bytes(chunk(app.state.buffer, offset)),
                u64::from_le_bytes(chunk(app.state.buffer, offset)),
            )
        } else {
            (
                u8::from_be_bytes(chunk(app.state.buffer, offset)),
                u16::from_be_bytes(chunk(app.state.buffer, offset)),
                u32::from_be_bytes(chunk(app.state.buffer, offset)),
                u64::from_be_bytes(chunk(app.state.buffer, offset)),
            )
        };

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
            Line::from(format!("f32: {:e}", f32::from_bits(x32),)),
            Line::from(format!("f64: {:e}", f64::from_bits(x64),)),
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
        .padding(Padding::uniform(if app.state.spacing { 1 } else { 0 }))
        .borders(Borders::ALL)
        .title_bottom(format!(" Align: {} ", app.state.alignment).fg(Color::White))
        .title_bottom(format!(" {}-endian ", app.state.endianness).fg(Color::White))
        .border_style(Style::default().fg(Color::DarkGray));

    frame.render_widget(&block, hex_area);
    let hex_area = block.inner(hex_area);

    let block = Block::new()
        .title("ASCII")
        .padding(Padding::uniform(if app.state.spacing { 1 } else { 0 }))
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::DarkGray));

    frame.render_widget(&block, ascii_area);
    let ascii_area = block.inner(ascii_area);

    // Store the rects to properly handle mouse input
    app.state.hex_area = hex_area;
    app.state.ascii_area = ascii_area;

    let lines_to_draw = hex_area.height as usize;

    let mut scrollbar_state = ScrollbarState::new(app.state.buffer.len())
        .position(offset)
        .viewport_content_length(lines_to_draw);
    frame.render_stateful_widget(
        Scrollbar::new(ScrollbarOrientation::VerticalRight).track_symbol(Some("│")),
        scrollbar_area,
        &mut scrollbar_state,
    );

    let mut lines = Vec::new();
    'outer: for i in 0..lines_to_draw {
        let line_offset = app.state.view + i * app.state.bytes_per_line();

        // Write the offset at the start of the line
        let mut spans = Vec::new();
        let offset_string = format!("{:x}", line_offset);
        spans.push(Span::styled(
            "0".repeat(8 - offset_string.len()),
            Style::default().fg(Color::DarkGray),
        ));
        spans.push(Span::raw(offset_string));

        for byte_offset in line_offset..(line_offset + app.state.bytes_per_line()) {
            if byte_offset % app.state.bytes_per_group == 0 {
                if app.state.spacing {
                    spans.push(" │ ".fg(Color::DarkGray));
                } else {
                    spans.push("│".fg(Color::DarkGray));
                }
            } else {
                spans.push(Span::raw(" "));
            }
            match app.state.buffer.get(byte_offset) {
                Some(byte) => spans.push(write_byte(&app.state, byte_offset, *byte)),
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
        let line_offset = app.state.view + i * app.state.bytes_per_line();

        let mut spans = Vec::new();
        for byte_offset in line_offset..(line_offset + app.state.bytes_per_line()) {
            match app.state.buffer.get(byte_offset) {
                Some(byte) => spans.push(render_ascii_char(&app.state, byte_offset, *byte)),
                None => {
                    lines.push(Line::from(spans));
                    break 'outer;
                }
            };
        }

        lines.push(Line::from(spans))
    }

    frame.render_widget(Paragraph::new(Text::from(lines)), ascii_area);

    if app.state.help_popup {
        let help_block = Block::bordered().title(" Help (press `q` or `?` to dismiss) ");
        let area = centered_rect(70, 40, frame_size);
        frame.render_widget(Clear, area);
        let area = area.inner(Margin::new(1, 1));
        frame.render_widget(&help_block, area);

        let keys = vec![
            (vec![], "General"),
            (vec!["?"], "Toggle help"),
            (vec!["G"], "Goto offset"),
            (vec!["CTRL+F", "/"], "Search string"),
            (vec!["I"], "Toggle inspector"),
            (vec!["O"], "Change format"),
            (vec!["Q", "CTRL+C"], "Quit"),
            (vec![], ""),
            (vec![], "Movement"),
            (vec!["UP", "K"], "Move cursor up"),
            (vec!["DOWN", "J"], "Move cursor down"),
            (vec!["LEFT", "H"], "Move cursor left"),
            (vec!["RIGHT", "L"], "Move cursor right"),
            (vec!["PAGE UP", "CTRL+U"], "Page up"),
            (vec!["PAGE DOWN", "CTRL+D"], "Page down"),
            (vec!["CTRL+A"], "Change alignment"),
            (vec!["CTRL+LEFT"], "Move cursor left by alignment"),
            (vec!["CTRL+RIGHT"], "Move cursor right by alignment"),
            (vec!["CTRL+UP", "MOUSE UP"], "Scroll up"),
            (vec!["CTRL+DOWN", "MOUSE DOWN"], "Scroll down"),
            (vec!["SHIFT+RIGHT"], "Extend cursor"),
            (vec!["SHIFT+LEFT"], "Shrink cursor"),
            (vec![";"], "Collapse cursor to 1 byte"),
            (vec![], ""),
            (vec![], "Seaching"),
            (vec!["ENTER"], "Confirm search"),
            (vec!["N"], "Next search occurrence"),
            (vec!["SHIFT+N"], "Previous search occurrence"),
        ];

        let table = Table::new(
            keys.iter().map(|(keys, action)| {
                if keys.is_empty() {
                    return Row::new(vec![Line::from(*action)]);
                }
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

fn style_for_byte(state: &AppState, offset: usize, byte: u8) -> Style {
    let mut style = Style::default();

    if state.cursor.contains(offset) {
        style = style.black().on_white().bold().underlined();
    } else if !state.occurrence_positions.is_empty() {
        let i = match state.occurrence_positions.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => (i + state.occurrence_positions.len() - 1) % state.occurrence_positions.len(),
        };
        let pos = state.occurrence_positions[i];
        if (pos..(pos + state.searched.len())).contains(&offset) {
            style = style.white().on_blue();
        }
    }
    if state.color && style.fg.is_none() {
        style = style.fg(byte_color(byte));
    }
    style
}

fn write_byte(state: &AppState, offset: usize, byte: u8) -> Span<'static> {
    let s = state.format.format_byte(byte);
    let style = style_for_byte(state, offset, byte);
    Span::styled(s, style)
}

fn render_ascii_char(state: &AppState, offset: usize, byte: u8) -> Span<'static> {
    let c = match byte {
        0..=31 => '.',
        byte @ (b' '..=b'~') => byte as char,
        127 => '.',
        128..=u8::MAX => '.',
    };
    let style = style_for_byte(state, offset, byte);
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
