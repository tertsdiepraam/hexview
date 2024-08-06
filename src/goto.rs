use ratatui::{
    buffer::Buffer,
    crossterm::event::{Event, KeyCode},
    layout::Rect,
    style::{Color, Style},
    widgets::{Block, Widget},
};

use crate::{line_editor::LineEditor, AppState};

#[derive(Default)]
pub struct Goto {
    pub visible: bool,
    line_editor: LineEditor<'static>,
}

impl Goto {
    pub fn input(&mut self, state: &mut AppState, event: Event) {
        if let Event::Key(key_event) = event {
            match key_event.code {
                KeyCode::Enter => self.goto(state),
                _ => {
                    self.line_editor.input(event);
                }
            }
        }
    }

    pub fn height(&self) -> u16 {
        3 * self.visible as u16
    }

    fn goto(&self, state: &mut AppState) {
        let line = self.line_editor.line();

        if line.is_empty() {
            return;
        }

        if let Some(rest) = line.strip_suffix('%') {
            let Ok(x) = rest.parse::<usize>() else {
                return;
            };
            if x > 100 {
                return;
            }
            state.set_cursor(x * (state.buffer.len() - 1) / 100, state.cursor.length);
        }

        let Ok(x) = usize::from_str_radix(&line, 16) else {
            return;
        };

        if x < state.buffer.len() {
            state.set_cursor(x, state.cursor.length);
        }
    }
}

impl<'a> Widget for &'a Goto {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let block = Block::bordered()
            .title(" Go to ")
            .border_style(Style::default().fg(Color::White));

        block.clone().render(area, buf);

        let area = block.inner(area);
        self.line_editor.widget().render(area, buf);
    }
}
