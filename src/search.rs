use ratatui::{
    buffer::Buffer,
    crossterm::event::{Event, KeyCode, KeyModifiers},
    layout::Rect,
    style::{Color, Style},
    widgets::{block::Title, Block, Widget},
};

use crate::{
    data_type::{DataType, Endianness, Repr},
    line_editor::LineEditor,
    AppState,
};

#[derive(Default)]
pub struct Search {
    pub visible: bool,
    data_type: Repr,
    line_editor: LineEditor<'static>,
    search_aligned: bool,
}

impl Search {
    pub fn input(&mut self, state: &mut AppState, event: Event) {
        if let Event::Key(key_event) = event {
            let ctrl = key_event.modifiers == KeyModifiers::CONTROL;
            match key_event.code {
                KeyCode::Enter => self.search(state),
                KeyCode::Char('q') if ctrl => {
                    self.search_aligned = !self.search_aligned;
                }
                KeyCode::Tab => {
                    self.data_type = match self.data_type {
                        Repr::Hex => Repr::Oct,
                        Repr::Oct => Repr::Dec,
                        Repr::Dec => Repr::Utf8,
                        Repr::Utf8 => Repr::Utf16,
                        Repr::Utf16 => Repr::Hex,
                    }
                }
                _ => {
                    self.line_editor.input(event);
                }
            }
        }
    }

    pub fn height(&self) -> u16 {
        3 * self.visible as u16
    }

    fn search(&mut self, state: &mut AppState) {
        let Some(bytes) = self.byte_string(state.endianness) else {
            return;
        };

        // Don't search again if we already have the results
        if bytes == state.searched {
            state.goto_occurrence(false);
            return;
        }

        let alignment_bytes = if self.search_aligned {
            state.alignment.bytes(state.cursor.length.get())
        } else {
            1
        };

        let vec = if alignment_bytes == 1 {
            memchr::memmem::find_iter(state.buffer, &bytes).collect()
        } else {
            let mut vec = Vec::new();
            for i in (0..state.buffer.len()).step_by(alignment_bytes) {
                if state.buffer[i..].starts_with(&bytes) {
                    vec.push(i);
                }
            }
            vec
        };

        state.searched = bytes;
        state.occurrence_positions = vec;
        state.goto_occurrence(false);
    }

    fn byte_string(&self, endianness: Endianness) -> Option<Vec<u8>> {
        let line = self.line_editor.line();

        let bytes = match self.data_type {
            Repr::Hex => bytes_with_radix(&line, 16)?,
            Repr::Oct => bytes_with_radix(&line, 8)?,
            Repr::Dec => bytes_with_radix(&line, 10)?,
            Repr::Utf8 => line.into_bytes(),
            Repr::Utf16 => line
                .encode_utf16()
                .flat_map(|wide| wide.to_bytes(endianness))
                .collect(),
        };

        // We won't want to search for empty strings
        if !bytes.is_empty() {
            Some(bytes)
        } else {
            None
        }
    }
}

fn bytes_with_radix(line: &str, radix: u32) -> Option<Vec<u8>> {
    let mut bytes = Vec::new();
    for x in line.split(' ') {
        bytes.push(u8::from_str_radix(x, radix).ok()?)
    }
    Some(bytes)
}

impl<'a> Widget for &'a Search {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let block = Block::bordered()
            .title(" Search ")
            .title(
                Title::from(format!(" Type: {:<6} [TAB] ", self.data_type))
                    .alignment(ratatui::layout::Alignment::Right),
            )
            .border_style(Style::default().fg(if self.visible {
                Color::White
            } else {
                Color::DarkGray
            }));
        block.clone().render(area, buf);

        let area = block.inner(area);
        self.line_editor.widget().render(area, buf);
    }
}
