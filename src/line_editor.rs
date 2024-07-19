use ratatui::widgets::Widget;
use tui_textarea::{Input, Key, TextArea};

/// Wrapper around a [TextArea] that restricts editing to a single line
#[derive(Default)]
pub struct LineEditor<'a>(TextArea<'a>);

impl<'a> LineEditor<'a> {
    pub fn widget(&'a self) -> impl Widget + 'a {
        self.0.widget()
    }

    pub fn input(&mut self, input: impl Into<Input>) -> bool {
        match input.into() {
            Input {
                key: Key::Char('m'),
                ctrl: true,
                ..
            }
            | Input {
                key: Key::Enter, ..
            } => false,
            input => {
                // TextArea::input returns if the input modified its text
                self.0.input(input)
            }
        }
    }

    pub fn line(&self) -> String {
        self.0.lines()[0].clone()
    }
}
