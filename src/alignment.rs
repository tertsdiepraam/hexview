#[derive(Clone, Copy, Default)]
pub enum Alignment {
    B1,
    B2,
    B4,
    B8,
    B16,
    #[default]
    Natural,
}

impl Alignment {
    pub fn bytes(self, cursor_size: usize) -> usize {
        match self {
            Alignment::B1 => 1,
            Alignment::B2 => 2,
            Alignment::B4 => 4,
            Alignment::B8 => 8,
            Alignment::B16 => 16,
            Alignment::Natural => cursor_size,
        }
    }

    pub fn next(self) -> Self {
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
