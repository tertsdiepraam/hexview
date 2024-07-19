use std::fmt::Display;

#[derive(Default)]
pub enum Repr {
    #[default]
    Hex,
    Oct,
    Dec,
    Utf8,
    Utf16,
}

impl Display for Repr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Repr::Hex => "Hex",
            Repr::Oct => "Oct",
            Repr::Dec => "Dec",
            Repr::Utf8 => "UTF-8",
            Repr::Utf16 => "UTF-16",
        };
        str.fmt(f)
    }
}

#[derive(Clone, Copy, Default)]
pub enum Endianness {
    #[default]
    Big,
    Little,
}

impl Display for Endianness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Endianness::Big => "big",
            Endianness::Little => "little",
        };
        str.fmt(f)
    }
}

impl Endianness {
    pub fn flip(self) -> Self {
        match self {
            Self::Big => Self::Little,
            Self::Little => Self::Big,
        }
    }
}

pub trait DataType {
    fn to_bytes(self, endianness: Endianness) -> Vec<u8>;
}

impl DataType for u8 {
    fn to_bytes(self, _endianness: Endianness) -> Vec<u8> {
        vec![self]
    }
}

impl DataType for u16 {
    fn to_bytes(self, endianness: Endianness) -> Vec<u8> {
        match endianness {
            Endianness::Big => self.to_be_bytes().into(),
            Endianness::Little => self.to_le_bytes().into(),
        }
    }
}

impl DataType for u32 {
    fn to_bytes(self, endianness: Endianness) -> Vec<u8> {
        match endianness {
            Endianness::Big => self.to_be_bytes().into(),
            Endianness::Little => self.to_le_bytes().into(),
        }
    }
}

