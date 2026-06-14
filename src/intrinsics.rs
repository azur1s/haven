#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Intrinsic {
    /// `len(slice) -> usize`
    Len,
    /// Direct bitwise casting, will never panic but will wrap around silently
    /// `bitcast(0, iN, uN, fN) -> iN/uN/fN`
    Bitcast,
}

impl Intrinsic {
    pub fn lookup(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Self::Len),
            "bitcast" => Some(Self::Bitcast),
            _ => None,
        }
    }
}