#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Intrinsic {
    Len,
}

impl Intrinsic {
    pub fn lookup(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Self::Len),
            _ => None,
        }
    }
}