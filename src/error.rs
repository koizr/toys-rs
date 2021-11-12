#[derive(Debug)]
pub enum ToysError {
    ParseError(String),
}

impl ToysError {
    pub fn to_string(self) -> String {
        match self {
            Self::ParseError(message) => message,
        }
    }
}
