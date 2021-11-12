#[derive(Debug)]
pub enum ToysError {
    ParseError(String),
}

impl ToysError {
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(self) -> String {
        match self {
            Self::ParseError(message) => message,
        }
    }
}
