use thiserror::Error;

pub type Result<T> = ::std::result::Result<T, MipsError>;

#[derive(Error, Debug)]
pub enum MipsError {
    #[error("Invalid state error: {0}")]
    InvalidState(String),
    #[error("Bad conversion")]
    BadConversion(),
    #[error("File not found: {0}")]
    FileNotFound(String),
    #[error("Bios' hash not known")]
    UnknownBios,
}