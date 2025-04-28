use thiserror::Error;
use crate::psx::cd::disc::SerialNumber;

pub type MipsResult<T> = ::std::result::Result<T, MipsError>;

#[derive(Error, Debug)]
pub enum MipsError {
    #[error("Invalid state error: {0}")]
    InvalidState(String),
    #[error("Bad conversion")]
    BadConversion(),
    #[error("File or directory not found: {0}")]
    FileOrDirNotFound(String),
    #[error("Bios' hash not known: {0}")]
    UnknownBios(String),
    #[error("Bad CD serial number: expected {expected} got {got}")]
    BadSerialNumber {
        expected: SerialNumber,
        got: SerialNumber,
    },
    #[error("We couldn't find the disc's serial number")]
    NoSerialNumber,
    #[error("The disc format was incorrect (i.e. probably not a valid PSX disc image): `{0}`")]
    BadDiscFormat(String),
    #[error("Invalid or unknown CDC firmware")]
    BadCdcFirmware,
    #[error("Invalid PSX executable")]
    BadExe,
}