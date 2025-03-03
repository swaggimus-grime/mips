use thiserror::Error;
use winit::error::OsError;

pub type Result<T> = ::std::result::Result<T, MipsError>;

#[derive(Error, Debug)]
pub enum MipsError {
    #[error("Failed to build window")]
    WindowBuildError(#[from] OsError)
}
