use std::io;
use thiserror::Error;

pub type Result<T> = ::std::result::Result<T, AppError>;

#[derive(Error, Debug)]
pub enum AppError {
    #[error("Window failed to build: {0}")]
    WindowBuildFailure(String)
}