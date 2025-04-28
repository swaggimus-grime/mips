use thiserror::Error;

pub type AppResult<T> = Result<T, AppError>;

#[derive(Error, Debug)]
pub enum AppError {
    #[error("Window failed to build: {0}")]
    WindowBuildFailure(String)
}