use std::{env, result};
use std::error::Error;
use tracing::info;
use crate::core::app::App;
use crate::error::AppResult;

mod core;
mod error;
mod sdl;

fn main() {
    let mut app = App::new();
    app.run();
}