use std::ops::Deref;
use crate::error::{Result, MipsError};

use winit::dpi::LogicalSize;
use winit::error::OsError;
use winit::event::{Event, WindowEvent};
use winit::event_loop::EventLoop;
use winit::window::{Window, WindowBuilder};
use crate::app::App;

pub struct Wnd {
    pub handle: Window
}

impl Wnd {
    pub fn new() -> Result<(Wnd, EventLoop<()>)> {
        let event_loop = EventLoop::new().unwrap();
        let handle = WindowBuilder::new()
            .with_title("Vulkan Tutorial (Rust)")
            .with_inner_size(LogicalSize::new(1024, 768))
            .build(&event_loop);
        match handle {
            Ok(handle) => {
                Ok((Wnd {
                    handle
                }, event_loop))
            },
            Err(e) => {
                Err(MipsError::WindowBuildError(e))
            }
        }
    }
}
