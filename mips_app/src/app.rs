use std::env;
use std::path::Path;
use winit::event::{Event, WindowEvent};
use mips_lib::Context;
use crate::window::{self, Wnd};
use crate::error::Result;

pub struct App {
    pub wnd: Wnd,
    ctx: Context
}

impl App {
    pub fn start() -> Result<()> {
        let wnd = Wnd::new()?;
        let mut app = App {
            wnd: wnd.0,
            ctx: Context::new_with_disc("assets/roms/Silent Hill (USA).cue").unwrap()
        };
        let ret = wnd.1.run((move |event, elwt| {
            match event {
                // Request a redraw when all events were processed.
                Event::AboutToWait => app.wnd.handle.request_redraw(),
                Event::WindowEvent { event, .. } => match event {
                    // Render a frame if our Vulkan app is not being destroyed.
                    WindowEvent::RedrawRequested if !elwt.exiting() => unsafe { app.render() }.unwrap(),
                    // Destroy our Vulkan app.
                    WindowEvent::CloseRequested => {
                        elwt.exit();
                        unsafe { app.destroy(); }
                    }
                    _ => {}
                }
                _ => {}
            }
        }));

        Ok(())
    }

    /// Renders a frame for our Vulkan app.
    pub unsafe fn render(&mut self) -> Result<()> {
        self.ctx.render_frame();
        Ok(())
    }

    pub unsafe fn destroy(&mut self) {}
}