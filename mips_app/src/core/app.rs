use std::env;
use std::sync::Arc;
use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowId};
use mips_lib::Mips;
use crate::error::*;
use crate::gfx::Gfx;

pub struct App {
    pub mips: Box<Mips>,
    ctx: Option<Ctx>
}

pub struct Ctx {
    window: Arc<Window>,
    gfx: Gfx
}

impl App {
    pub fn new(event_loop: &EventLoop<()>) -> Self {
        let sys_dir = env::current_dir().unwrap();
        
        App {
            mips: Mips::new(sys_dir.as_path(), None).unwrap(),
            ctx: None,
        }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let window = Arc::new(
            event_loop
                .create_window(Window::default_attributes())
                .unwrap(),
        );
        self.ctx = Some(Ctx { 
            window,
            gfx: Gfx::new()
        });
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => {
                println!("The close button was pressed; stopping");
                event_loop.exit();
            },
            WindowEvent::RedrawRequested => {
                self.mips.update();
                if let Some(ctx) = &self.ctx {
                    
                }
            }
            _ => (),
        }
    }
}