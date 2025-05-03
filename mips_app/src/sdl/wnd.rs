pub mod canvas;

use std::ops::Deref;
use crate::sdl;

pub struct Window {
    wnd: sdl3::video::Window,
}

impl Window {
    pub fn new(ctx: &sdl::Context) -> Self {
        let sdl_window = ctx.video().window("MIPS", 256, 240)
            .position_centered()
            .build()
            .unwrap();

        Window {
            wnd: sdl_window,
        }
    }

    pub fn wnd(&self) -> &sdl3::video::Window {
        &self.wnd
    }
    
    pub fn width(&self) -> u32 {
        self.wnd.size_in_pixels().0
    }
    
    pub fn height(&self) -> u32 {
        self.wnd.size_in_pixels().1
    }
}

impl Deref for Window {
    type Target = sdl3::video::Window;
    fn deref(&self) -> &Self::Target {
        &self.wnd
    }
}