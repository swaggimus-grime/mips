pub mod wnd;
pub(crate) mod evt;
pub mod audio;

use std::ops::Deref;

pub struct Context {
    ctx: sdl3::Sdl,
}

impl Context {
    pub fn new() -> Self {
        let ctx = sdl3::init().unwrap();

        Context {
            ctx,
        }
    }
    
    pub fn video(&self) -> sdl3::VideoSubsystem {
        self.ctx.video().unwrap()
    }
    pub fn audio(&self) -> sdl3::AudioSubsystem {
        self.ctx.audio().unwrap()
    }
}

impl Deref for Context {
    type Target = sdl3::Sdl;
    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}