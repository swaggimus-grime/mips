pub mod wnd;
pub(crate) mod evt;
pub mod audio;

use std::ops::Deref;

pub struct Context {
    ctx: sdl3::Sdl,
    video_ctx: sdl3::VideoSubsystem,
    audio_ctx: sdl3::AudioSubsystem,
}

impl Context {
    pub fn new() -> Self {
        let ctx = sdl3::init().unwrap();
        let video_ctx = ctx.video().unwrap();
        let audio_ctx = ctx.audio().unwrap();

        Context {
            ctx,
            video_ctx,
            audio_ctx,       
        }
    }
    
    pub fn video(&self) -> &sdl3::VideoSubsystem {
        &self.video_ctx
    }
    pub fn audio(&self) -> &sdl3::AudioSubsystem {
        &self.audio_ctx
    }
}

impl Deref for Context {
    type Target = sdl3::Sdl;
    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}