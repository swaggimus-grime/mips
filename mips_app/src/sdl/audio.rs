pub(crate) mod stream;

use std::ops::Deref;
use sdl3::audio::{AudioFormat, AudioSpec};
use crate::sdl::Context;

pub struct Device {
    device: sdl3::audio::AudioDevice
}

impl Device {
    pub fn resume(&mut self) {
        self.device.resume();
    }
}

impl From<&Context> for Device {
    fn from(ctx: &Context) -> Self {
        Device {
            device: ctx.audio().default_playback_device()
        }
    }
}

impl Deref for Device {
    type Target = sdl3::audio::AudioDevice;
    fn deref(&self) -> &Self::Target {
        &self.device
    }
}