pub(crate) mod stream;

use std::ops::Deref;
use sdl3::audio::{AudioFormat, AudioSpec};
use crate::sdl::Context;

#[derive(Clone)]
pub struct Device {
    device: sdl3::audio::AudioDevice
}

impl Device {
    pub fn resume(&mut self) {
        self.device.resume();
    }
}

impl From<sdl3::AudioSubsystem> for Device {
    fn from(audio: sdl3::AudioSubsystem) -> Self {
        Device {
            device: audio.default_playback_device()
        }
    }
}

impl Deref for Device {
    type Target = sdl3::audio::AudioDevice;
    fn deref(&self) -> &Self::Target {
        &self.device
    }
}

