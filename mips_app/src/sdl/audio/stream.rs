use std::ops::Deref;
use sdl3::audio::{AudioFormat, AudioSpec, AudioStream};
use crate::sdl::audio::Device;
use crate::sdl::Context;

pub struct Stream {
    stream: sdl3::audio::AudioStreamOwner,
}

impl Stream {
    pub fn play(&mut self, data: &[i16]) {
        self.stream.put_data_i16(data).unwrap();
    }
    
    pub fn resume(&mut self) {
        self.stream.resume();
    }
}

impl From<&Device> for Stream {
    fn from(dev: &Device) -> Self {
        let spec = AudioSpec {
            // The PSX SPU runs at 44.1 kHz
            freq: Some(44100),
            channels: Some(2),
            format: Some(AudioFormat::s16_sys())
        };

        let stream = dev.deref().clone().open_device_stream(Some(&spec)).unwrap();

        Stream {
            stream
        }
    }
}