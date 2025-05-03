use std::ops::Deref;
use crate::sdl::Context;

pub struct EventPump {
    pump: sdl3::EventPump,
}

impl EventPump {
    pub fn poll(&mut self) {
        self.pump.poll_iter().for_each(drop);
    }
}

impl From<&Context> for EventPump {
    fn from(context: &Context) -> Self {
        EventPump {
            pump: context.event_pump().unwrap(),
        }
    }
}

impl Deref for EventPump {
    type Target = sdl3::EventPump;
    fn deref(&self) -> &Self::Target {
        &self.pump
    }
}