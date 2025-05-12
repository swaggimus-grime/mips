use num_traits::ToPrimitive;
use sdl3::keyboard::Keycode;
use mips_lib::{Button, ButtonQueue, ButtonState, DeviceType};
use crate::sdl::input::device::Device;
use crate::sdl::input::raw_input::RawInput;

pub struct Keyboard {
    button_queue: ButtonQueue
}

impl Keyboard {
    pub fn new() -> Self {
        Keyboard {
            button_queue: Vec::new()
        }
    }
}

impl Device for Keyboard {
    fn device_type(&self) -> DeviceType {
        DeviceType::Keyboard
    }
    
}

impl RawInput for Keycode {
    fn device_type() -> DeviceType {
        DeviceType::Keyboard
    }
    
    fn as_string(&self) -> String {
        self.to_string()
    }
}
