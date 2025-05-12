use mips_lib::{Button, ButtonState, DeviceType};
use crate::sdl::input::device::Device;
use crate::sdl::input::raw_input::RawInput;

pub struct ButtonIdx(pub u8);

pub struct DualShock {
    button_queue: Vec<(ButtonState, Button)>
}

impl DualShock {
    pub fn new() -> Self {
        DualShock {
            button_queue: Vec::new()
        }
    }
}

impl Device for DualShock {
    fn device_type(&self) -> DeviceType {
        DeviceType::DualShock
    }
}

impl RawInput for ButtonIdx {
    fn device_type() -> DeviceType {
        DeviceType::DualShock
    }

    fn as_string(&self) -> String {
        self.0.to_string()
    }
}