pub use mips_lib::{Button, ButtonState, DeviceType};
use mips_lib::ButtonQueue;

pub trait Device {
    fn device_type(&self) -> DeviceType;
}
