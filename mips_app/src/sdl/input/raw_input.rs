use mips_lib::{Button, DeviceType};

pub trait RawInput {
    fn device_type() -> DeviceType;
    
    fn as_string(&self) -> String;
}
