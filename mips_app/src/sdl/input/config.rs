use std::collections::HashMap;
use std::path::Path;
use ini::{Error, Ini, Properties};
use num_traits::FromPrimitive;
use sdl3::keyboard::Keycode;
use serde::{Deserialize, Serialize};
use tracing::{info, warn};
use mips_lib::{Button, DeviceType};

pub struct Config {
    device_type: DeviceType,
    bindings: HashMap<String, Button>
}

impl Config {
    pub fn write(&self) {
        let mut conf = Ini::new();
        conf.with_section(Some("User"))
            .set("name", "Raspberry树莓")
            .set("value", "Pi");
        conf.with_section(Some("Library"))
            .set("name", "Sun Yat-sen U")
            .set("location", "Guangzhou=world");
        conf.write_to_file("conf.ini").unwrap();
    }
    
    pub fn bindings(&self) -> HashMap<String, Button> {
        self.bindings.clone()
    }
}

impl From<&Path> for Config {
    fn from(path: &Path) -> Self {
        let ini = Ini::load_from_file(path).unwrap();
        let device_type = ini.section(Some("Device")).unwrap().get("Type").unwrap();
        let bindings_sec = ini.section(Some("Bindings")).unwrap();

        let mut device_type = match device_type {
            "Keyboard" => DeviceType::Keyboard,
            "Dualshock" => DeviceType::DualShock,
            _ => {
                warn!("Unknown device type in input config file {}: DeviceType = {}", path.display(), device_type);
                DeviceType::Unknown
            },
        };

        let mut bindings = HashMap::new();
        for (device_input, psx_input) in bindings_sec {
            bindings.insert(device_input.to_string(), Button::from_u32(psx_input.parse::<u32>().unwrap()).unwrap());
        }

        Config {
            device_type,
            bindings
        }
    }
}
