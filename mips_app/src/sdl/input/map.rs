use std::collections::HashMap;
use std::hash::Hash;
use mips_lib::Button;
use crate::sdl::input::config::Config;
use crate::sdl::input::raw_input::RawInput;

pub struct ButtonMap {
    map: HashMap<String, Button>
}

impl ButtonMap {
    pub fn new() -> Self {
        Self {
            map: HashMap::new()
        }
    }
    
    pub fn button(&self, input: String) -> Option<Button> {
        match self.map.get(&input) {
            Some(btn) => Some(*btn),
            None => None
        }
    }
}

impl From<Config> for ButtonMap {
    fn from(config: Config) -> Self {
        Self {
            map: config.bindings()
        }
    }
}