use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{DerefMut, Index};
use std::rc::Rc;
use std::sync::mpsc::Sender;
use sdl3::keyboard::Keycode;
use tracing::error;
use crate::sdl::input::device::{Device};
use crate::sdl::input::dualshock::{ButtonIdx, DualShock};
use crate::sdl::input::keyboard::Keyboard;
use mips_lib::{ButtonState, Button, DeviceType, ButtonQueue};
use crate::sdl::input::raw_input::RawInput;

#[derive(Clone)]
pub struct Controller {
    id: u32,
    device: Rc<RefCell<Box<dyn Device>>>,
    senders: Vec<Sender<(ButtonState, String)>>,
}

impl Controller {
    pub fn new(device_type: DeviceType, id: u32) -> Self {
        Self {
            id,
            device: match device_type {
                DeviceType::Unknown => panic!("Unknown controller type"),
                DeviceType::Keyboard => Rc::new(RefCell::new(Box::new(Keyboard::new()))),
                DeviceType::DualShock => Rc::new(RefCell::new(Box::new(DualShock::new()))),
            },
            senders: Vec::new(),
        }
    }

    pub fn device_type(&self) -> DeviceType {
        self.device.borrow().device_type()
    }
    
    pub fn add_port_sender(&mut self, sender: Sender<(ButtonState, String)>) {
        self.senders.push(sender);
    }
    
    pub fn push_input<T: RawInput>(&mut self, state: ButtonState, raw_input: T) {
        for sender in self.senders.iter() {
            sender.send((state, raw_input.as_string())).unwrap();
        }
    }
}

pub struct ControllerMap {
    map: HashMap<DeviceType, Vec<Rc<RefCell<Controller>>>>
}

impl ControllerMap {
    pub fn new() -> Self {
        let mut map = HashMap::with_capacity(1);

        // We're always gonna have a keyboard to play with
        map.insert(DeviceType::Keyboard, vec![Rc::new(RefCell::new(Controller::new(DeviceType::Keyboard, 0)))]);

        Self {
            map
        }
    }

    pub fn keyboard(&mut self) -> Rc<RefCell<Controller>> {
        Rc::clone(&self.map.get_mut(&DeviceType::Keyboard).unwrap()[0])
    }

    pub fn insert_controller(&mut self, controller: Controller) {
        if let Some(controllers) = self.map.get_mut(&controller.device_type()) {
            debug_assert!(controller.id + 1 < controllers.len() as u32);
            controllers.push(Rc::new(RefCell::new(controller)));
        } else {
            self.map.insert(controller.device_type(), vec![Rc::new(RefCell::new(controller))]);
        }
    }

    pub fn remove_controller(&mut self, device_type: DeviceType, which: u32) {
        if let Some(controllers) = self.map.get_mut(&device_type) {
            controllers.remove(which as usize);
        } else {
            error!("Attempting to remove controller at index {}", which);
        }
    }

    pub fn push_keycode(&mut self, state: ButtonState, keycode: sdl3::keyboard::Keycode) {
        if let Some(keyboard_vec) = self.map.get_mut(&DeviceType::Keyboard) {
            match keyboard_vec.get_mut(0) {
                Some(keyboard) => keyboard.borrow_mut().push_input(state, keycode),
                None => {}
            }
        } else {
            error!("Failed to register keyboard input {}", keycode);
        }
    }

    pub fn push_gamepad_input(&mut self, state: ButtonState, which: u32, button_idx: u8) {
        if let Some(dualshocks) = self.map.get_mut(&DeviceType::DualShock) {
            debug_assert!(which < dualshocks.len() as u32);
            match dualshocks.get_mut(which as usize) {
                Some(dualshock) => dualshock.borrow_mut().push_input(state, ButtonIdx(button_idx)),
                None => {}
            }
        } else {
            error!("Failed to register dualshock input {}", button_idx);
        }
    }
}