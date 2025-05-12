use std::cell::RefCell;
use crate::sdl::input::dualshock::DualShock;
use std::path::Path;
use std::rc::Rc;
use std::sync::mpsc::Receiver;
use mips_lib::{ButtonQueue, ButtonState};
use crate::sdl::input::config::Config;
use crate::sdl::input::controller::Controller;
use crate::sdl::input::keyboard::Keyboard;
use crate::sdl::input::map::ButtonMap;

pub struct Port {
    input_recv: Option<Receiver<(ButtonState, String)>>,
    btn_map: ButtonMap
}

impl Port {
    pub fn new() -> Self {
        Self {
            input_recv: None,
            btn_map: ButtonMap::new()
        }
    }
    
    pub fn connect_controller(&mut self, controller: Rc<RefCell<Controller>>) {
        let (input_send, input_recv) = std::sync::mpsc::channel();
        self.input_recv = Some(input_recv);
        controller.borrow_mut().add_port_sender(input_send);
    }
    
    pub fn load_config(&mut self, config: Config) {
        self.btn_map = ButtonMap::from(config);
    }
    
    pub fn inputs(&self) -> ButtonQueue {
        let mut queue = ButtonQueue::new();
        if let Some(input_recv) = self.input_recv.as_ref() {
            while let Ok((state, input_string)) = input_recv.try_recv() {
                if let Some(button)  = self.btn_map.button(input_string) {
                    queue.push((state, button));
                }
            }
        }
        
        queue
    }
}

