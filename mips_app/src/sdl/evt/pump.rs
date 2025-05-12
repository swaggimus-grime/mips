use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use sdl3::event::{Event, EventPollIterator, EventType, WindowEvent};
use mips_lib::ButtonState;
use crate::core::app::App;
use crate::sdl::Context;
use crate::sdl::evt::observer::Observer;
use crate::sdl::input::{Controller, DeviceType};

type Handler = fn(e: Event);

pub struct EventPump {
    pump: sdl3::EventPump,
}

impl EventPump {
    
    fn poll_iter(&mut self) -> EventPollIterator {
        self.pump.poll_iter()
    }
}

pub fn poll(app: &mut App) {
    let events = app.event_pump.poll_iter();
    for e in events {
        match e {
            Event::Quit {..} => {
                app.running = false;
            },
            Event::KeyDown { keycode: Some(keycode), ..} => {
                app.controllers.push_keycode(ButtonState::Pressed, keycode);
            },
            Event::KeyUp { keycode: Some(keycode), ..} => {
                app.controllers.push_keycode(ButtonState::Released, keycode);
            },
            Event::JoyDeviceAdded { which, .. } => {
                let controller = Controller::new(DeviceType::DualShock, which);
                app.controllers.insert_controller(controller);
            },
            Event::JoyDeviceRemoved { which, .. } => {
                app.controllers.remove_controller(DeviceType::DualShock, which);
            },
            Event::JoyButtonDown {which, button_idx, ..} => {
                app.controllers.push_gamepad_input(ButtonState::Pressed, which, button_idx);
            },
            Event::JoyButtonUp {which, button_idx, ..} => {
                app.controllers.push_gamepad_input(ButtonState::Released, which, button_idx);
            },
            Event::Window { win_event, ..} => {
                match win_event {
                    WindowEvent::Resized(w, h) => {
                        //app.wnd.on_resize(w, h);
                    },
                    _ => {}
                }
            }
            _ => {}
        }
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