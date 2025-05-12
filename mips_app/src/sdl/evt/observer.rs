use crate::sdl::evt::EventPump;

pub trait Observer {
    fn subscribe(&mut self, event_pump: &mut EventPump);
    fn unsubscribe(&mut self, event_pump: &mut EventPump);
}