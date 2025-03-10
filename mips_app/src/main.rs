use std::error::Error;
use winit::event_loop::{ControlFlow, EventLoop};
use crate::core::app::App;

mod core;
mod gfx;
mod error;

fn main() -> Result<(), impl Error> {
    let event_loop = EventLoop::new().unwrap();

    // ControlFlow::Poll continuously runs the event loop, even if the OS hasn't
    // dispatched any events. This is ideal for games and similar applications.
    event_loop.set_control_flow(ControlFlow::Poll);

    // ControlFlow::Wait pauses the event loop if no events are available to process.
    // This is ideal for non-game applications that only update in response to user
    // input, and uses significantly less power/CPU time than ControlFlow::Poll.
    event_loop.set_control_flow(ControlFlow::Wait);

    let mut app = App::new(&event_loop);
    event_loop.run_app(&mut app)
}