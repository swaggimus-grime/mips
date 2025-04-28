use std::{env, result};
use std::error::Error;
use tracing::info;
use crate::error::AppResult;

#[cfg(feature = "rasterizer-vulkan")]
mod core;
#[cfg(feature = "rasterizer-vulkan")]
mod gfx;
mod error;

#[cfg(feature = "rasterizer-vulkan")]
fn vulkan_entry() -> AppResult<()> {
    use crate::core::app::App;
    use winit::event_loop::{ControlFlow, EventLoop};
    use winit::error::EventLoopError;
    
    let event_loop = EventLoop::new().unwrap();

    // ControlFlow::Poll continuously runs the event loop, even if the OS hasn't
    // dispatched any events. This is ideal for games and similar applications.
    event_loop.set_control_flow(ControlFlow::Poll);

    // ControlFlow::Wait pauses the event loop if no events are available to process.
    // This is ideal for non-game applications that only update in response to user
    // input, and uses significantly less power/CPU time than ControlFlow::Poll.
    event_loop.set_control_flow(ControlFlow::Wait);

    let mut app = App::new(&event_loop);
    let exit_code = event_loop.run_app(&mut app);
    match exit_code {
        Err(EventLoopError::ExitFailure(error)) => {
            panic!("Error: {}", error);
        },
        _ => Ok(()),
    }
}

#[cfg(feature = "rasterizer-software")]
fn software_entry() -> AppResult<()> {
    use minifb::Window;
    use minifb::WindowOptions;
    use minifb::Key;
    use mips_lib::Mips;

    let sys_dir = env::current_dir().unwrap();
    let mut mips = Mips::new(sys_dir.as_path(), Some("Silent Hill (USA).cue")).unwrap();
    //mips.insert_disc("Silent Hill (USA).cue");
    
    const WIDTH: usize = 256;
    const HEIGHT: usize = 240;
    let mut window = Window::new(
        "Rust Game",
        WIDTH,
        HEIGHT,
        WindowOptions::default(),
    ).unwrap_or_else(|e| {
        panic!("{}", e);
    });

    window.limit_update_rate(Some(std::time::Duration::from_micros(16600))); // ~60fps

    let mut x: usize = 0;
    let mut y: usize = 0;

    while window.is_open() && !window.is_key_down(Key::Escape) {
        // Move the pixel when the arrow keys are pressed
        mips.update();
        if let Some(buffer) =  mips.output_frame() {
            window.update_with_buffer(buffer.pixels.as_slice(), buffer.width as usize, buffer.height as usize).unwrap();
        };

        let samples = mips.output_audio_samples();
        // Clear the emulator's buffer for next frame
        mips.clear_audio_samples();
        
        //mips.poll_mem_cards();
        //mips.poll_gamepads();
    }
    
    Ok(())
}

fn main() -> AppResult<()> {
    let stdout_subscriber = tracing_subscriber::fmt::init();
    info!("Begin log");
    
    #[cfg(feature = "rasterizer-software")]
    let result = software_entry();
    
    #[cfg(feature = "rasterizer-vulkan")]
    let result = vulkan_entry();
    
    result
}