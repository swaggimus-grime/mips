use std::collections::HashMap;
use std::env;
use std::ops::Deref;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use sdl3::event::EventType;
use tracing::info;
use mips_lib::Mips;
use crate::sdl;
use crate::sdl::input::DeviceType;
use crate::ui::UI;

pub struct App {
    pub mips: Box<Mips>,
    ctx: sdl::Context,
    pub event_pump: sdl::evt::EventPump,
    wnd: sdl::wnd::Window,
    pub running: bool,
    ports: [sdl::input::Port; 2],
    pub controllers: sdl::input::ControllerMap,
    ui: UI
}

impl App {
    pub fn new() -> Self {
        let ctx = sdl::Context::new();
        let wnd = sdl::wnd::Window::new(&ctx);
        let event_pump = sdl::evt::EventPump::from(&ctx);

        let sys_dir = env::current_dir().unwrap();

        tracing_subscriber::fmt::init();
        info!("Begin log");
        
        App {
            mips: Mips::new(sys_dir.as_path(), Some("Silent Hill (USA).cue")).unwrap(),
            ctx,
            event_pump,
            wnd,
            running: true,
            ports: [
                sdl::input::Port::new(), 
                sdl::input::Port::new(),
            ],
            controllers: sdl::input::ControllerMap::new(),
            ui: UI::new()
        }
    }
    
    pub fn on_resize(&mut self, w: i32, h: i32) {
        
    }

    pub fn run(&mut self) {
        use std::ops::Deref;
        use sdl3::pixels::PixelFormat;
        use sdl3::sys::pixels::SDL_PixelFormat;

        let mut canvas = sdl::wnd::canvas::Canvas::from(&self.wnd);
        
        let texture_creator = canvas.deref().texture_creator();
        
        if let Some(port) = self.ports.get_mut(0) {
            let config = sdl::input::Config::from("assets/config/profile.input.ini".as_ref());
            port.connect_controller(self.controllers.keyboard());
            port.load_config(config);
            self.mips.connect_gamepad(0, DeviceType::Keyboard);
        }

        // Audio stream: must be created and used in main thread
        let audio = self.ctx.audio();
        let audio_device = sdl::audio::Device::from(audio);
        let mut audio_stream = sdl::audio::stream::StreamWithCallback::from(audio_device);
        audio_stream.resume();

        let mut texture = texture_creator
            .create_texture_streaming(
                unsafe { PixelFormat::from_ll(SDL_PixelFormat::XRGB8888) },
                self.wnd.width(),
                self.wnd.height(),
            )
            .expect("Failed to create texture");

        const FRAME_TIME: Duration = Duration::from_nanos(16_666_667); // ~60 FPS
        let mut last_frame = Instant::now();
        
        // Main loop
        while self.running {
            let frame_start = Instant::now();
            
            sdl::evt::pump::poll(self);

            audio_stream.enqueue(self.mips.output_audio_samples());
            self.mips.clear_audio_samples();
            
            self.mips.update();
            
            // Render video frame
            if let Some(frame) = self.mips.output_frame() {
                canvas.clear();
                if frame.width != self.wnd.width() || frame.height != self.wnd.height() {
                    texture = texture_creator
                        .create_texture_static(
                            unsafe { PixelFormat::from_ll(SDL_PixelFormat::XRGB8888) },
                            frame.width,
                            frame.height,
                        )
                        .expect("Failed to create texture");
                }

                let pitch = frame.width as usize * size_of::<u32>(); // bytes per row
                let pixel_bytes: &[u8] = bytemuck::cast_slice(frame.pixels.as_slice());

                texture.update(None, pixel_bytes, pitch).unwrap();
                canvas.copy(&texture);
                canvas.present();
            }
            
            self.ui.update();
            
            let btn_states = self.ports[0].inputs();
            self.mips.poll_gamepads(btn_states);
            self.mips.refresh_gamepads();

            // Frame timing
            let elapsed = frame_start.elapsed();
            if elapsed < FRAME_TIME {
                std::thread::sleep(FRAME_TIME - elapsed);
            }

            last_frame = frame_start;
        }
    }
}
