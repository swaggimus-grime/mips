use std::env;
use std::ops::Deref;
use std::time::Duration;
use sdl3::gpu::TextureFormat;
use sdl3::pixels::PixelFormat;
use sdl3::render::TextureAccess;
use sdl3::sys::pixels::SDL_PixelFormat;
use tracing_subscriber::util::SubscriberInitExt;
use mips_lib::Mips;
use crate::sdl;

pub struct App {
    pub mips: Box<Mips>,
    ctx: sdl::Context,
    wnd: sdl::wnd::Window,
}

impl App {
    pub fn new() -> Self {
        let ctx = sdl::Context::new();
        let wnd = sdl::wnd::Window::new(&ctx);

        let sys_dir = env::current_dir().unwrap();
        
        App {
            mips: Mips::new(sys_dir.as_path(), Some("Silent Hill (USA).cue")).unwrap(),
            ctx,
            wnd
        }
    }

    pub fn run(&mut self) {
        let mut canvas = sdl::wnd::canvas::Canvas::from(&self.wnd);
        let mut event_pump = sdl::evt::EventPump::from(&self.ctx);
        let texture_creator = canvas.deref().texture_creator();
        let mut audio_device = sdl::audio::Device::from(&self.ctx);
        let mut audio_stream = sdl::audio::stream::Stream::from(&audio_device);
        audio_stream.resume();

        loop {
            canvas.clear();

            event_pump.poll();

            self.mips.update();

            if let Some(frame) = self.mips.output_frame() {
                let mut texture = texture_creator
                    .create_texture_streaming(unsafe { PixelFormat::from_ll(SDL_PixelFormat::XRGB8888) }, frame.width, frame.height)
                    .map_err(|e| e.to_string()).unwrap();

                // Cast the u32 buffer to &[u8]
                let byte_slice: &[u8] = unsafe {
                    std::slice::from_raw_parts(
                        frame.pixels.as_ptr() as *const u8,
                        frame.pixels.len() * std::mem::size_of::<u32>(),
                    )
                };

                // Update the texture in one call — fast!
                texture.update(None, byte_slice, (frame.width * 4) as usize);
                canvas.copy(&texture);
            }
            
            audio_stream.play(self.mips.output_audio_samples());
            self.mips.clear_audio_samples();

            canvas.present();
            //std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        }
    }
}
