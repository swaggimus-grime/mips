//! Rustation libretro core

// ```
#![allow(clippy::needless_range_loop)]
// This one is not too terrible but I find it not very useful when writing an emulator because
// the vast majority of the time our types are actually constrained by the original
// hardware, so using "as" casts is not a problem the vast majority of the time.
#![allow(clippy::cast_lossless)]
// I seem to get weird false positive in the GTE code for this one
#![allow(clippy::redundant_closure_call)]
// Wants to rewrite some numeric comparison chains as match when it doesn't make a lot of sense
// IMO.
#![allow(clippy::comparison_chain)]
// Doesn't let me use r, g, b and y, u, v in the same function!
#![allow(clippy::many_single_char_names)]
// I get a lot of false positives in the GPU draw code for this one
#![allow(clippy::redundant_clone)]
// Useless warning in rasterizer code
#![allow(clippy::useless_let_if_seq)]

#[macro_use]
extern crate arrayref;
extern crate libc;
#[macro_use]
extern crate log;
extern crate cdimage;
extern crate flexbuffers;
extern crate fnv;
extern crate serde;
extern crate serde_big_array;
extern crate sha as sha_backend;
extern crate thiserror;

mod assembler;
mod bitwise;
mod box_array;
mod error;
mod memory_card;
#[cfg(feature = "debugger")]
mod debugger;
mod psx;
mod sha;

use std::env;
use crate::sha::sha256;
use box_array::BoxArray;
use cdimage::cue::Cue;
use error::{PsxError, Result};
use memory_card::MemoryCardFile;
use psx::bios::Metadata;
use psx::bios::{Bios, BIOS_SIZE};
use psx::cd::CdcState;
use psx::disc::Disc;
use psx::gpu::RasterizerOption;
use psx::pad_memcard::devices::gamepad::{Button, ButtonState, DigitalPad, DualShock};
use psx::pad_memcard::devices::{DeviceInterface, DisconnectedDevice};
use psx::{Psx, CDC_ROM_SHA256, CDC_ROM_SIZE};
use std::ffi::OsStr;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use crate::psx::disc::Region;

/// Emulation context containing the emulator state
pub struct Context {
    psx: Box<psx::Psx>,
    /// All disc images this emulator instance knows about
    images: Vec<DiscImage>,
    /// Index of the currently selected image in `images`
    cur_image: usize,
    /// The type of controller connected on the two input ports
    //controller_type: [libretro::InputDevice; 2],
    /// The type of MemoryCards connected to the console (user-configurable)
    memcard_types: [options::MemoryCardType; 2],
    /// Objects used to deal with reading/storing the memory card images to files
    memcard_files: [MemoryCardFile; 2],
    /// Internal frame width
    internal_width: u32,
    /// Internal frame height
    internal_height: u32,
    /// Current max width
    max_width: u32,
    /// Current max height
    max_height: u32,
    /// Current aspect ratio
    aspect_ratio: f32,
    /// Button combination used to control the analog button
    analog_combo: AnalogCombo,
    /// Ratio to match the analog deadzone of the emulator's controller to that of a real DualShock
    analog_compensation: f32,
    /// CD state overlay mode
    cd_overlay: CdOverlay,
    /// Current position of the CD spin indicator
    cd_spin_pos: f32,
}

impl Context {
    pub fn new_with_disc<T: AsRef<Path>>(disc_path: T) -> Result<Context> {
        let image = DiscImage::new(disc_path);
        let psx = Context::load_disc(&image)?;
        let mut ctx = Context {
            psx,
            images: vec![image],
            cur_image: 0,
            // Start with both port disconnected and wait for the frontend to tell us what to use
            // in `set_controller`
            //controller_type: [libretro::InputDevice::None; 2],
            memcard_types: [options::MemoryCardType::Disconnected; 2],
            memcard_files: [MemoryCardFile::dummy(), MemoryCardFile::dummy()],
            internal_width: 640,
            internal_height: 480,
            max_width: 640,
            max_height: 576,
            aspect_ratio: 4. / 3.,
            analog_combo: AnalogCombo::SelectR3,
            analog_compensation: 1.,
            cd_overlay: CdOverlay::Disabled,
            cd_spin_pos: 0.,
        };


        //libretro::Context::refresh_variables(&mut ctx);

        //ctx.setup_memory_cards();

        #[cfg(feature = "debugger")]
        {
            let mut debugger = Box::new(debugger::Debugger::new());

            // Set to true to log BIOS API calls
            debugger.set_log_bios_calls(false);

            psx::debugger::swap_debugger(debugger);

            // Trigger the debugger immediately to wait for a connection
            psx::debugger::trigger_break(&mut ctx.psx);
        }

        Ok(ctx)
    }

    /*
    fn cur_image(&self) -> &DiscImage {
        self.images.get(self.cur_image).expect("Invalid cur_image!")
    }
    */
    
    /*
    fn poll_controllers(&mut self) {
        let gamepads = self.psx.pad_memcard.gamepads_mut();

        for port in 0..2 {
            let ty = self.controller_type[port];

            // Update buttons
            let has_buttons =
                ty == libretro::InputDevice::JoyPad || ty == libretro::InputDevice::Analog;

            let mut select_pressed = false;
            let mut l3_pressed = false;
            let mut r3_pressed = false;

            if has_buttons {
                let device = gamepads[port].device_mut();

                for &(retrobutton, psxbutton) in &BUTTON_MAP {
                    let state = if libretro::button_pressed(port, retrobutton) {
                        match retrobutton {
                            libretro::JoyPadButton::Select => select_pressed = true,
                            libretro::JoyPadButton::R3 => r3_pressed = true,
                            libretro::JoyPadButton::L3 => l3_pressed = true,
                            _ => (),
                        }

                        ButtonState::Pressed
                    } else {
                        ButtonState::Released
                    };

                    device.set_button_state(psxbutton, state);
                }
            }

            // Update analog sticks
            let has_sticks = ty == libretro::InputDevice::Analog;

            if has_sticks {
                let device = gamepads[port].device_mut();

                // Special combo for the Analog button
                let analog_pressed = match self.analog_combo {
                    AnalogCombo::SelectR3 => select_pressed && r3_pressed,
                    AnalogCombo::SelectL3 => select_pressed && l3_pressed,
                    AnalogCombo::L3R3 => l3_pressed && r3_pressed,
                };

                device.set_button_state(
                    Button::Analog,
                    if analog_pressed {
                        ButtonState::Pressed
                    } else {
                        ButtonState::Released
                    },
                );

                let comp = self.analog_compensation;

                let compensate = |v: i16| {
                    let v = f32::from(v) * comp;

                    if v > f32::from(i16::MAX) {
                        i16::MAX
                    } else if v < f32::from(i16::MIN) {
                        i16::MIN
                    } else {
                        v as i16
                    }
                };

                let left_x = compensate(libretro::axis_state(
                    port,
                    libretro::AnalogInput::Left,
                    libretro::AnalogAxis::X,
                ));
                let left_y = compensate(libretro::axis_state(
                    port,
                    libretro::AnalogInput::Left,
                    libretro::AnalogAxis::Y,
                ));
                let right_x = compensate(libretro::axis_state(
                    port,
                    libretro::AnalogInput::Right,
                    libretro::AnalogAxis::X,
                ));
                let right_y = compensate(libretro::axis_state(
                    port,
                    libretro::AnalogInput::Right,
                    libretro::AnalogAxis::Y,
                ));

                device.set_axis_state((left_x, left_y), (right_x, right_y));

                let (strong, weak) = device.get_rumble();

                // Values are 8 bits on the PSX
                let mut strong = strong as u16;
                strong |= strong << 8;
                libretro::set_rumble(port, libretro::RumbleEffect::Strong, strong);

                let mut weak = weak as u16;
                weak |= weak << 8;
                libretro::set_rumble(port, libretro::RumbleEffect::Weak, weak);
            }
        }
    }
    */

    fn load_image(image: &DiscImage) -> Result<Disc> {
        let path = image.path();

        let disc = if path.extension().and_then(|ext| ext.to_str()) == Some("cue") {
            Cue::new(path)
        } else {
            Cue::new_from_zip(path)
        }?;

        let disc = Disc::new(Box::new(disc))?;

        let serial = disc.serial_number();
        let region = disc.region();

        info!("Disc serial number: {}", serial);
        info!("Detected disc region: {:?}", region);

        Ok(disc)
    }

    fn load_disc(image: &DiscImage) -> Result<Box<psx::Psx>> {
        let sys_dir = &image.path.parent().unwrap();
        let disc = Context::load_image(image)?;
        let region = disc.region();

        let bios = find_bios(|md| md.region == region, &sys_dir)?;

        let cdc_firmware = find_cdc_firmware(sys_dir)?;

        Ok(psx::Psx::new_with_disc(disc, bios, cdc_firmware)?)
    }

    /*
    /// Disconnect any configured Memory Card
    fn disconnect_memory_cards(&mut self) {
        let mut memory_cards = self.psx.pad_memcard.memory_cards_mut();

        for m in &mut memory_cards {
            let device = Box::new(DisconnectedDevice);
            m.connect_device(device);
        }

        for m in self.memcard_files.iter_mut() {
            *m = MemoryCardFile::dummy();
        }
    }
     */

    /// Initialize the memory cards connected to the emulated console
    /*
    fn setup_memory_cards(&mut self) {
        // Start by disconnecting everything to make sure we never half-configure the memory card.
        // We want to make it hard for the user to lose their saves...
        self.disconnect_memory_cards();

        let save_path = match libretro::get_save_directory() {
            Some(p) => p,
            None => {
                warn!("No save directory defined, disabling memory cards");
                return;
            }
        };

        for slot in 0..=1 {
            let filename = match self.memcard_types[slot] {
                options::MemoryCardType::Disconnected => continue,
                options::MemoryCardType::Common(idx) => {
                    let p = format!("rustation_common.{}.mcr", idx);

                    save_path.join(p)
                }
                options::MemoryCardType::PerGame(idx) => {
                    let p: &Path = self.cur_image().basename().as_ref();
                    let p = p.with_extension(format!("{}.mcr", idx));

                    save_path.join(p)
                }
            };

            match MemoryCardFile::load_or_create(&filename) {
                Ok((mcf, mc)) => {
                    // Success
                    info!("Memory Card {} is {}", slot, mcf.path().display());

                    let memory_cards = self.psx.pad_memcard.memory_cards_mut();
                    memory_cards[slot].connect_device(Box::new(mc));
                    self.memcard_files[slot] = mcf;
                }
                Err(e) => {
                    error!(
                        "Can't load or create memory card '{}': {}",
                        filename.display(),
                        e
                    );
                }
            }
        }
    }

    /// Called when we're about to quit to force-flush any pending Memory Card write
    fn flush_memory_cards(&mut self) {
        let memory_cards = self.psx.pad_memcard.memory_cards();

        self.memcard_files[0].force_dump(memory_cards[0].device());
        self.memcard_files[1].force_dump(memory_cards[1].device());
    }
    */

    // Precise FPS values for the video output for the given VideoClock. It's actually possible to
    // configure the PlayStation GPU to output with NTSC timings with the PAL clock (and vice-versa)
    // which would make this code invalid but it wouldn't make a lot of sense for a game to do that.
    fn video_output_framerate(&self) -> f32 {
        todo!();
        /*
        match self.psx.video_standard() {
            // 53.690MHz GPU clock frequency, 263 lines per field, 3413 cycles per line
            psx::VideoStandard::Ntsc => 59.81,
            // 53.222MHz GPU clock frequency, 314 lines per field, 3406 cycles per line
            psx::VideoStandard::Pal => 49.76,
        }

         */
    }

    /*
    fn get_geometry(&self) -> libretro::GameGeometry {
        let upscale_shift = options::CoreOptions::internal_upscale_factor();

        let vram_display_mode = options::CoreOptions::display_full_vram();

        let (w, h) = vram_display_mode.max_resolution();
        let aspect_ratio = vram_display_mode.aspect_ratio();

        let w = w as u32;
        let h = h as u32;

        let max_width;
        let max_height;
        if w <= 640 && h <= 576 {
            max_width = (640 << upscale_shift) as libc::c_uint;
            max_height = (576 << upscale_shift) as libc::c_uint;
        } else {
            // Normally only used in "weird" modes such as "show full VRAM"
            //
            // Does not scale with upscale_shift
            max_width = w as libc::c_uint;
            max_height = h as libc::c_uint;
        }

        libretro::GameGeometry {
            base_width: self.internal_width,
            base_height: self.internal_height,
            max_width,
            max_height,
            aspect_ratio,
        }
    }

     */

    pub fn render_frame(&mut self) {
        //self.poll_controllers();

        self.psx.run_frame();

        self.output_frame();

        // Send sound samples
        let samples = self.psx.get_audio_samples();
        //libretro::send_audio_samples(samples);
        // Clear the emulator's buffer for next frame
        self.psx.clear_audio_samples();

        // Refresh memory cards
        let mut memory_cards = self.psx.pad_memcard.memory_cards_mut();
        for (file, mc) in self.memcard_files.iter_mut().zip(memory_cards.iter_mut()) {
            let device = mc.device_mut();

            device.new_frame();
            file.maybe_dump(device);
        }

        // Refresh pads
        let mut gamepads = self.psx.pad_memcard.gamepads_mut();
        for gp in gamepads.iter_mut() {
            let device = gp.device_mut();

            device.new_frame();
        }
    }

    pub fn output_frame(&mut self) {
        let draw_cd_state = match self.cd_overlay {
            CdOverlay::Disabled => None,
            CdOverlay::Enabled => Some((
                self.psx.cd.state(),
                self.psx.cd.disc_speed(),
                self.psx.cd.sled_position(),
            )),
            CdOverlay::Dynamic => {
                let state = self.psx.cd.state();

                if state.is_idle() {
                    None
                } else {
                    Some((state, self.psx.cd.disc_speed(), self.psx.cd.sled_position()))
                }
            }
        };

        let mut frame = match self.psx.take_frame() {
            Some(f) => f,
            None => {
                error!("No frame generated!");
                return;
            }
        };

        /*
        if frame.width != self.internal_width || frame.height != self.internal_height {
            // Internal resolution changed, see if we need to adjust the geometry
            self.internal_width = frame.width;
            self.internal_height = frame.height;

            let geom = self.get_geometry();

            if geom.aspect_ratio != self.aspect_ratio
                || geom.max_width != self.max_width
                || geom.max_height != self.max_height
            {
                self.max_width = geom.max_width;
                self.max_height = geom.max_height;
                self.aspect_ratio = geom.aspect_ratio;
                //libretro::set_geometry(&geom);
            }
        }
        */

        if let Some((state, speed, disc_pos)) = draw_cd_state {
            use crate::psx::gpu::Pixel;

            // Draw the disc state indicator
            let dim = std::cmp::min(frame.width / 10, frame.height);
            let x_off = dim;
            let y_off = dim;

            // Disc circle
            let r = dim / 2 - 1;
            let x_center = x_off + dim / 2;
            let y_center = y_off + dim / 2;

            let mut draw_pixel = |x: u32, y: u32, color: Pixel| {
                frame.pixels[(y * frame.width + x) as usize] = color.to_rgb888();
            };

            let col = match state {
                CdcState::ShellOpen => Pixel::from_rgb(0x84, 0x60, 0x1d),
                CdcState::NoDisc => Pixel::from_rgb(0x84, 0x2e, 0x1d),
                CdcState::Idle => Pixel::from_rgb(0x4a, 0x4a, 0x4a),
                CdcState::Seeking => Pixel::from_rgb(0xaf, 0xad, 0x2d),
                CdcState::AudioStreaming => Pixel::from_rgb(0x11, 0x87, 0x23),
                CdcState::DataStreaming => Pixel::from_rgb(0x9a, 0x2d, 0xaf),
            };

            // Draw the circle using Bresenham's algo
            {
                // Start from the top
                let mut x = 0;
                let mut y = r;
                let mut d = 3 - 2 * (r as i32);

                let mut draw_octants = |x, y| {
                    draw_pixel(x_center + x, y_center + y, col);
                    draw_pixel(x_center + x, y_center - y, col);
                    draw_pixel(x_center - x, y_center + y, col);
                    draw_pixel(x_center - x, y_center - y, col);
                    draw_pixel(x_center + y, y_center + x, col);
                    draw_pixel(x_center + y, y_center - x, col);
                    draw_pixel(x_center - y, y_center + x, col);
                    draw_pixel(x_center - y, y_center - x, col);
                };

                draw_octants(x, y);

                while x < y {
                    x = x + 1;
                    if d > 0 {
                        y = y - 1;
                        d = d + 4 * ((x as i32) - (y as i32)) + 10;
                    } else {
                        d = d + 4 * (x as i32) + 6;
                    }
                    draw_octants(x, y);
                }
            }

            // Draw the spin/position indicator
            if state != CdcState::ShellOpen && state != CdcState::NoDisc {
                use std::f32::consts::PI;

                // Linear speed: 75 * speed frames (sectors) per second
                let linear_speed_mm =
                    cdimage::disc_position::CD_FRAME_LENGTH_MM * 75 * (speed as u32);
                let radius_mm = disc_pos.disc_radius().map(|r| r.to_millis()).unwrap_or(1.);

                // Relative position on the disc
                let mut pos_rel =
                    radius_mm / cdimage::disc_position::CD_PROGRAM_RADIUS_MAX.to_millis();

                if pos_rel > 1.0 {
                    pos_rel = 1.0;
                }

                let rps = (linear_speed_mm as f32) / (radius_mm * PI * 2.);
                // We divide by 10 because otherwise the spin speed is too fast for the framerate
                // That means that the spin indicator is 5 times slower than real time
                self.cd_spin_pos += (rps / self.video_output_framerate()) / 5.;
                self.cd_spin_pos = self.cd_spin_pos.fract();

                let rads = self.cd_spin_pos * PI * 2.;

                let x = ((x_center as f32) + ((r as f32) * rads.cos() * pos_rel).round()) as u32;
                let y = ((y_center as f32) + ((r as f32) * rads.sin() * pos_rel).round()) as u32;

                draw_pixel(x, y, col);
                draw_pixel(x + 1, y, col);
                draw_pixel(x - 1, y, col);
                draw_pixel(x, y + 1, col);
                draw_pixel(x, y - 1, col);
            }
        }

        //libretro::frame_done(&frame.pixels, frame.width, frame.height);
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        info!("Shutting down");
        //self.flush_memory_cards();
    }
}

/*
impl libretro::Context for Context {
    fn set_controller(&mut self, port: usize, mut device: libretro::InputDevice, subclass: u32) {
        if port > 1 {
            warn!(
                "Can't configure controller for port {}, only 2 supported",
                port + 1
            );
            return;
        }

        let gamepads = self.psx.pad_memcard.gamepads_mut();

        let new_pad: Box<dyn DeviceInterface> = match subclass {
            PSX_CONTROLLER_DIGITAL => Box::new(DigitalPad::new()),
            PSX_CONTROLLER_DUALSHOCK => Box::new(DualShock::new()),
            _ => {
                // Try to match the generic class instead
                match device {
                    libretro::InputDevice::None => Box::new(DisconnectedDevice),
                    libretro::InputDevice::JoyPad => Box::new(DigitalPad::new()),
                    libretro::InputDevice::Analog => Box::new(DualShock::new()),
                    _ => {
                        error!(
                            "Received bogus controller config for port {}: {:?}.0x{:x}.\
                               Disconnecting it",
                            port, device, subclass
                        );
                        device = libretro::InputDevice::None;
                        Box::new(DisconnectedDevice)
                    }
                }
            }
        };

        info!("New controller on port {}: {}", port, new_pad.description());

        //self.controller_type[port] = device;
        gamepads[port].connect_device(new_pad);
    }

    fn render_frame(&mut self) {
        self.poll_controllers();

        self.psx.run_frame();

        self.output_frame();

        // Send sound samples
        let samples = self.psx.get_audio_samples();
        //libretro::send_audio_samples(samples);
        // Clear the emulator's buffer for next frame
        self.psx.clear_audio_samples();

        // Refresh memory cards
        let mut memory_cards = self.psx.pad_memcard.memory_cards_mut();
        for (file, mc) in self.memcard_files.iter_mut().zip(memory_cards.iter_mut()) {
            let device = mc.device_mut();

            device.new_frame();
            file.maybe_dump(device);
        }

        // Refresh pads
        let mut gamepads = self.psx.pad_memcard.gamepads_mut();
        for gp in gamepads.iter_mut() {
            let device = gp.device_mut();

            device.new_frame();
        }
    }

    fn get_system_av_info(&self) -> libretro::SystemAvInfo {
        libretro::SystemAvInfo {
            geometry: self.get_geometry(),
            timing: libretro::SystemTiming {
                fps: self.video_output_framerate() as f64,
                sample_rate: 44_100.,
            },
        }
    }

    /*
    fn refresh_variables(&mut self) {
        self.analog_combo = options::CoreOptions::analog_combo();
        self.analog_compensation = options::CoreOptions::analog_compensation();
        self.cd_overlay = options::CoreOptions::cd_overlay();

        let vram_display_mode = options::CoreOptions::display_full_vram();
        self.psx
            .gpu
            .set_rasterizer_option(RasterizerOption::VRamDisplayMode(vram_display_mode));

        let force_transparency = options::CoreOptions::force_transparency();
        self.psx
            .gpu
            .set_rasterizer_option(RasterizerOption::ForceTransparency(force_transparency));

        let color_depth = options::CoreOptions::internal_color_depth();

        let d24 = match color_depth {
            15 => false,
            24 => true,
            d => panic!("Unexpected color depth: {}", d),
        };

        self.psx
            .gpu
            .set_rasterizer_option(RasterizerOption::Draw24Bpp(d24));
        self.psx
            .gpu
            .set_rasterizer_option(RasterizerOption::DitherForceDisable(d24));

        let (wf, draw_poly) = match options::CoreOptions::wireframe() {
            options::WireframeMode::Disabled => (false, true),
            options::WireframeMode::Overlay => (true, true),
            options::WireframeMode::Exclusive => (true, false),
        };
        self.psx
            .gpu
            .set_rasterizer_option(RasterizerOption::Wireframe(wf));
        self.psx
            .gpu
            .set_rasterizer_option(RasterizerOption::DrawPolygons(draw_poly));

        let upscale_shift = options::CoreOptions::internal_upscale_factor();
        self.psx
            .gpu
            .set_rasterizer_option(RasterizerOption::UpscaleShift(upscale_shift));

        self.psx
            .cd
            .set_cd_loading_speed(options::CoreOptions::cd_speed() / 2);

        self.psx
            .spu
            .set_reverb_enable(options::CoreOptions::reverb_enable());

        self.psx
            .gte
            .set_overclock(options::CoreOptions::gte_overclock());

        let mut memcard_types = [
            options::CoreOptions::memory_card_1_type(),
            options::CoreOptions::memory_card_2_type(),
        ];

        if memcard_types[0] == memcard_types[1] {
            warn!("Both memory cards are set to the same type, disconnecting slot 2");
            memcard_types[1] = options::MemoryCardType::Disconnected;
        }

        if memcard_types != self.memcard_types {
            self.memcard_types = memcard_types;
            self.setup_memory_cards();
        }
    }

     */

    fn reset(&mut self) {
        match Context::load_disc(self.cur_image()) {
            Ok(mut psx) => {
                info!("Game reset");
                std::mem::swap(&mut self.psx.pad_memcard, &mut psx.pad_memcard);
                self.psx = psx;
            }
            Err(_) => warn!("Couldn't reset game"),
        }
    }

    fn gl_context_reset(&mut self) {}

    fn gl_context_destroy(&mut self) {}

    fn serialize_size(&self) -> usize {
        // Needs to be an upper bound for the savestate size. For now our savestates's layout is
        // not fixed so the size is not constant (which makes things like netplay impossible)
        18 * 1024 * 1024
    }

    fn serialize(&mut self, buf: &mut [u8]) -> ::std::result::Result<(), ()> {
        use serde::Serialize;

        // Make sure that we don't have anything "stuck" in our rasterizer channels since otherwise
        // it won't be correctly serialized.
        let _frame = self.psx.take_frame();

        let mut fb = flexbuffers::FlexbufferSerializer::new();

        if let Err(e) = self.psx.serialize(&mut fb) {
            error!("Couldn't serialize savestate: {}", e);
            return Err(());
        };

        let fbuf = fb.view();

        if fbuf.len() > buf.len() {
            error!(
                "Couldn't serialize savestate because it's too big ({} > {})",
                fbuf.len(),
                buf.len()
            );
            return Err(());
        }

        buf[0] = b'R';
        buf[1] = b'S';
        buf[2] = b'X';
        buf[3] = b'1';

        let len = (fbuf.len() as u32).to_le_bytes();

        buf[4] = len[0];
        buf[5] = len[1];
        buf[6] = len[2];
        buf[7] = len[3];

        buf[8..(8 + fbuf.len())].clone_from_slice(fbuf);

        Ok(())
    }

    fn unserialize(&mut self, buf: &[u8]) -> ::std::result::Result<(), ()> {
        if buf[0..4] != *b"RSX1" {
            error!("Failed to load savestate: bad magic");
            return Err(());
        }

        let len = u32::from_le_bytes(*array_ref![buf, 4, 4]) as usize;

        let buf = &buf[8..(8 + len)];

        let fbr = match flexbuffers::Reader::get_root(buf) {
            Ok(r) => r,
            Err(e) => {
                error!("Failed to load savestate: {}", e);
                return Err(());
            }
        };

        if let Err(e) = self.psx.deserialize_and_load(fbr) {
            error!("Failed to load savestate: {}", e);
            return Err(());
        };

        libretro::Context::refresh_variables(self);

        Ok(())
    }

    fn get_num_images(&self) -> usize {
        self.images.len()
    }

    fn get_image_index(&self) -> usize {
        self.cur_image
    }

    fn get_image_path(&self, index: usize) -> Option<&Path> {
        self.images.get(index).map(|d| d.path())
    }

    fn is_disc_ejected(&self) -> bool {
        !self.psx.cd.disc_present()
    }

    fn eject_disc(&mut self) -> ::std::result::Result<(), ()> {
        self.psx.cd.eject_disc();

        Ok(())
    }

    fn insert_disc(&mut self) -> ::std::result::Result<(), ()> {
        let disc = match Context::load_image(self.cur_image()) {
            Ok(disc) => disc,
            Err(e) => {
                error!("Couldn't load {:?}: {}", self.cur_image().path(), e);
                return Err(());
            }
        };

        // Swap the memory cards if necessary since they depend on the disc name
        self.setup_memory_cards();

        self.psx.cd.load_disc(disc);

        Ok(())
    }

    fn set_image_index(&mut self, index: usize) -> ::std::result::Result<(), ()> {
        if index >= self.get_num_images() {
            // The libretro spec says that this can mean that we want to load no disc, but for us
            // there's not really any difference between no disc loaded and disc ejected, so I'm
            // not sure if it's worth handling this case
            error!("Ignoring set_image_index with an index out of range");
            return Err(());
        }

        if !self.is_disc_ejected() && index != self.cur_image {
            error!("Refusing to change the image index while the CD is not ejected");
            return Err(());
        }

        self.cur_image = index;

        Ok(())
    }

    fn get_image_label(&self, index: usize) -> Option<String> {
        self.images
            .get(index)
            .map(|img| img.basename().to_string_lossy().into_owned())
    }

    fn add_image_index(&mut self) -> ::std::result::Result<(), ()> {
        // Since we don't know what the new image will be at this point I just create a dummy entry
        let new = match self.images.first() {
            Some(i) => i.clone(),
            None => DiscImage::new(""),
        };

        self.images.push(new);

        Ok(())
    }

    fn replace_image_index(&mut self, index: usize, path: &Path) -> ::std::result::Result<(), ()> {
        if index == self.cur_image && !self.is_disc_ejected() {
            error!("Can't replace the image of the currently loaded disc!");
            return Err(());
        }

        match self.images.get_mut(index) {
            Some(i) => *i = DiscImage::new(path),
            None => {
                error!("Image index {} out of range", index);
                return Err(());
            }
        }

        Ok(())
    }
}

 */

/// Attempt to find a BIOS for `region` in the system directory
fn find_bios<F>(predicate: F, sys_dir: &Path) -> Result<Bios>
where
    F: Fn(&Metadata) -> bool,
{
    info!("Looking for a suitable BIOS in {:?}", sys_dir);

    let dir = ::std::fs::read_dir(&sys_dir)?;

    for entry in dir {
        match entry {
            Ok(entry) => {
                let path = entry.path();

                match entry.metadata() {
                    Ok(md) => {
                        if !md.is_file() {
                            debug!("Ignoring {:?}: not a file", path);
                        } else if md.len() != BIOS_SIZE as u64 {
                            debug!("Ignoring {:?}: bad size", path);
                        } else {
                            match try_bios(&predicate, &path) {
                                Ok(bios) => {
                                    info!("Using BIOS {:?} ({:?})", path, bios.metadata());
                                    return Ok(bios);
                                }
                                Err(e) => {
                                    warn!("Ignoring {:?}: {:?}", path, e);
                                }
                            }
                        }
                    }
                    Err(e) => warn!("Ignoring {:?}: can't get file metadata: {}", path, e),
                }
            }
            Err(e) => warn!("Error while reading directory: {}", e),
        }
    }

    error!("Couldn't find a suitable BIOS image");


    Err(PsxError::NoBiosFound)
}

/// Attempt to read and load the BIOS at `path`
fn try_bios<F>(predicate: F, path: &Path) -> Result<Bios>
where
    F: Fn(&Metadata) -> bool,
{
    let mut file = File::open(path)?;

    // Load the BIOS
    let mut rom = BoxArray::from_vec(vec![0; BIOS_SIZE]);

    file.read_exact(&mut *rom)?;

    let bios = Bios::new(rom)?;
    let md = bios.metadata();

    info!("Found BIOS DB entry for {:?}: {:?}", path, md);

    if md.known_bad {
        let m = format!("{:?}: known bad dump", path);
        Err(PsxError::BadBios(m))
    } else if !predicate(md) {
        let m = format!("{:?}: rejected by predicate", path);
        Err(PsxError::BadBios(m))
    } else {
        Ok(bios)
    }
}

/// Attempt to find the CDC firmware in the system directory
fn find_cdc_firmware(sys_dir: &Path) -> Result<BoxArray<u8, CDC_ROM_SIZE>> {
    info!(
        "Looking for a suitable CDC firmware in {:?}",
        sys_dir
    );

    let dir = ::std::fs::read_dir(&sys_dir)?;

    for entry in dir {
        match entry {
            Ok(entry) => {
                let path = entry.path();

                match entry.metadata() {
                    Ok(md) => {
                        if !md.is_file() {
                            debug!("Ignoring {:?}: not a file", path);
                        } else if md.len() != CDC_ROM_SIZE as u64 {
                            debug!("Ignoring {:?}: bad size", path);
                        } else {
                            let mut file = File::open(&path)?;

                            let mut rom = vec![0; CDC_ROM_SIZE];

                            file.read_exact(&mut rom)?;

                            let sha = sha256(&rom);

                            if sha != CDC_ROM_SHA256 {
                                warn!("Ignoring {:?}: invalid SHA256", path);
                            } else {
                                info!("Using CDC firmware {:?}", path);
                                return Ok(BoxArray::from_vec(rom));
                            }
                        }
                    }
                    Err(e) => warn!("Ignoring {:?}: can't get file metadata: {}", path, e),
                }
            }
            Err(e) => warn!("Error while reading directory: {}", e),
        }
    }

    error!("Couldn't find a suitable CDC firmware");

    Err(PsxError::NoCdcFirmwareFound)
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AnalogCombo {
    SelectR3,
    SelectL3,
    L3R3,
}

#[derive(serde::Serialize, serde::Deserialize, Copy, Clone, PartialEq, Eq, Debug, Default)]
#[repr(u8)]
pub enum VRamDisplayMode {
    #[default]
    Native,
    Full16bpp,
    Full8bpp,
    Full4bpp,
}

impl VRamDisplayMode {
    fn max_resolution(self) -> (u16, u16) {
        match self {
            // Maximum resolution supported by the PlayStation video output is 640x576. That high a
            // vertical resolution would mean no blanking however, so it doesn't make a lot of
            // sense.
            VRamDisplayMode::Native => (640, 480),
            VRamDisplayMode::Full16bpp => (1024, 512),
            VRamDisplayMode::Full8bpp => (2048, 512),
            VRamDisplayMode::Full4bpp => (4096, 512),
        }
    }

    fn aspect_ratio(self) -> f32 {
        match self {
            VRamDisplayMode::Native => 4. / 3.,
            VRamDisplayMode::Full16bpp => 2. / 1.,
            VRamDisplayMode::Full8bpp => 4. / 1.,
            VRamDisplayMode::Full4bpp => 8. / 1.,
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize, Copy, Clone, PartialEq, Eq, Debug, Default)]
#[repr(u8)]
pub enum CdOverlay {
    #[default]
    Disabled,
    Enabled,
    /// Overlay enabled only when CD is active
    Dynamic,
}

mod options {
    //! Core options

    use super::{AnalogCombo, CdOverlay, VRamDisplayMode};
    use std::str::FromStr;

    #[derive(PartialEq, Eq)]
    pub enum WireframeMode {
        Disabled,
        Overlay,
        Exclusive,
    }

    #[derive(PartialEq, Eq, Copy, Clone)]
    pub enum MemoryCardType {
        Disconnected,
        Common(u16),
        PerGame(u16),
    }

    /*
    libretro_variables!(
    pub struct CoreOptions (prefix = "rustation") {
        internal_upscale_factor: u8, parse_upscale
            => "Internal upscaling factor; 1x (native)|2x|4x";
        internal_color_depth: u8, parse_u8
            => "Internal color depth; dithered 15bpp (native)|24bpp";
        display_full_vram: VRamDisplayMode, parse_full_vram
            => "Display full VRAM; disabled|16bpp|8bpp|4bpp";
        force_transparency: bool, parse_bool
            => "Force transparency for all draw commands; disabled|enabled";
        wireframe: WireframeMode, parse_wireframe
            => "Draw wireframe for triangles and quads; disabled|overlay|wireframe only";
        gte_overclock: bool, parse_bool
            => "GTE overclock; disabled|enabled";
        cd_speed: u8, parse_u8
            => "CD Loading Speed; 2x (Native)|4x|6x|8x|10x|12x|14x";
        cd_overlay: CdOverlay, parse_cd_overlay
            => "CD overlay; disabled|enabled|dynamic";
        reverb_enable: bool, parse_bool
            => "Enable audio reverberation; enabled|disabled";
        analog_combo: AnalogCombo, parse_analog_combo
            => "Analog toggle button combo; \
            Select + R3|Select + L3|L3 + R3";
        analog_compensation: f32, parse_percent
            => "Analog deadzone compensation; \
            100%|105%|110%|115%|120%|130%|150%|50%|70%|80%|85%|90%|95%";
        memory_card_1_type: MemoryCardType, parse_memcard_index
            => "Memory card slot 1; \
            per-game.0|per-game.1|per-game.2|per-game.3|per-game.4|per-game.5|per-game.6|per-game.7|per-game.8|per-game.9|\
            per-game.10|per-game.11|per-game.12|per-game.13|per-game.14|per-game.15|per-game.16|per-game.17|per-game.18|per-game.19|\
            common.0|common.1|common.2|common.3|common.4|common.5|common.6|common.7|common.8|common.9|\
            common.10|common.11|common.12|common.13|common.14|common.15|common.16|common.17|common.18|common.19|\
            disconnected";
        memory_card_2_type: MemoryCardType, parse_memcard_index
            => "Memory card slot 2; \
            common.0|common.1|common.2|common.3|common.4|common.5|common.6|common.7|common.8|common.9|\
            common.10|common.11|common.12|common.13|common.14|common.15|common.16|common.17|common.18|common.19|\
            per-game.0|per-game.1|per-game.2|per-game.3|per-game.4|per-game.5|per-game.6|per-game.7|per-game.8|per-game.9|\
            per-game.10|per-game.11|per-game.12|per-game.13|per-game.14|per-game.15|per-game.16|per-game.17|per-game.18|per-game.19|\
            disconnected";
    });

     */

    fn parse_memcard_index(opt: &str) -> Result<MemoryCardType, ()> {
        if opt == "disconnected" {
            return Ok(MemoryCardType::Disconnected);
        }

        let mut s = opt.split('.');

        let ty = s.next().ok_or(())?;
        let index = s.next().ok_or(())?;

        let index: u16 = index.parse().map_err(|_| ())?;

        match ty {
            "per-game" => Ok(MemoryCardType::PerGame(index)),
            "common" => Ok(MemoryCardType::Common(index)),
            _ => {
                error!("Unkown memory card type {}", ty);
                Err(())
            }
        }
    }

    fn parse_upscale(opt: &str) -> Result<u8, <u8 as FromStr>::Err> {
        let num = opt.trim_matches(|c: char| !c.is_numeric());

        let n: u8 = num.parse()?;

        Ok(n.ilog2() as u8)
    }

    fn parse_percent(opt: &str) -> Result<f32, <u32 as FromStr>::Err> {
        let num = opt.trim_matches(|c: char| !c.is_numeric());

        let percent: u32 = num.parse()?;

        Ok((percent as f32) / 100.)
    }

    fn parse_u8(opt: &str) -> Result<u8, <u8 as FromStr>::Err> {
        let num = opt.trim_matches(|c: char| !c.is_numeric());

        num.parse()
    }

    fn parse_bool(opt: &str) -> Result<bool, ()> {
        match opt {
            "true" | "enabled" | "on" => Ok(true),
            "false" | "disabled" | "off" => Ok(false),
            _ => Err(()),
        }
    }

    fn parse_full_vram(opt: &str) -> Result<VRamDisplayMode, ()> {
        let mode = match opt {
            "disabled" => VRamDisplayMode::Native,
            "16bpp" => VRamDisplayMode::Full16bpp,
            "8bpp" => VRamDisplayMode::Full8bpp,
            "4bpp" => VRamDisplayMode::Full4bpp,
            _ => return Err(()),
        };

        Ok(mode)
    }

    fn parse_cd_overlay(opt: &str) -> Result<CdOverlay, ()> {
        let mode = match opt {
            "disabled" => CdOverlay::Disabled,
            "enabled" => CdOverlay::Enabled,
            "dynamic" => CdOverlay::Dynamic,
            _ => return Err(()),
        };

        Ok(mode)
    }

    fn parse_analog_combo(opt: &str) -> Result<AnalogCombo, ()> {
        let combo = match opt {
            "Select + R3" => AnalogCombo::SelectR3,
            "Select + L3" => AnalogCombo::SelectL3,
            "L3 + R3" => AnalogCombo::L3R3,
            _ => return Err(()),
        };

        Ok(combo)
    }

    fn parse_wireframe(opt: &str) -> Result<WireframeMode, ()> {
        let mode = match opt {
            "disabled" => WireframeMode::Disabled,
            "overlay" => WireframeMode::Overlay,
            "wireframe only" => WireframeMode::Exclusive,
            _ => return Err(()),
        };

        Ok(mode)
    }
}

fn init() {
    /*
    retrolog::init();

    if !libretro::set_pixel_format(libretro::PixelFormat::Xrgb8888) {
        error!("Can't set pixel format to XRGB 8888!");
    }
     */
}

/*
fn init_variables() {
    options::CoreOptions::register();
}

*/

/// Called when a game is loaded and a new context must be built
fn load_game(disc: PathBuf) -> Option<Box<Context>> {
    todo!();
    /*
    info!("Loading {:?}", disc);

    match Context::new(&disc) {
        Ok(ctx) => Some(Box::new(ctx)),
        Err(e) => {
            error!("Couldn't build context: {}", e);
            None
        }
    }
    */
}

/// A loadable disc image stored somewhere on disc
#[derive(Clone)]
struct DiscImage {
    path: PathBuf,
}

impl DiscImage {
    fn new<P: AsRef<Path>>(path: P) -> DiscImage {
        let path = path.as_ref().to_path_buf();

        DiscImage { path }
    }

    fn path(&self) -> &Path {
        self.path.as_ref()
    }

    fn basename(&self) -> &OsStr {
        self.path.file_stem().expect("Couldn't get disc image stem")
    }
}

/*
/// Libretro to PlayStation button mapping. Libretro's mapping is based on the SNES controller so
/// libretro's A button matches the PlayStation's Circle button.
static BUTTON_MAP: [(libretro::JoyPadButton, Button); 16] = [
    (libretro::JoyPadButton::Up, Button::DUp),
    (libretro::JoyPadButton::Down, Button::DDown),
    (libretro::JoyPadButton::Left, Button::DLeft),
    (libretro::JoyPadButton::Right, Button::DRight),
    (libretro::JoyPadButton::Start, Button::Start),
    (libretro::JoyPadButton::Select, Button::Select),
    (libretro::JoyPadButton::A, Button::Circle),
    (libretro::JoyPadButton::B, Button::Cross),
    (libretro::JoyPadButton::Y, Button::Square),
    (libretro::JoyPadButton::X, Button::Triangle),
    (libretro::JoyPadButton::L, Button::L1),
    (libretro::JoyPadButton::R, Button::R1),
    (libretro::JoyPadButton::L2, Button::L2),
    (libretro::JoyPadButton::R2, Button::R2),
    (libretro::JoyPadButton::L3, Button::L3),
    (libretro::JoyPadButton::R3, Button::R3),
];

static INPUT_DESCRIPTORS: [libretro::InputDescriptor; 41] = [
    // Port 0
    libretro::InputDescriptor::joypad_button(
        0,
        libretro::JoyPadButton::Left,
        cstring!("D-Pad Left"),
    ),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::Up, cstring!("D-Pad Up")),
    libretro::InputDescriptor::joypad_button(
        0,
        libretro::JoyPadButton::Down,
        cstring!("D-Pad Down"),
    ),
    libretro::InputDescriptor::joypad_button(
        0,
        libretro::JoyPadButton::Right,
        cstring!("D-Pad Right"),
    ),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::B, cstring!("Cross")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::A, cstring!("Circle")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::X, cstring!("Triangle")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::Y, cstring!("Square")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::L, cstring!("L1")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::L2, cstring!("L2")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::L3, cstring!("L3")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::R, cstring!("R1")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::R2, cstring!("R2")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::R3, cstring!("R3")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::Select, cstring!("Select")),
    libretro::InputDescriptor::joypad_button(0, libretro::JoyPadButton::Start, cstring!("Start")),
    libretro::InputDescriptor::analog_axis(
        0,
        libretro::AnalogInput::Left,
        libretro::AnalogAxis::X,
        cstring!("Left Analog X"),
    ),
    libretro::InputDescriptor::analog_axis(
        0,
        libretro::AnalogInput::Left,
        libretro::AnalogAxis::Y,
        cstring!("Left Analog Y"),
    ),
    libretro::InputDescriptor::analog_axis(
        0,
        libretro::AnalogInput::Right,
        libretro::AnalogAxis::X,
        cstring!("Right Analog X"),
    ),
    libretro::InputDescriptor::analog_axis(
        0,
        libretro::AnalogInput::Right,
        libretro::AnalogAxis::Y,
        cstring!("Right Analog Y"),
    ),
    // Port 1
    libretro::InputDescriptor::joypad_button(
        1,
        libretro::JoyPadButton::Left,
        cstring!("D-Pad Left"),
    ),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::Up, cstring!("D-Pad Up")),
    libretro::InputDescriptor::joypad_button(
        1,
        libretro::JoyPadButton::Down,
        cstring!("D-Pad Down"),
    ),
    libretro::InputDescriptor::joypad_button(
        1,
        libretro::JoyPadButton::Right,
        cstring!("D-Pad Right"),
    ),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::B, cstring!("Cross")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::A, cstring!("Circle")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::X, cstring!("Triangle")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::Y, cstring!("Square")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::L, cstring!("L1")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::L2, cstring!("L2")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::L3, cstring!("L3")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::R, cstring!("R1")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::R2, cstring!("R2")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::R3, cstring!("R3")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::Select, cstring!("Select")),
    libretro::InputDescriptor::joypad_button(1, libretro::JoyPadButton::Start, cstring!("Start")),
    libretro::InputDescriptor::analog_axis(
        1,
        libretro::AnalogInput::Left,
        libretro::AnalogAxis::X,
        cstring!("Left Analog X"),
    ),
    libretro::InputDescriptor::analog_axis(
        1,
        libretro::AnalogInput::Left,
        libretro::AnalogAxis::Y,
        cstring!("Left Analog Y"),
    ),
    libretro::InputDescriptor::analog_axis(
        1,
        libretro::AnalogInput::Right,
        libretro::AnalogAxis::X,
        cstring!("Right Analog X"),
    ),
    libretro::InputDescriptor::analog_axis(
        1,
        libretro::AnalogInput::Right,
        libretro::AnalogAxis::Y,
        cstring!("Right Analog Y"),
    ),
    // End of table
    libretro::InputDescriptor::end_of_table(),
];

/// Standard, digital-only controller (SCPH-1080)
const PSX_CONTROLLER_DIGITAL: libc::c_uint = libretro::InputDevice::JoyPad.subclass(0);
/// DualShock analog controller (SCPH-1200)
const PSX_CONTROLLER_DUALSHOCK: libc::c_uint = libretro::InputDevice::Analog.subclass(0);

/// All supported controller types
static CONTROLLER_DESCRIPTIONS: [libretro::ControllerDescription; 2] = [
    libretro::ControllerDescription {
        desc: cstring!("PlayStation Digital Controller"),
        id: PSX_CONTROLLER_DIGITAL,
    },
    libretro::ControllerDescription {
        desc: cstring!("PlayStation DualShock Analog Controller"),
        id: PSX_CONTROLLER_DUALSHOCK,
    },
];

/// Controller settings for two player mode (no multitap)
static CONTROLLER_INFO_2P: [libretro::ControllerInfo; 3] = [
    // Player 1
    libretro::ControllerInfo {
        types: &CONTROLLER_DESCRIPTIONS as *const _,
        num_types: CONTROLLER_DESCRIPTIONS.len() as _,
    },
    // Player 2
    libretro::ControllerInfo {
        types: &CONTROLLER_DESCRIPTIONS as *const _,
        num_types: CONTROLLER_DESCRIPTIONS.len() as _,
    },
    // End of table marker
    libretro::ControllerInfo::end_of_table(),
];

fn init_controllers() {
    libretro::set_controller_info(&CONTROLLER_INFO_2P);
    libretro::set_input_descriptors(&INPUT_DESCRIPTORS);
}


 */