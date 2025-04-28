extern crate core;

mod psx;
mod error;
mod util;
mod hash;
mod settings;
mod mem_card;
mod bitwise;

use std::ops::DerefMut;
use util::fs::sys_dir::{SysDir, SearchFor};
use util::fs::file::bin;
use std::path::Path;
use cdimage::cue::Cue;
use log::{error, info};
use psx::bus::Bus;
use error::{MipsResult, MipsError};
use crate::mem_card::MemoryCardFile;
use crate::psx::bios::bios::{Bios, BIOS_SIZE};
use crate::psx::cd::CDC_ROM_SIZE;
use crate::psx::cd::disc::Disc;
use crate::psx::exe::Exe;
use crate::psx::graphics::rasterizer::handle::Frame;
use crate::settings::mips::MipsSettings;
use crate::util::ds::box_slice::BoxSlice;

pub struct Mips {
    bus: Box<Bus>,
    settings: MipsSettings,
    /// Objects used to deal with reading/storing the memory card images to files
    memcard_files: BoxSlice<MemoryCardFile, 2>,
    sys_dir: SysDir
}

impl Mips {
    pub fn new(sys_dir: &Path, game_path: Option<&str>) -> MipsResult<Box<Mips>> {
        let sys_dir = SysDir::new(sys_dir);
        
        let mut cdc_firmware = {
            let cdc_firmware_path = sys_dir.search(SearchFor::CdcFirmware)?;
            open_cdc_firmware(cdc_firmware_path.as_path())?
        };
        
        //let test_exe = {
        //    let exe_path = sys_dir.search(SearchFor::Executables)?;
        //    let test_exe_path = exe_path.join("psxtest_cpu.exe");
        //    open_exe(test_exe_path.as_path())?
        //};

        let bios = {
            let bios_path = sys_dir.search(SearchFor::Bios)?;
            open_bios(bios_path.as_path())?
        };

        let disc = {
            match game_path {
                Some(game_path) => {
                    let games_path = sys_dir.search(SearchFor::Games)?;
                    let disc_path = games_path.join(game_path);
                    Some(open_disc(disc_path.as_path())?)
                },
                None => None
            }
        };
        
        Ok(Box::new(Mips {
            bus: Box::new(Bus::new(bios, *cdc_firmware, disc)?),
            settings: MipsSettings::default(),
            memcard_files: BoxSlice::from_vec(vec![MemoryCardFile::dummy(), MemoryCardFile::dummy()]),
            sys_dir
        }))
    }
    
    pub fn update(&mut self) {
        self.bus.update();
    }

    pub fn output_frame(&mut self) -> Option<Frame> {
        self.bus.take_frame()
    }
    
    pub fn output_audio_samples(&mut self) -> &[i16] {
        self.bus.get_audio_samples()
    }
    
    pub fn clear_audio_samples(&mut self) {
        self.bus.clear_audio_samples()
    }
    
    pub fn insert_disc(&mut self, disc_path: &str) -> MipsResult<()> {
        let disc = {
            let games_path = self.sys_dir.search(SearchFor::Games)?;
            let disc_path = games_path.join(disc_path);
            open_disc(disc_path.as_path())?
        };
        
        self.bus.insert_disc(disc);
        Ok(())
    }

    pub fn poll_mem_cards(&mut self) {
        let mut memory_cards = self.bus.pad_memcard.memory_cards_mut();
        for (file, mc) in self.memcard_files.iter_mut().zip(memory_cards.iter_mut()) {
            let device = mc.device_mut();

            device.new_frame();
            file.maybe_dump(device);
        }
    }

    pub fn poll_gamepads(&mut self) {
        // Refresh pads
        let mut gamepads = self.bus.pad_memcard.gamepads_mut();
        for gp in gamepads.iter_mut() {
            let device = gp.device_mut();

            device.new_frame();
        }
    }
}

fn open_bios(bios_path: &Path) -> MipsResult<Bios> {
    let rom = bin::from_file(bios_path)?;
    let bios = Bios::new(rom)?;
    Ok(bios)
}

/// Attempt to find the CDC firmware in the system directory
fn open_cdc_firmware(cdc_firmware_path: &Path) -> MipsResult<BoxSlice<u8, CDC_ROM_SIZE>> {
    let rom = bin::from_file(cdc_firmware_path)?;
    Ok(rom)
}

fn open_disc(disc_path: &Path) -> MipsResult<Disc> {
    let path = disc_path;

    let disc = if path.extension().and_then(|ext| ext.to_str()) == Some("cue") {
        Cue::new(path)
    } else {
        Cue::new_from_zip(path)
    }.unwrap();

    let disc = Disc::new(Box::new(disc))?;

    let serial = disc.serial_number();
    let region = disc.region();

    info!("Disc serial number: {}", serial);
    info!("Detected disc region: {:?}", region);

    Ok(disc)
}

fn open_exe(path: &Path) -> MipsResult<Exe> {
    let exe = Exe::new(path);
    
    exe
}