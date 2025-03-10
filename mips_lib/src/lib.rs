extern crate core;

mod psx;
mod error;
mod util;
mod hash;

use util::fs::sys_dir::{SysDir, SearchFor};
use util::fs::file::bin;
use std::path::Path;
use log::info;
use psx::bus::Bus;
use error::{Result, MipsError};
use crate::psx::bios::bios::{Bios, BIOS_SIZE};

pub struct Mips {
    bus: Bus
}

impl Mips {
    pub fn new(sys_dir: &Path) -> Result<Mips> {
        let sys_dir = SysDir::new(sys_dir);
        let bios_path = sys_dir.search(SearchFor::Bios)?;
        let bios = open_bios(bios_path.as_path())?;
        
        Ok(Mips {
            bus: Bus::new(bios),
        })
    }
    
    pub fn update(&mut self) {
        self.bus.update();
    }
}

fn open_bios(bios_path: &Path) -> Result<Bios> {
    let mut rom = bin::from_file(bios_path)?;
    let bios = Bios::new(rom)?;
    let md = bios.metadata();
    Ok(bios)
}
