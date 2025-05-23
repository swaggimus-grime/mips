mod cdc;
pub mod disc;
pub mod iso9660;

use std::ops::{Deref, DerefMut};
pub use cdc::CdcState;
pub use cdc::MC68HC05_ROM_DUMP_SIZE as CDC_ROM_SIZE;
use cdimage::DiscPosition;
use log::info;
use disc::Disc;
use crate::error::{MipsError, MipsResult};
use crate::hash::sha::sha256;
use crate::psx::addressable::Addressable;
use crate::psx::bus::Bus;
use crate::psx::cd;
use crate::psx::processor::irq;
use crate::util::ds::box_slice::BoxSlice;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct CdInterface {
    pub cdc: Box<cdc::Cdc>,
    /// Counter to prevent overclocking the CDC when the MDEC is active (since it's probably
    /// streaming data from the CD)
    mdec_busy_cooldown: u16,
}

impl CdInterface {
    pub fn new(disc: Option<Disc>, mut cdc_rom: [u8; cd::CDC_ROM_SIZE]) -> MipsResult<CdInterface> {
        if !cfg!(test) {
            // Check that we get the expected firmware. Not all CDC firmware versions will be
            // compatible with this code since there have been significant changes between
            // revisions of the Bus hardware (a PSOne firmware almost certainly wouldn't work
            // without tweaks for instance). As such for now I only support one single ROM from the
            // SCPH-5502 (PAL) hardware and patch it below for other regions.
            let sha = sha256(&cdc_rom);

            if sha != CDC_ROM_SHA256 {
                return Err(MipsError::BadCdcFirmware);
            }
        }

        let region = disc
            .as_ref()
            .map(|d| d.region())
            .unwrap_or(disc::Region::NorthAmerica);

        if region != disc::Region::Europe {
            info!("Patching CDC firmware for {:?}", region);

            // Patch the expected license string: SCEE for Europe (default in this ROM, so no
            // change), SCEI for Japan, SCEA for America
            cdc_rom[0x3ca4] = match region {
                disc::Region::Europe => b'E',
                disc::Region::Japan => b'I',
                disc::Region::NorthAmerica => b'A',
            };
        }

        let cdc = cdc::Cdc::new(&cdc_rom, disc);

        Ok(CdInterface {
            cdc: Box::new(cdc),
            mdec_busy_cooldown: 0,
        })
    }

    pub fn set_cd_loading_speed(&mut self, loading_speed: u8) {
        self.cdc.set_cd_loading_speed(loading_speed);
    }

    pub fn disc_present(&self) -> bool {
        self.cdc.disc_present()
    }

    pub fn eject_disc(&mut self) -> Option<Disc> {
        self.cdc.take_disc()
    }

    pub fn load_disc(&mut self, disc: Disc) {
        self.cdc.load_disc(disc)
    }

    pub fn state(&self) -> CdcState {
        self.cdc.state()
    }

    pub fn disc_speed(&self) -> u8 {
        self.cdc.disc_speed()
    }

    pub fn sled_position(&self) -> DiscPosition {
        self.cdc.position()
    }
}

/// Called by the DMA when it wants to get our CD data
pub fn dma_load(bus: &mut Bus) -> u32 {
    // We read 4 bytes at a time
    let b0 = u32::from(bus.cd.cdc.host_dma_read());
    let b1 = u32::from(bus.cd.cdc.host_dma_read());
    let b2 = u32::from(bus.cd.cdc.host_dma_read());
    let b3 = u32::from(bus.cd.cdc.host_dma_read());

    // Pack in a little endian word
    b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
}

pub fn run_audio_cycle(bus: &mut Bus) -> [i16; 2] {
    if bus.mdec.is_busy() {
        // Prevent overclocking for a quarter of a second
        bus.cd.mdec_busy_cooldown = 44_100 / 4;
    } else if bus.cd.mdec_busy_cooldown > 0 {
        bus.cd.mdec_busy_cooldown -= 1;
    }

    let sample = bus.cd.cdc.run_audio_cycle(bus.cd.mdec_busy_cooldown == 0);
    refresh_irq(bus);

    sample
}

pub fn store<T: Addressable>(bus: &mut Bus, off: u32, val: T) {
    let v = val.as_u8();
    let off = off as u8;

    bus.cd.cdc.host_write(off, v);

    refresh_irq(bus);
}

pub fn load<T: Addressable>(bus: &mut Bus, off: u32) -> T {
    let off = off as u8;

    let v = bus.cd.cdc.host_read(off);

    T::from_u32(u32::from(v))
}

fn refresh_irq(bus: &mut Bus) {
    irq::set_level(bus, irq::Interrupt::CdRom, bus.cd.cdc.irq_active());
}

/// This is the SHA256 for the firmware we tested with, `scph-5502_SC430939.bin`.
///
/// It's a BIOS for european systems but we can hotpatch it for other regions.
///
/// The image format is the one used by No$ in his dumps, look at the documentation of
/// `cdc::mc64hc05::ROM_DUMP_SIZE` for more info
pub const CDC_ROM_SHA256: [u8; 32] = [
    0xbf, 0x59, 0x0f, 0xbf, 0x60, 0x55, 0xf4, 0x28, 0x13, 0x85, 0x10, 0xb2, 0x6a, 0x2f, 0x20, 0x06,
    0xb7, 0xea, 0xb5, 0x4e, 0xad, 0x48, 0xc1, 0xdd, 0xb1, 0xa1, 0xa5, 0xd2, 0x69, 0x92, 0x42, 0xdb,
];

