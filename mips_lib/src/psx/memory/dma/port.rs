use crate::psx::bus::Bus;
use crate::psx::graphics::gpu;
use crate::psx::{cd, mdec};
use crate::psx::processor::ClockCycle;
use crate::psx::sound::spu;

/// The 7 DMA channels
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Port {
    /// Macroblock decoder input
    MDecIn = 0,
    /// Macroblock decoder output
    MDecOut = 1,
    /// Graphics Processing Unit
    Gpu = 2,
    /// CD-ROM drive
    CdRom = 3,
    /// Sound Processing Unit
    Spu = 4,
    /// Extension port
    Pio = 5,
    /// Used to clear the ordering table in RAM
    Otc = 6,
}

impl From<u32> for Port {
    fn from(val: u32) -> Self {
        match val {
            0 => Port::MDecIn,
            1 => Port::MDecOut,
            2 => Port::Gpu,
            3 => Port::CdRom,
            4 => Port::Spu,
            5 => Port::Pio,
            6 => Port::Otc,
            n => panic!("Invalid DMA channel {}", n),
        }
    }
}

/// Perform a DMA port write. Returns the overhead of the write
pub fn store(bus: &mut Bus, port: Port, v: u32) -> ClockCycle {
    match port {
        Port::Spu => {
            spu::dma_store(bus, v);
            // XXX Mednafen has a long comment explaining where this value comes from (and mention
            // that the average should be closer to 96). This is of course a wildly inaccurate
            // approximation but let's not worry about that for the time being.
            47
        }
        Port::Gpu => {
            gpu::dma_store(bus, v);
            0
        }
        Port::MDecIn => {
            mdec::dma_store(bus, v);
            0
        }
        _ => unimplemented!("DMA port store {:?}", port),
    }
}

/// Perform a DMA port read and returns the value alongside with the write offset (for MDEC, 0
/// elsewhere) and the delay penalty for the read
pub fn load(bus: &mut Bus, port: Port) -> (u32, u32, ClockCycle) {
    let mut offset = 0;
    let mut delay = 0;

    let v = match port {
        Port::Otc => {
            let channel = &bus.dma[port];

            if channel.remaining_words == 1 {
                // Last entry contains the end of table marker
                0xff_ffff
            } else {
                // Pointer to the previous entry
                channel.cur_address.wrapping_sub(4) & 0x1f_ffff
            }
        }
        // XXX latency taken from mednafen
        Port::CdRom => {
            delay = 8;
            cd::dma_load(bus)
        }
        Port::Spu => spu::dma_load(bus),
        Port::MDecOut => {
            let (v, off) = mdec::dma_load(bus);
            offset = off;
            v
        }
        Port::Gpu => gpu::dma_load(bus),
        _ => unimplemented!("DMA port load {:?}", port),
    };

    (v, offset, delay)
}

