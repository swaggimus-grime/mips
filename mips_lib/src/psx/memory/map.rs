//! PlayStation memory map

/// Mask array used to strip the region bits of the address. The mask is selected using the 3
/// MSBs of the address so each entry effectively matches 512kB of the address space. KSEG2 is
/// not touched since it doesn't share anything with the other regions.
const REGION_MASK: [u32; 8] = [
    // KUSEG: 2048MB
    0xffff_ffff,
    0xffff_ffff,
    0xffff_ffff,
    0xffff_ffff,
    // KSEG0:  512MB
    0x7fff_ffff,
    // KSEG1:  512MB
    0x1fff_ffff,
    // KSEG2: 1024MB
    0xffff_ffff,
    0xffff_ffff,
];

/// Mask a CPU address to remove the region bits.
pub fn mask_region(addr: u32) -> u32 {
    // Index address space in 512MB chunks
    let index = (addr >> 29) as usize;

    addr & REGION_MASK[index]
}

pub struct Range(pub u32, u32);

impl Range {
    /// Return `Some(offset)` if addr is contained in `self`
    pub fn contains(self, addr: u32) -> Option<u32> {
        let Range(start, length) = self;

        if addr >= start && addr < start + length {
            Some(addr - start)
        } else {
            None
        }
    }
}

/// Main RAM: 2MB mirrored four times over the first 8MB
pub const RAM: Range = Range(0x0000_0000, 8 * 1024 * 1024);

/// Expansion region 1
pub const EXPANSION_1: Range = Range(0x1f00_0000, 512 * 1024);

/// BIOS ROM. Read-only, significantly slower to access than system RAM
pub const BIOS: Range = Range(0x1fc0_0000, 512 * 1024);

/// ScratchPad: data cache used as a fast 1kB RAM
pub const SCRATCH_PAD: Range = Range(0x1f80_0000, 1024);

/// Memory latency and expansion mapping
pub const MEM_CONTROL: Range = Range(0x1f80_1000, 36);

/// Gamepad and memory card controller
pub const PAD_MEMCARD: Range = Range(0x1f80_1040, 32);

/// Register that has something to do with RAM configuration, configured by the BIOS
pub const RAM_SIZE: Range = Range(0x1f80_1060, 4);

/// Interrupt Control registers (status and mask)
pub const IRQ_CONTROL: Range = Range(0x1f80_1070, 8);

/// Direct Memory Access registers
pub const DMA: Range = Range(0x1f80_1080, 0x80);

/// Timer registers
pub const TIMERS: Range = Range(0x1f80_1100, 0x30);

/// CDROM controller
pub const CDROM: Range = Range(0x1f80_1800, 4);

/// GPU Registers
pub const GPU: Range = Range(0x1f80_1810, 8);

/// MDEC registers
pub const MDEC: Range = Range(0x1f80_1820, 8);

/// SPU registers
pub const SPU: Range = Range(0x1f80_1c00, 640);

/// Expansion region 2
pub const EXPANSION_2: Range = Range(0x1f80_2000, 66);

/// Cache control register. Full address since it's in KSEG2
pub const CACHE_CONTROL: Range = Range(0xfffe_0130, 4);