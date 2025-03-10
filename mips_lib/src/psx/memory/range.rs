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

pub struct Range(u32, u32);

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

/// BIOS ROM. Read-only, significantly slower to access than system RAM
pub const BIOS: Range = Range(0x1fc0_0000, 512 * 1024);

pub const EXPANSION_2: Range = Range(0x1f80_2000, 1);

