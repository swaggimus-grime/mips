use crate::error::*;
use crate::psx::addressable::{AccessWidth, Addressable};
use crate::psx::bios::metadata::{lookup_blob, Metadata};
use crate::psx::ioable::Loadable;
use crate::util::ds::box_slice::*;

pub struct Bios {
    rom: BoxSlice<u8, BIOS_SIZE>,
    metadata: &'static Metadata,
}

impl Bios {
    /// Create a BIOS image from `binary` and attempt to match it with an entry in the database. If
    /// no match can be found return an error.
    pub fn new(rom: BoxSlice<u8, BIOS_SIZE>) -> Result<Bios> {
        match lookup_blob(&rom) {
            Some(metadata) => Ok(Bios {
                rom,
                metadata,
            }),
            None => Err(MipsError::UnknownBios),
        }
    }

    /// Return a static pointer to the BIOS's Metadata
    pub fn metadata(&self) -> &'static Metadata {
        self.metadata
    }
    
    /// Return the raw BIOS ROM
    pub fn rom(&self) -> &[u8; BIOS_SIZE] {
        &self.rom
    }
}

impl Loadable<u8> for Bios {
    fn load<const N: usize>(&self, offset: usize) -> [u8; N] {
        let bytes = &self.rom.as_slice()[offset..offset + N];
        let mut r: [u8; N] = [0; N];
        r.clone_from_slice(bytes);
        return r;
    }
}

impl Loadable<u32> for Bios {
    fn load<const N: usize>(&self, offset: usize) -> [u32; N] {
        let size = u32::size();
        let bytes = &self.rom.as_slice()[offset..offset + N * size];
        let mut r: [u32; N] = [0; N];
        for i in 0..N {
            let i_unit = i * size;
            let b0 = bytes[i_unit] as u32;
            let b1 = bytes[i_unit + 1] as u32;
            let b2 = bytes[i_unit + 2] as u32;
            let b3 = bytes[i_unit + 3] as u32;
            r[i] = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
        }
        return r;
    }
}

impl Loadable<u16> for Bios {
    fn load<const N: usize>(&self, offset: usize) -> [u16; N] {
        todo!();
    }
}

/// 512 KB
pub const BIOS_SIZE: usize = 512 * 1024;
