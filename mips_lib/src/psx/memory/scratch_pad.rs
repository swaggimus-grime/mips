use crate::psx::addressable::Addressable;

#[derive(serde::Serialize, serde::Deserialize)]
pub(crate) struct ScratchPad {
    #[serde(with = "serde_big_array::BigArray")]
    data: [u8; SCRATCH_PAD_SIZE],
}

impl ScratchPad {
    /// Instantiate Scratch Pad
    pub fn new() -> ScratchPad {
        ScratchPad {data:[0; SCRATCH_PAD_SIZE]}
    }

    /// Fetch the little endian value at `offset`
    pub fn load<T: Addressable>(&self, offset: u32) -> T {
        // The two MSBs are ignored, the 2MB RAM is mirrored four times over the first 8MB of
        // address space
        let offset = (offset & 0x1f_ffff) as usize;

        let mut v = 0;

        for i in 0..T::width() as usize {
            let b = u32::from(self.data[offset + i]);

            v |= b << (i * 8)
        }

        Addressable::from_u32(v)
    }

    /// Store the 32bit little endian word `val` into `offset`
    pub fn store<T: Addressable>(&mut self, offset: u32, val: T) {
        // The two MSBs are ignored, the 2MB RAM is mirrored four times over the first 8MB of
        // address space
        let offset = (offset & 0x1f_ffff) as usize;

        let val = val.as_u32();

        for i in 0..T::width() as usize {
            self.data[offset + i] = (val >> (i * 8)) as u8;
        }
    }
}

/// Scratch Pad (data cache): 1KB
const SCRATCH_PAD_SIZE: usize = 1024;