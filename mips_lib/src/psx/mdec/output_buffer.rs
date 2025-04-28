/// Structure containing a fully decoded block before it's put in the output FIFO
#[derive(serde::Serialize, serde::Deserialize)]
pub struct OutputBuffer {
    /// Maximum size of a decoded block is 192bytes for a 64x64 24bpp block
    #[serde(with = "serde_big_array::BigArray")]
    buf: [u8; 192],
    write_idx: u8,
    read_idx: u8,
}

impl OutputBuffer {
    pub fn new() -> OutputBuffer {
        OutputBuffer {
            buf: [0; 192],
            write_idx: 0,
            read_idx: 0,
        }
    }

    pub fn clear(&mut self) {
        self.write_idx = 0;
        self.read_idx = 0;
    }

    pub fn is_empty(&mut self) -> bool {
        self.write_idx == self.read_idx
    }

    pub(crate) fn push_byte(&mut self, b: u8) {
        self.buf[usize::from(self.write_idx)] = b;
        self.write_idx += 1;
    }

    pub(crate) fn push_halfword(&mut self, v: u16) {
        self.push_byte(v as u8);
        self.push_byte((v >> 8) as u8);
    }

    fn pop_byte(&mut self) -> u8 {
        debug_assert!(!self.is_empty());

        let b = self.buf[usize::from(self.read_idx)];

        self.read_idx += 1;

        b
    }

    pub(crate) fn pop_word(&mut self) -> u32 {
        let b0 = self.pop_byte() as u32;
        let b1 = self.pop_byte() as u32;
        let b2 = self.pop_byte() as u32;
        let b3 = self.pop_byte() as u32;

        b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
    }
}
