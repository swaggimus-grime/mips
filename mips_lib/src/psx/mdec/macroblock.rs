use std::ops::{Index, IndexMut};

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Macroblock {
    #[serde(with = "serde_big_array::BigArray")]
    pub(crate) block: [i8; 8 * 8],
}

impl Macroblock {
    pub fn new() -> Macroblock {
        Macroblock { block: [0; 8 * 8] }
    }
}

impl Index<usize> for Macroblock {
    type Output = i8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.block[index]
    }
}

impl IndexMut<usize> for Macroblock {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.block[index]
    }
}
/// Coefficients during macroblock decoding
#[derive(serde::Serialize, serde::Deserialize)]
pub struct MacroblockCoeffs {
    #[serde(with = "serde_big_array::BigArray")]
    pub(crate) coeffs: [i16; 8 * 8],
}

impl MacroblockCoeffs {
    pub fn new() -> MacroblockCoeffs {
        MacroblockCoeffs { coeffs: [0; 8 * 8] }
    }

    /// RLE-encoded values are encoded using a "zigzag" pattern in
    /// order to maximise the number of consecutive zeroes.
    pub(crate) fn set_zigzag(&mut self, pos: u8, coeff: i16) {
        // Zigzag LUT
        let zigzag: [u8; 64] = [
            0x00, 0x08, 0x01, 0x02, 0x09, 0x10, 0x18, 0x11, 0x0a, 0x03, 0x04, 0x0b, 0x12, 0x19,
            0x20, 0x28, 0x21, 0x1a, 0x13, 0x0c, 0x05, 0x06, 0x0d, 0x14, 0x1b, 0x22, 0x29, 0x30,
            0x38, 0x31, 0x2a, 0x23, 0x1c, 0x15, 0x0e, 0x07, 0x0f, 0x16, 0x1d, 0x24, 0x2b, 0x32,
            0x39, 0x3a, 0x33, 0x2c, 0x25, 0x1e, 0x17, 0x1f, 0x26, 0x2d, 0x34, 0x3b, 0x3c, 0x35,
            0x2e, 0x27, 0x2f, 0x36, 0x3d, 0x3e, 0x37, 0x3f,
        ];

        if pos >= 64 {
            // XXX Not sure how the MDEC deals with index
            // overflows. Does it wrap around somehow? Does it move to
            // the next block?
            panic!("Block index overflow!");
        }

        let index = zigzag[pos as usize];

        self.coeffs[index as usize] = coeff
    }
}

impl Index<usize> for MacroblockCoeffs {
    type Output = i16;

    fn index(&self, index: usize) -> &Self::Output {
        &self.coeffs[index]
    }
}

impl IndexMut<usize> for MacroblockCoeffs {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.coeffs[index]
    }
}
