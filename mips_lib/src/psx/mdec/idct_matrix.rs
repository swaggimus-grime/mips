use std::ops::{Index, IndexMut};
use crate::psx::mdec::macroblock::{Macroblock, MacroblockCoeffs};

#[derive(serde::Serialize, serde::Deserialize)]
pub struct IdctMatrix {
    #[serde(with = "serde_big_array::BigArray")]
    matrix: [i16; 64],
}

impl IdctMatrix {
    pub fn new() -> IdctMatrix {
        IdctMatrix { matrix: [0; 64] }
    }

    pub(crate) fn set(&mut self, index: u8, coef: i16) {
        // We shuffle the table to make it a bit more cache friendly in `idct` as well as making it
        // easier to implement it using SIMD
        let index = ((index & 7) << 3) | ((index >> 3) & 7);
        self[index as usize] = coef;
    }

    /// Compute the Inverse Discrete Cosine Transform of `coeffs` and store the result in `block`
    pub(crate) fn idct(&self, coeffs: &MacroblockCoeffs, block: &mut Macroblock) {
        // XXX This function could greatly benefit from SIMD code when Rust supports it. The full
        // IDCT takes 1024 multiplications.
        let mut block_tmp = [0i16; 8 * 8];

        // First pass, store intermediate results in `block_tmp`
        for y in 0..8 {
            for x in 0..8 {
                let mut sum = 0i32;

                for c in 0..8 {
                    let coef = coeffs[y * 8 + c] as i32;

                    // XXX what happens in case of overflow? Should test on real hardware.
                    sum += coef * self[x * 8 + c] as i32;
                }

                let v = (sum + 0x4000) >> 15;

                block_tmp[x * 8 + y] = v as i16;
            }
        }

        // 2nd pass, saturate the values into `block`
        for y in 0..8 {
            for x in 0..8 {
                let mut sum = 0i32;

                for c in 0..8 {
                    let coef = block_tmp[y * 8 + c] as i32;

                    // XXX what happens in case of overflow? Should test on real hardware.
                    sum += coef * self[x * 8 + c] as i32;
                }

                let v = (sum + 0x4000) >> 15;

                // Sign extend 9bit value
                let v = v as u16;
                let v = v << (16 - 9);
                let v = (v as i16) >> (16 - 9);

                // Saturate
                let v = if v < -128 {
                    -128
                } else if v > 127 {
                    127
                } else {
                    v as i8
                };

                block[y * 8 + x] = v;
            }
        }
    }
}

impl Index<usize> for IdctMatrix {
    type Output = i16;

    fn index(&self, index: usize) -> &Self::Output {
        &self.matrix[index]
    }
}

impl IndexMut<usize> for IdctMatrix {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.matrix[index]
    }
}
