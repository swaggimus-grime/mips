use crate::psx::mdec::OutputDepth;

/// Command word
#[derive(serde::Serialize, serde::Deserialize, Copy, Clone)]
pub struct Command(pub u32);

impl Command {
    pub fn opcode(self) -> u32 {
        self.0 >> 29
    }

    pub fn block_len(self) -> u16 {
        debug_assert!(self.opcode() == 1);
        self.0 as u16
    }

    pub fn output_format(self) -> u32 {
        (self.0 >> 25) & 0xf
    }

    /// Returns true if we are loading color quantization matrices, false otherwise
    pub fn quant_color(self) -> bool {
        debug_assert!(self.opcode() == 2);
        self.0 & 1 != 0
    }

    pub fn output_depth(self) -> OutputDepth {
        debug_assert!(self.opcode() == 1);

        match (self.0 >> 27) & 3 {
            0 => OutputDepth::D4,
            1 => OutputDepth::D8,
            2 => OutputDepth::D24,
            3 => OutputDepth::D15,
            _ => unreachable!(),
        }
    }

    pub fn is_monochrome(self) -> bool {
        matches!(self.output_depth(), OutputDepth::D4 | OutputDepth::D8)
    }

    /// Returns the value that should be written to the MSB when outputting 15bpp
    pub fn d15_msb(self) -> bool {
        (self.0 >> 25) & 1 != 0
    }

    /// True if the output should be signed
    pub fn output_signed(self) -> bool {
        (self.0 >> 26) & 1 != 0
    }
}
