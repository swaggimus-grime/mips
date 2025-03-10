use super::instruction::Instruction;

struct CacheLine {
    // Tag has the upper 20 bits of the physical address being cached
    tag_valid: u32,
    instructions: [Instruction; 4]
}

impl CacheLine {
    fn new() -> CacheLine {
        // The cache starts in a random state. In order to catch
        // missbehaving software we fill them with "trap" values
        CacheLine {
            // Tag is 0, all line valid
            tag_valid: 0x0,
            instructions: [Instruction(0); 4],
        }
    }

    /// Return the cacheline's tag
    fn tag(&self) -> u32 {
        self.tag_valid & 0xfffff000
    }

    /// Return the cacheline's first valid word
    fn valid_index(&self) -> u32 {
        // We store the valid bits in bits [4:2], this way we can just
        // mask the PC value in `set_tag_valid` without having to
        // shuffle the bits around
        (self.tag_valid >> 2) & 0x7
    }

    /// Set the cacheline's tag and valid bits. `pc` is the first
    /// valid PC in the cacheline.
    fn set_tag_valid(&mut self, pc: u32) {
        self.tag_valid =  pc & 0x7ffff00c;
    }

    /// Invalidate the entire cacheline by pushing the index out of
    /// range. Doesn't change the tag or contents of the line.
    fn invalidate(&mut self) {
        // Setting bit 4 means that the value returned by valid_index
        // will be in the range [4, 7] which is outside the valid
        // cacheline index range [0, 3].
        self.tag_valid |= 0x10;
    }

    fn instruction(&self, index: u32) -> Instruction {
        self.instructions[index as usize]
    }

    fn set_instruction(&mut self, index: u32, instruction: Instruction) {
        self.instructions[index as usize] = instruction;
    }
}

impl Default for CacheLine {
    fn default() -> CacheLine {
        CacheLine::new()
    }
}