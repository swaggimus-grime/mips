use crate::psx::bus::Bus;
use super::cpu::RegIdx;

#[derive(Clone, Copy)]
pub struct Instruction(pub u32);

impl Instruction {
    /// Return bits [31:26] of the instruction
    pub fn primary(self) -> u32 {
        let Instruction(op) = self;

        op >> 26
    }

    /// Return bits [5:0] of the instruction
    pub fn secondary(self) -> u32 {
        let Instruction(op) = self;

        op & 0x3f
    }

    /// Return coprocessor opcode in bits [25:21]
    pub fn cop(self) -> u32 {
        let Instruction(op) = self;

        (op >> 21) & 0x1f
    }

    /// Return register index in bits [25:21]
    pub fn s(self) -> RegIdx {
        let Instruction(op) = self;

        RegIdx((op >> 21) & 0x1f)
    }

    /// Return register index in bits [20:16]
    pub fn t(self) -> RegIdx {
        let Instruction(op) = self;

        RegIdx((op >> 16) & 0x1f)
    }

    /// Return register index in bits [15:11]
    pub fn d(self) -> RegIdx {
        let Instruction(op) = self;

        RegIdx((op >> 11) & 0x1f)
    }

    /// Return immediate value in bits [16:0]
    pub fn imm(self) -> u32 {
        let Instruction(op) = self;

        op & 0xffff
    }

    /// Return immediate value in bits [16:0] as a sign-extended 32bit
    /// value
    pub fn imm_se(self) -> u32 {
        let Instruction(op) = self;

        let v = (op & 0xffff) as i16;

        v as u32
    }

    /// Shift Immediate values are stored in bits [10:6]
    pub fn shift(self) -> u32 {
        let Instruction(op) = self;

        (op >> 6) & 0x1f
    }

    /// Jump target stored in bits [25:0]
    pub fn imm_jmp(self) -> u32 {
        let Instruction(op) = self;

        op & 0x3ffffff
    }

    /// Return true if the instruction contains a GTE/COP2 opcode
    pub fn is_gte_op(self) -> bool {
        // XXX This will match all GTE instructions including mfc/mtc
        // and friends, do we only want to match GTE operations
        // instead?
        self.primary() == 0b010001
    }
}
