use std::fmt;
use std::fmt::{Display, Formatter};
use crate::psx::processor::RegisterIndex;

#[derive(serde::Serialize, serde::Deserialize, Clone, Copy)]
pub struct Instruction(pub(crate) u32);

impl Instruction {
    pub fn new(machine_code: u32) -> Instruction {
        Instruction(machine_code)
    }

    /// Return bits [31:26] of the instruction
    pub(crate) fn opcode(self) -> usize {
        let Instruction(op) = self;

        (op >> 26) as usize
    }

    /// Return bits [5:0] of the instruction
    pub(crate) fn function(self) -> usize {
        let Instruction(op) = self;

        (op & 0x3f) as usize
    }

    /// Return coprocessor opcode in bits [25:21]
    pub(crate) fn cop_opcode(self) -> u32 {
        let Instruction(op) = self;

        (op >> 21) & 0x1f
    }

    /// Return immediate value in bits [16:0]
    pub(crate) fn imm(self) -> u32 {
        let Instruction(op) = self;

        op & 0xffff
    }

    /// Jump target stored in bits [25:0].
    pub(crate) fn imm_jump(self) -> u32 {
        let Instruction(op) = self;

        // The two LSBs aren't stored since (due to alignment constraints) they're assumed to be 0.
        (op & 0x3ff_ffff) << 2
    }

    /// Return immediate value in bits [16:0] as a sign-extended 32bit
    /// value
    pub(crate) fn imm_se(self) -> u32 {
        let Instruction(op) = self;

        let v = (op & 0xffff) as i16;

        v as u32
    }

    /// Shift Immediate values are stored in bits [10:6]
    pub(crate) fn shift(self) -> u32 {
        let Instruction(op) = self;

        (op >> 6) & 0x1f
    }

    /// Return register index in bits [25:21]
    pub(crate) fn s(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex(((op >> 21) & 0x1f) as u8)
    }

    /// Return register index in bits [20:16]
    pub(crate) fn t(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex(((op >> 16) & 0x1f) as u8)
    }

    /// Return register index in bits [15:11]
    pub(crate) fn d(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex(((op >> 11) & 0x1f) as u8)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:08x}", self.0)
    }
}

/*
/// Immediate-type
bitfield! {
    #[derive(serde::Serialize, serde::Deserialize, Clone, Copy)]
    pub struct IType(u32);
    impl Debug;

    /// Immediate value (bits 15 - 0)
    pub u16, imm, set_imm: 15, 0;

    /// Target register index (bits 20 - 16)
    pub u8, rt, set_rt: 20, 16;

    /// Source register index (bits 25 - 21)
    pub u8, rs, set_rs: 25, 21;

    // Padding (bits 31 - 26)
    // Skipped for clarity; unused
}

impl IType {
    pub fn imm_ze32(&self) -> u32 {
        self.imm() as u32
    }

    pub fn imm_se32(&self) -> u32 {
        (self.imm() as i16) as u32
    }

    pub fn t(&self) -> RegisterIndex {
        RegisterIndex(self.rt())
    }

    pub fn s(&self) -> RegisterIndex {
        RegisterIndex(self.rs())
    }
}

/// Jump-type
bitfield! {
    #[derive(Clone, Copy)]
    pub struct JType(u32);
    impl Debug;

    /// Jump destination (bits 25 - 0)
    pub u32, dest, set_dest: 25, 0;

    // Padding (bits 31 - 26)
    // Skipped; reserved or unused
}

impl JType {
    pub fn imm(&self) -> u32 {
        (self.dest() & 0x3FF_FFFF) << 2
    }
}

/// Register-type
bitfield! {
    #[derive(Clone, Copy)]
    pub struct RType(u32);
    impl Debug;

    /// Function code (bits 5 - 0)
    pub u8, func, set_func: 5, 0;

    /// Shift amount (bits 10 - 6)
    pub u8, shift_val, set_shift_val: 10, 6;

    /// Destination register index (bits 15 - 11)
    pub u8, rd, set_rd: 15, 11;

    /// Target register index (bits 20 - 16)
    pub u8, rt, set_rt: 20, 16;

    /// Source register index (bits 25 - 21)
    pub u8, rs, set_rs: 25, 21;

    // Padding (bits 31 - 26)
    // Skipped; reserved or unused
}

impl RType {
    pub fn s(&self) -> RegisterIndex {
        RegisterIndex(self.rs())
    }

    pub fn t(&self) -> RegisterIndex {
        RegisterIndex(self.rt())
    }

    pub fn d(&self) -> RegisterIndex {
        RegisterIndex(self.rd())
    }
    
    pub fn shift(&self) -> u8 {
        self.shift_val()
    }
    
    pub fn secondary(&self) -> u8 {
        self.func()
    }
}

/// Raw data
bitfield! {
    #[derive(Clone, Copy)]
    pub struct RawData(u32);
    impl Debug;

    /// Sub-function (bits 5 - 0)
    pub u8, sub_func, set_sub_func: 5, 0;

    // Padding (bits 6 - 20) — 15 bits
    // Skipped

    /// Coprocessor ID or opcode extension (bits 25 - 21)
    pub u8, cop, set_cop: 25, 21;

    /// Main function/opcode (bits 31 - 26)
    pub u8, main_func, set_main_func: 31, 26;
}

impl RawData {
    pub fn secondary(&self) -> usize {
        self.sub_func() as usize
    }
    
    pub fn cop_opcode(&self) -> u8 {
        self.cop()
    }
    
    pub fn primary(&self) -> usize {
        self.main_func() as usize
    }
}

/// CPU Instruction
#[derive(Clone, Copy)]
pub union Instruction {
    raw: RawData,
    i: IType,
    j: JType,
    r: RType
}

impl Instruction {
    pub fn new(word: u32) -> Instruction {
        Instruction {
            raw: RawData(word),
        }
    }

    pub fn i(&self) -> &IType {
        unsafe { &self.i }
    }

    pub fn j(&self) -> &JType {
        unsafe { &self.j }
    }

    pub fn r(&self) -> &RType { unsafe { &self.r } }

    pub fn raw(&self) -> &RawData { unsafe { &self.raw } }
    
    pub fn bits(&self) -> u32 { unsafe { self.raw.0 } }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:08x}", self.bits())
    }
}
 */