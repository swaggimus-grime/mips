pub mod cpu;
mod cache;
mod instruction;
mod opcodes;
pub mod cop0;
pub mod irq;
pub mod gte;

#[derive(serde::Serialize, serde::Deserialize, Clone, Copy, PartialEq, Eq)]
pub struct RegisterIndex(pub u8);

pub type ClockCycle = i32;

