use log::warn;
use crate::psx::processor::cop0::Exception;
use crate::psx::processor::instruction::Instruction;
use crate::psx::processor::opcodes;
use crate::psx::bus::Bus;

pub struct RegIdx(pub u32);

pub type ClockCycle = i32;

pub struct Cpu {
    // Next instruction to fetch
    next_pc: u32,
    // Fetched instruction that's ready to execute
    pc: u32,
    // Instruction that is currently executing
    current_pc: u32,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            next_pc: 0xbfc00000,
            pc:  0x0,
            current_pc: 0x0,
        }
    }
}

pub fn execute(bus: &mut Bus) {
    let cpu = &mut bus.cpu;
    cpu.current_pc = cpu.pc;
    cpu.pc = cpu.next_pc;
    cpu.next_pc = cpu.pc.wrapping_add(4);
    match cpu.current_pc % 4 == 0 {
        true => {
            let instruction = fetch_instruction(bus);
            opcodes::run_instruction(bus, instruction);
        },
        false => {
            exception(bus, Exception::IlllegalOp);
        }
    }
}

fn fetch_instruction(bus: &mut Bus) -> Instruction {
    let addr = bus.cpu.current_pc;
    let word: [u32; 1] = bus.load(addr);
    Instruction(word[0])
}


/// Trigger an exception
fn exception(bus: &mut Bus, cause: Exception) {
    todo!();
    /*
    // Update the status register
    let handler_addr = cop0::enter_exception(bus, cause);

    // Exceptions don't have a branch delay, we jump directly into
    // the handler
    bus.cpu.pc = handler_addr;
    bus.cpu.next_pc = handler_addr.wrapping_add(4);

     */
}