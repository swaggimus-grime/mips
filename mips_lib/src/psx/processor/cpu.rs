//! PSX MIPS CPU implementation, including instruction cache
//!
//! The timings code is copied from mednafen

use super::cop0::Exception;

//#[cfg(feature = "debugger")]
//use super::debugger;

use std::fmt;
use crate::psx::addressable::{AccessWidth, Addressable};
use crate::psx::bus::Bus;
use crate::psx::memory::map;
use crate::psx::processor::cache::ICacheLine;
pub(crate) use crate::psx::processor::instruction::Instruction;
pub(crate) use crate::psx::processor::{cop0, ClockCycle, RegisterIndex};
use crate::psx::processor::opcodes::OPCODE_HANDLERS;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Cpu {
    /// Address of the instruction currently being executed. Used for
    /// setting the EPC in exceptions.
    pub(crate) current_pc: u32,
    /// The Program Counter register: points to the next instruction
    pub(crate) pc: u32,
    /// Next value for the PC, used to emulate the branch delay slot
    pub(crate) next_pc: u32,
    /// General Purpose Registers. The first entry (R0) must always contain 0
    regs: [u32; 32],
    /// HI register for division remainder and multiplication MSBs
    pub(crate) hi: u32,
    /// LO register for division quotient and multiplication LSBs
    pub(crate) lo: u32,
    /// Load initiated by the current instruction (will take effect after the load delay slot). The
    /// values in the triplet are: target register, value, number of cycles taken by the load
    pub(crate) load: Option<(RegisterIndex, u32, u8)>,
    /// If a load is taking place this is the register being targetted
    pub(crate) free_cycles_reg: RegisterIndex,
    /// `free_cycles[free_cycles_reg]` contains the number of cycles left before the last load (if
    /// any) completes. That means that at any given moment only one cell of this array is
    /// effectively in use but laying things out that way lets us avoid a bunch of branching in
    /// `reg_dep` which is executed once *per CPU register* for *every* instruction, so it gives us
    /// a significant speedup.
    pub(crate) free_cycles: [u8; 32],
    /// Set by the current instruction if a branch occurred and the next instruction will be in the
    /// delay slot.
    pub(crate) branch: bool,
    /// Instruction cache (256 4-word cachelines, for a total of 4KiB)
    #[serde(with = "serde_big_array::BigArray")]
    icache: [ICacheLine; 0x100],
    /// Set if the current instruction executes in the delay slot
    delay_slot: bool,
    /// If true BREAK instructions trigged the debugger instead of generating an exception
    pub(crate) debug_on_break: bool,
    /// Date at which the last division or multiplication will be done. DIV(U) and MULT(U) can run
    /// concurrently with other "normal" MIPS instructions and only block if a mf(hi|lo) is
    /// executed before they're finished
    pub(crate) mult_div_end: ClockCycle,
    /// Date at which the last GTE operation will be done. GTE commands can run concurrently with
    /// main CPU instructions and will only stall is
    #[serde(default)]
    pub(crate) gte_command_end: ClockCycle,
    /// Offset added to the index in the opcode jumptable when decoding instructions
    pub(crate) opcode_table_offset: u8,
}

impl Cpu {
    pub fn new() -> Cpu {
        // Reset value for the PC: beginning of BIOS ROM
        let reset_pc = 0xbfc0_0000;

        Cpu {
            current_pc: reset_pc,
            pc: reset_pc,
            next_pc: reset_pc.wrapping_add(4),
            // Not sure what the reset values of the general purpose registers is but it shouldn't
            // matter since the BIOS doesn't read them. R0 is always 0 however, so that shouldn't
            // be changed.
            regs: [0; 32],
            hi: 0,
            lo: 0,
            load: None,
            free_cycles_reg: RegisterIndex(0),
            free_cycles: [0; 32],
            branch: false,
            icache: [ICacheLine::new(); 0x100],
            delay_slot: false,
            debug_on_break: false,
            mult_div_end: 0,
            gte_command_end: 0,
            opcode_table_offset: 0,
        }
    }

    /// Returns the address of the instruction currently being executed
    pub fn current_pc(&self) -> u32 {
        self.current_pc
    }

    /// Force PC address. Meant to be used from the debugger. Use at your own risk.
    #[cfg(feature = "debugger")]
    pub fn force_pc(&mut self, pc: u32) {
        self.pc = pc;
        self.next_pc = self.pc.wrapping_add(4);
        self.delay_slot = false;
    }

    /// Returns true if the instruction currently being executed is in a delay slot
    pub fn in_delay_slot(&self) -> bool {
        self.delay_slot
    }

    /// Get the value of all general purpose registers
    #[cfg(feature = "debugger")]
    pub fn regs(&self) -> &[u32] {
        &self.regs
    }

    /// Get the value of the LO register
    #[cfg(feature = "debugger")]
    pub fn lo(&self) -> u32 {
        self.lo
    }

    /// Get the value of the HI register
    #[cfg(feature = "debugger")]
    pub fn hi(&self) -> u32 {
        self.hi
    }

    /// Rebase our internal counters that are relative to the global `cycles`
    pub fn rebase_counters(&mut self, cycles: ClockCycle) {
        if self.mult_div_end > 0 {
            self.mult_div_end -= cycles;
        }

        if self.gte_command_end > 0 {
            self.gte_command_end -= cycles;
        }
    }

    /// Return the current value of register `index`
    pub(crate) fn reg(&self, index: RegisterIndex) -> u32 {
        self.regs[index.0 as usize]
    }

    /// Put `val` into register `index`. If `index` is 0 nothing happens as R0 always contains 0.
    pub(crate) fn set_reg(&mut self, index: RegisterIndex, val: u32) {
        self.regs[index.0 as usize] = val;

        // R0 always contains 0
        self.regs[0] = 0;
    }

    /// Branch to immediate value `offset`.
    pub(crate) fn branch(&mut self, offset: u32) {
        // Offset immediates are always shifted two places to the
        // right since `PC` addresses have to be aligned on 32bits at
        // all times.
        let offset = offset << 2;

        self.next_pc = self.pc.wrapping_add(offset);
        self.branch = true;
    }

    /// Execute and clear any pending load
    pub(crate) fn delayed_load(&mut self) {
        if let Some((reg, val, duration)) = self.load {
            self.set_reg(reg, val);

            self.free_cycles[reg.0 as usize] = duration;
            self.free_cycles_reg = reg;

            // We clear the load now that it's been executed
            self.load = None;
        }
    }

    /// Called when any currently-executing load needs to be synced
    fn load_sync(&mut self) {
        self.free_cycles[self.free_cycles_reg.0 as usize] = 0;
    }

    /// Execute the pending delayed and setup the next one. If the new load targets the same
    /// register as the current one then the older one is cancelled (i.e. it never makes it to the
    /// register).
    ///
    /// This method should be used instead of `delayed_load` for instructions that setup a delayed
    /// load.
    pub(crate) fn delayed_load_chain(&mut self, reg: RegisterIndex, val: u32, duration: u8, sync: bool) {
        if let Some((pending_reg, pending_val, duration)) = self.load {
            // This takes care of the following situation:
            //
            //    lw   $t0, 0($s0)
            //    lw   $t0, 0($s1)
            //    move $t0, $s1
            //
            // In this situation the 2nd LW targets the same register an the one just before.
            // In this scenario the first load never completes and the value of T0 in the move
            // won't have been modified by either LW (the first one being interrupted by the
            // second one, and the second one not having yet finished since we're in the delay
            // slot).
            if pending_reg != reg {
                // We target a different register, we can execute the delay load.
                self.set_reg(pending_reg, pending_val);

                // If the calling function doesn't force a sync (i.e. the load doesn't come from
                // memory) we can also set the free_cycles as usual since the load can run in the
                // background while other instruction executes
                if !sync {
                    self.free_cycles[pending_reg.0 as usize] = duration;
                    self.free_cycles_reg = pending_reg;
                }
            }
        }

        self.load = Some((reg, val, duration));
    }
}

/// Conventional names given to the MIPS registers
const REGISTER_NAMES: [&str; 32] = [
    "r0", // Hardwired to be always 0
    "at", // Assembler Temporary (reserved for the assembler)
    "v0", "v1", // First and second return values
    "a0", "a1", "a2", "a3", // First four function arguments
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", // Temporary registers
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", // Saved registers
    "t8", "t9", // Temporary registers
    "k0", "k1", // Reserved for kernel use
    "gp", // Global pointer (not normally used on the PSX)
    "sp", // Stack Pointer
    "fp", // Frame Pointer
    "ra", // Return address
];

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "PC: 0x{:08x}", self.pc)?;

        for i in 0..16 {
            writeln!(
                f,
                "{}: 0x{:08x}    {}: 0x{:08x}",
                REGISTER_NAMES[i],
                self.regs[i],
                REGISTER_NAMES[i + 16],
                self.regs[i + 16]
            )?;
        }

        Ok(())
    }
}

/// Called whenever the IRQ state has potentially changed
pub fn irq_changed(bus: &mut Bus) {
    bus.cpu.opcode_table_offset = if cop0::irq_pending(bus) {
        // Use the 2nd half of the jump table
        64
    } else {
        // Use the normal table
        0
    };
}

pub fn run_next_instruction(bus: &mut Bus) {
    // Explanation of the various *pc variables:
    //
    // * `bus.cpucurrent_pc`: Pointer to the instruction about to be executed.
    //
    // * `bus.cpu.pc`: Pointer to the next instruction to be executed. It's possible for this value
    //                 to change before the next instruction is reached if an exception occurs
    //                 (exceptions have no delay slot).
    //
    // * `bus.cpu.next_pc`: Value `bus.cpu.pc` will take on the *next* cycle, so effectively a
    //                      pointer to the next next instruction being executed. It's possible for
    //                      this value to change before the next instruction is reached if an
    //                      exception *or* a branch/jump occurs. We can't change `bus.cpu.pc`
    //                      directly in case of a branch because we need to emulate the branch
    //                      delay slot.
    //
    // So basically when a branch/jump is executed only `bus.cpu.next_pc` is modified, which means
    // that the value of the next instruction to be executed (pointed at by `bus.cpu.pc`) remains
    // in the pipeline. Thus the branch delay slot is emulated accurately.
    bus.cpu.current_pc = bus.cpu.pc;
    bus.cpu.pc = bus.cpu.next_pc;
    bus.cpu.next_pc = bus.cpu.pc.wrapping_add(4);

    // If the last instruction was a branch then we're in the delay slot
    bus.cpu.delay_slot = bus.cpu.branch;
    bus.cpu.branch = false;

    // Debugger entrypoint: used for code breakpoints and stepping
    #[cfg(feature = "debugger")]
    {
        //debugger::pc_change(bus);
    }

    if bus.cpu.current_pc % 4 != 0 {
        // PC is not correctly aligned!
        exception(bus, Exception::LoadAddressError);
        return;
    }

    // Fetch instruction at PC
    let instruction = fetch_instruction(bus);

    instruction_tick(bus);
    

    let opcode_index = instruction.opcode() | bus.cpu.opcode_table_offset as usize;

    let handler = OPCODE_HANDLERS[opcode_index];

    handler(bus, instruction);
}

/// Advance the CPU cycle counter by one tick unless we're still catching up with a load
pub fn instruction_tick(bus: &mut Bus) {
    let r = bus.cpu.free_cycles_reg;
    let free_cycles = &mut bus.cpu.free_cycles[r.0 as usize];

    if *free_cycles > 0 {
        // We're still catching up with a load. Since `load` advances the cycle counter to the
        // end of the load it means that we're still catching up, so we don't do anything
        *free_cycles -= 1;
    } else {
        // We're in sync, we can move the time forward
        bus.tick(1);
    }
}

/// Fetch the instruction at `current_pc` through the instruction cache
fn fetch_instruction(bus: &mut Bus) -> Instruction {
    let pc = bus.cpu.current_pc;

    // KUSEG and KSEG0 regions are cached. KSEG1 is uncached and
    // KSEG2 doesn't contain any code
    let cached = pc < 0xa000_0000;

    if cached && bus.icache_enabled() {
        // The MSB is ignored: running from KUSEG or KSEG0 hits the same cachelines. So for
        // instance addresses 0x00000000 and 0x80000000 have the same tag and you can jump from one
        // to the other without having to reload the cache.

        // Cache tag: bits [30:12]
        let tag = pc & 0x7fff_f000;
        // Cache line "bucket": bits [11:4]
        let line_off = ((pc >> 4) & 0xff) as usize;
        // Index in the cache line: bits [3:2]
        let index = (pc >> 2) & 3;

        // Fetch the cacheline for this address
        let mut line = bus.cpu.icache[line_off];

        // Check the tag and validity
        if line.tag() != tag || line.valid_index() > index {
            // Cache miss. Fetch the cacheline starting at the current index. If the index is not 0
            // then some words are going to remain invalid in the cacheline.
            let mut cpc = pc;

            // We're about to access the memory to fetch the instructions, we need to finish any
            // active load first
            bus.cpu.load_sync();

            // Cache timing lifted straight from Mednafen
            bus.tick(7 - index as i32);

            for i in index..4 {
                let instruction = bus.xmem.load_instruction(cpc);

                line.set_instruction(i, instruction);
                cpc += 4;
            }

            // Set the tag and valid bits
            line.set_tag_valid(pc);

            // Store updated cacheline
            bus.cpu.icache[line_off] = line;
        }

        // Cache line is now guaranteed to be valid
        line.instruction(index)
    } else {
        // XXX Apparently pointing the PC to KSEG2 causes a bus error no matter what, even if you
        // point it at some valid register address (like the "cache control" register). Not like it
        // should happen anyway, there's nowhere to put code in KSEG2, only a bunch of registers.

        // We need to wait for any active load to finish before we can fetch the instruction
        bus.cpu.load_sync();

        // When running without a cache the penalty is about 4 cycles per instruction, sometimes
        // more (but on average for typical code fairly close to 4). This is therefore a bit
        // optimistic but it would be pretty tricky to emulate the pipeline more accurately and
        // running a tiny bit too fast shouldn't be too much of a problem, this isn't a Game Boy.
        bus.tick(4);

        bus.xmem.load_instruction(pc)
    }
}

/// Handle writes when the cache is isolated
pub fn cache_store<T: Addressable>(bus: &mut Bus, addr: u32, val: T) {
    // Implementing full cache emulation requires handling many corner cases. For now I'm just
    // going to add support for cache invalidation which is the only use case for cache isolation
    // as far as I know.
    let val = val.as_u32();

    if !bus.icache_enabled() {
        panic!("Cache maintenance while instruction cache is disabled");
    }

    if T::width() != AccessWidth::Word || val != 0 {
        panic!("Unsupported write while cache is isolated: {:08x}", val);
    }

    let line_off = ((addr >> 4) & 0xff) as usize;

    // Fetch the cacheline for this address
    let mut line = bus.cpu.icache[line_off];

    if bus.tag_test_mode() {
        // In tag test mode the write invalidates the entire targeted cacheline
        line.invalidate();
    } else {
        // Otherwise the write ends up directly in the cache.
        let index = (addr >> 2) & 3;

        let instruction = Instruction(val);

        line.set_instruction(index, instruction);
    }

    bus.cpu.icache[line_off] = line;
}

/// Trigger an exception
pub(crate) fn exception(bus: &mut Bus, cause: Exception) {
    // Update the status register
    let handler_addr = cop0::enter_exception(bus, cause);

    // Exceptions don't have a branch delay, we jump directly into
    // the handler
    bus.cpu.pc = handler_addr;
    bus.cpu.next_pc = handler_addr.wrapping_add(4);
}

/// Execute a memory write
pub(crate) fn store<T: Addressable>(bus: &mut Bus, addr: u32, v: T) {
    if bus.cop0.cache_isolated() {
        // When the cache is isolated the CPU writes don't reach the system bus, instead they end
        // up in the cache.
        cache_store(bus, addr, v);
        return;
    }

    #[cfg(feature = "debugger")]
    {
        //debugger::memory_write(bus, addr);
    }

    bus.store(addr, v);
}

/// Execute a memory read and return the value alongside with the number of cycles necessary for
/// the load to complete;
pub(crate) fn load<T: Addressable>(bus: &mut Bus, addr: u32, from_lwc: bool) -> (T, u8) {
    // Any pending load must terminate before we attempt to start a new one
    bus.cpu.load_sync();

    #[cfg(feature = "debugger")]
    {
        //debugger::memory_read(bus, addr);
    }

    // The Scratch Pad is the CPU data cache, it therefore has very low latency and needs to be
    // special-cased
    {
        // XXX Scratch Pad can't be accessed through uncached address space, so this is a bit too
        // aggressive.
        let abs_addr = map::mask_region(addr);

        if let Some(offset) = map::SCRATCH_PAD.contains(abs_addr) {
            return (bus.scratch_pad.load(offset), 0);
        }
    }

    if bus.cpu.load.is_none() {
        // From mednafen: apparently the CPU manages to schedule loads faster if they happen in a
        // row?
        bus.tick(2);
    }

    let prev_cc = bus.cycles;

    let v = bus.load(addr);

    // From mednafen: delay to complete the load
    let d = if from_lwc { 1 } else { 2 };
    bus.tick(d);

    // Compute the duration of the load. The CPU (if possible) keeps executing instructions
    // while the load takes place, so effectively at this point `bus.cpu.cycles` is too far
    // ahead by `duration` cycles, so `instruction_tick` will actually skip cycles until we catch
    // up.
    let duration = bus.cycles - prev_cc;

    // The duration of an instruction should be a small number that fits easily in an u8 to save
    // some space (and some cache)
    debug_assert!(duration < 0x100);

    (v, duration as u8)
}

/// The PSX CPU is supposed to run at 33.868Mhz. This frequency is exactly 0x300 times the CD
/// sample rate frequency of 44.1kHz so that the SPU can run synchronously.
pub const CPU_FREQ_HZ: ClockCycle = 33_868_800;
