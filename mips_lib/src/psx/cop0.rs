//! MIPS Coprocessor 0
//!
//! Unlike the GTE (Coprocessor 2) this one is part of standard (and required) MIPS. On the
//! PlayStation it mainly deals with exceptions (generated by interrupts or software). It also
//! provide facilities for cache management (used to clean the instruction cache for instance) and
//! breakpoint registers.
//!
//! It's also the coprocessor that's supposed to manage virtual memory but there's no such thing on
//! the PSX.

use super::cpu::RegisterIndex;
use super::{cpu, irq, Psx};

/// Coprocessor 0: System control
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Cop0 {
    /// Cop0 register 12: Status register
    sr: u32,
    /// Cop0 register 13: Cause register
    cause: u32,
    /// Cop0 register 14: Exception PC
    epc: u32,
}

impl Cop0 {
    pub fn new() -> Cop0 {
        Cop0 {
            sr: 0,
            cause: 0,
            epc: 0,
        }
    }

    /// Return the value of the SR register
    pub fn sr(&self) -> u32 {
        self.sr
    }

    /// Returns true if the cache is isolated.
    pub fn cache_isolated(&self) -> bool {
        self.sr & 0x10000 != 0
    }

    /// Returns true if the CPU can process interrupts
    pub fn irq_enabled(&self) -> bool {
        self.sr & 1 != 0
    }

    /// Return the value of the BAD register
    pub fn bad(&self) -> u32 {
        // XXX we don't emulate the "BAD" cop0 register yet. It's almost useless in the PSX anyway
        // since there's no MMU.
        0
    }
}

/// Move To Coprocessor 0
pub fn mtc0(psx: &mut Psx, cop_r: RegisterIndex, v: u32) {
    match cop_r.0 {
        // Breakpoints registers
        3 | 5 | 6 | 7 | 9 | 11 => {
            if v != 0 {
                warn!("Unhandled write to cop0r{}: {:08x}", cop_r.0, v)
            }
        }
        12 => {
            psx.cop0.sr = v;
            cpu::irq_changed(psx);
        }
        // Cause register
        13 => {
            // TODO: be careful to correctly handle the two software interrupt bits [8:9]. Should
            // probably call `cpu::irq_changed`.
            if v != 0 {
                unimplemented!("Unhandled write to CAUSE register: {:08x}", v)
            }
            cpu::irq_changed(psx);
        }
        _ => panic!("Unhandled COP0 register {}", cop_r.0),
    }
}

/// Move From Coprocessor 0
pub fn mfc0(psx: &mut Psx, cop_r: RegisterIndex) -> u32 {
    match cop_r.0 {
        6 => {
            // No$ says this register "randomly" memorizes a jump target after certain exceptions
            // occur. Doesn't seem very useful and would require a lot more testing to implement
            // accurately.
            warn!("Unhandled read from JUMP_DEST (cop0r6)");
            0
        }
        7 => {
            // DCIC: breakpoint control
            warn!("Unhandled read from DCIC (cop0r7)");
            0
        }
        8 => {
            // This register should be mostly useless on the PlayStation since it doesn't have
            // virtual memory, however some exceptions do write to this register so maybe it's
            // worth implementing better
            warn!("Unhandled read from BAD_VADDR (cop0r8)");
            psx.cop0.bad()
        }
        12 => psx.cop0.sr(),
        13 => cause(psx),
        14 => psx.cop0.epc,
        15 => PROCESSOR_ID,
        _ => {
            warn!("Unhandled read from COP0 register {}", cop_r.0);
            0
        }
    }
}

/// Called when the CPU is about to enter an exception handler. Returns the address of the handler
/// that should be used.
pub fn enter_exception(psx: &mut Psx, cause: Exception) -> u32 {
    // Shift bits [5:0] of `SR` two places to the left. Those bits are three pairs of Interrupt
    // Enable/User Mode bits behaving like a stack 3 entries deep. Entering an exception pushes a
    // pair of zeroes by left shifting the stack which disables interrupts and puts the CPU in
    // kernel mode. The original third entry is discarded (it's up to the kernel to handle more
    // than two recursive exception levels).
    let pc = psx.cpu.current_pc();

    let mode = psx.cop0.sr & 0x3f;

    psx.cop0.sr &= !0x3f;
    psx.cop0.sr |= (mode << 2) & 0x3f;

    // Update `CAUSE` register with the exception code (bits [6:2])
    psx.cop0.cause &= !0x7c;
    psx.cop0.cause |= (cause as u32) << 2;

    if psx.cpu.in_delay_slot() {
        // When an exception occurs in a delay slot `EPC` points to the branch instruction and bit
        // 31 of `CAUSE` is set.
        psx.cop0.epc = pc.wrapping_sub(4);
        psx.cop0.cause |= 1 << 31;
    } else {
        psx.cop0.epc = pc;
        psx.cop0.cause &= !(1 << 31);
    }

    // Since we've just disabled the interrupts we may need to refresh the CPU state
    cpu::irq_changed(psx);

    // The address of the exception handler address depends on the value of the BEV bit in SR
    if (psx.cop0.sr & (1 << 22)) != 0 {
        0xbfc0_0180
    } else {
        0x8000_0080
    }
}

/// The counterpart to "enter_exception": shift SR's mode back into place. Doesn't touch CAUSE or
/// EPC however.
pub fn return_from_exception(psx: &mut Psx) {
    let cop0 = &mut psx.cop0;

    let mode = cop0.sr & 0x3f;

    // Bits [5:4] (the third and last mode in the stack) remains untouched and is therefore
    // a copy of the 2nd entry.
    cop0.sr &= !0xf;
    cop0.sr |= mode >> 2;

    // We might have changed the exception enable bit, refresh the CPU
    cpu::irq_changed(psx);
}

pub fn cause(psx: &Psx) -> u32 {
    let mut c = psx.cop0.cause;

    // Set the IRQ bit if necessary
    c |= (irq::active(psx) as u32) << 10;

    c
}

/// Returns true if the CPU should be interrupted
pub fn irq_pending(psx: &Psx) -> bool {
    // Status Register bits [8:15] line up with the same bits in cause to mask any pending
    // interrupts
    let active_interrupts = psx.cop0.sr & cause(psx) & 0xff_ff00;

    psx.cop0.irq_enabled() && active_interrupts != 0
}

/// Exception types (as stored in the `CAUSE` register)
#[derive(Clone, Copy, Debug)]
#[allow(unused)]
pub enum Exception {
    /// Interrupt Request
    Interrupt = 0x0,
    /// Address error on load
    LoadAddressError = 0x4,
    /// Address error on store
    StoreAddressError = 0x5,
    /// System call (caused by the SYSCALL opcode)
    SysCall = 0x8,
    /// Breakpoint (caused by the BREAK opcode)
    Break = 0x9,
    /// CPU encountered an unknown instruction
    IllegalInstruction = 0xa,
    /// Unsupported coprocessor operation
    CoprocessorError = 0xb,
    /// Arithmetic overflow
    Overflow = 0xc,
}

/// Value of the "Processor ID" register (Cop0r15). This is the value
/// returned by my SCPH-7502.
pub const PROCESSOR_ID: u32 = 0x0000_0002;
