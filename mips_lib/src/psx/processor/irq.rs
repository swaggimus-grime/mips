//! IRQ handling

use crate::psx::bus::Bus;
use crate::psx::processor::cpu;

/// The PlayStation supports 10 interrupts
#[derive(serde::Serialize, serde::Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Interrupt {
    /// Display in vertical blanking
    VBlank = 0,
    /// CDROM controller
    CdRom = 2,
    /// DMA transfer done
    Dma = 3,
    /// Timer0 interrupt
    Timer0 = 4,
    /// Timer1 interrupt
    Timer1 = 5,
    /// Timer2 interrupt
    Timer2 = 6,
    /// Gamepad and Memory Card controller interrupt
    PadMemCard = 7,
    /// SPU interrupt
    Spu = 9,
}

#[derive(serde::Serialize, serde::Deserialize, Clone, Copy)]
pub struct InterruptState {
    /// Interrupt lane levels
    level: u16,
    /// Interrupt status
    status: u16,
    /// Interrupt mask
    mask: u16,
}

impl InterruptState {
    pub fn new() -> InterruptState {
        InterruptState {
            level: 0,
            status: 0,
            mask: 0,
        }
    }
}

pub fn status(bus: &Bus) -> u16 {
    bus.irq.status
}

pub fn mask(bus: &Bus) -> u16 {
    bus.irq.mask
}

pub fn set_high(bus: &mut Bus, which: Interrupt) {
    let m = 1 << which as usize;

    if bus.irq.level & m != 0 {
        // No change
        return;
    }

    // Rising edge
    bus.irq.level |= m;
    bus.irq.status |= m;

    cpu::irq_changed(bus);
}

pub fn set_low(bus: &mut Bus, which: Interrupt) {
    let m = 1 << which as usize;

    bus.irq.level &= !m;
}

pub fn set_level(bus: &mut Bus, which: Interrupt, high: bool) {
    if high {
        set_high(bus, which);
    } else {
        set_low(bus, which);
    }
}

/// Pulse the IRQ lane
pub fn trigger(bus: &mut Bus, which: Interrupt) {
    let m = 1 << which as usize;

    bus.irq.status |= m;
    bus.irq.level &= !m;

    cpu::irq_changed(bus);
}

pub fn set_mask(bus: &mut Bus, mask: u16) {
    // Temporary hack: trigger an error if a non-implemented interrupt is requested
    let supported: [Interrupt; 8] = [
        Interrupt::VBlank,
        Interrupt::CdRom,
        Interrupt::Dma,
        Interrupt::Timer0,
        Interrupt::Timer1,
        Interrupt::Timer2,
        Interrupt::PadMemCard,
        Interrupt::Spu,
    ];

    let rem = supported
        .iter()
        .fold(mask, |mask, &it| mask & !(1 << it as u16));

    debug_assert!(rem == 0, "Unsupported interrupt: {:04x}", rem);

    bus.irq.mask = mask;

    cpu::irq_changed(bus);
}

/// Acknowledge interrupts by writing 0 to the corresponding bit
pub fn ack(bus: &mut Bus, ack: u16) {
    bus.irq.status &= ack;

    cpu::irq_changed(bus);
}

/// Returns true if we currently have at least one active and unmasked interrupt
pub fn active(bus: &Bus) -> bool {
    status(bus) & mask(bus) != 0
}

/// Basic helper enum that can be used for return values of functions and method that can trigger
/// an interrupt. Generates a compiler warning if it's not checked, which should help avoid
/// "losing" interrupts
#[must_use]
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum IrqState {
    Idle,
    Active,
}

impl IrqState {
    pub fn is_active(self) -> bool {
        self == IrqState::Active
    }
}
