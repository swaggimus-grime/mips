use log::Level::Debug;
use crate::psx::memory::range::mask_region;
use crate::psx::addressable::{AccessWidth, Addressable};
use crate::psx::bios::bios::Bios;
use crate::psx::ioable::Loadable;
use crate::psx::memory::mem_ctrl::*;
use crate::psx::processor::cpu;
use super::processor::cpu::{ClockCycle, Cpu};
use crate::psx::memory::range;

pub struct Bus {
    pub(crate) cpu: Cpu,
    pub(crate) bios: Bios,
    cycles: ClockCycle,
    mem_ctrl: MemoryControl
}

impl Bus {
    pub fn new(bios: Bios) -> Bus {
        Bus {
            cpu: Cpu::new(),
            bios,
            cycles: 0,
            mem_ctrl: MemoryControl::new()
        }
    }

    pub fn tick(&mut self, cycles: ClockCycle) {
        self.cycles += cycles;
    }

    pub fn update(&mut self) {
        cpu::execute(self);
    }

    pub fn load<T: Addressable, const N: usize>(&mut self, addr: u32) -> [T; N] 
    where Bios: Loadable<T> {
        let phys_addr = range::mask_region(addr);
        if let Some(offset) = range::BIOS.contains(phys_addr) {
            let delay = self.mem_ctrl.access_delay(TimingOn::Bios, AccessWidth::Word);
            self.tick(delay);
            let ret: [T; N] = self.bios.load::<N>(offset as usize);
            return ret;
        }
        panic!("Wtf do I do with this address");
    }
}