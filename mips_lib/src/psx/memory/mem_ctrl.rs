use std::ops::{Index, IndexMut};
use bitfield::bitfield;
use crate::psx::addressable::AccessWidth;
use crate::psx::processor::ClockCycle;

pub struct MemoryControl {
    regs: [u32; 9],
    timings: [[ClockCycle; 3]; 4]
}

pub enum TimingOn {
    Bios = 0,
    Spu = 1,
    Cd = 2,
    Exp1 = 3
}

enum Register {
    Exp1Base,
    Exp2Base,
    Exp1Delay,
    Exp3Delay,
    BiosDelay,
    SpuDelay,
    CdDelay,
    Exp2Delay,
    ComDelay,
}

impl MemoryControl {
    pub fn new() -> MemoryControl {
        // Reset values based on Duckstation. Some of these are questionable
        // to me (like the delay sizes) because they don't appear explicitly
        // in the nocash specs.
        let mut mem_ctrl = MemoryControl {
            regs: [
                0x1F00_0000,
                0x1F80_2000,
                0x0013_243F,
                0x0000_3022,
                0x0013_243F,
                0x2009_31E1,
                0x0002_0843,
                0x0007_0777,
                0x0003_1125
            ],
            timings: [[ClockCycle::default();3]; 4 ]
        };
        mem_ctrl.recalc_access_delays();
        mem_ctrl
    }
    
    pub fn access_delay(&self, device: TimingOn, access: AccessWidth) -> ClockCycle {
        self.timings[device as usize][access as usize]
    }
    
    fn recalc_access_delays(&mut self) {
        self.timings[TimingOn::Bios as usize] = self.calc_access_delays(Register::BiosDelay);
        self.timings[TimingOn::Spu as usize] = self.calc_access_delays(Register::SpuDelay);
        self.timings[TimingOn::Cd as usize] = self.calc_access_delays(Register::CdDelay);
        self.timings[TimingOn::Exp1 as usize] = self.calc_access_delays(Register::Exp1Delay);
    }

    fn calc_access_delays(&self, delay_reg: Register) -> [ClockCycle; 3] {
        let com_delay = ComDelay(self[Register::ComDelay]);
        let (mut first, mut seq, mut min) = (0, 0, 0);
        let delay_reg = MemDelay(self[delay_reg]);
        if delay_reg.use_com0() {
            let com0 = com_delay.com0() as i32;
            first +=  - 1;
            seq += com0 - 1;
        }
        if delay_reg.use_com2() {
            let com2 = com_delay.com2() as i32;
            first += com2;
            seq += com2;
        }
        if delay_reg.use_com3() {
            min = com_delay.com3() as i32;
        }
        if first < 6 {
            first += 1;
        }

        let read_delay = delay_reg.read_delay() as i32;
        first += read_delay + 2;
        seq += read_delay + 2;

        if first < (min + 6) {
            first = min + 6;
        }
        if seq < (min + 2) {
            seq = min + 2;
        }
        
        let bus_width = delay_reg.bus_width();
        [first, match bus_width {
            true => first,
            false => first + seq
        }, match bus_width {
            true => first + seq,
            false => first + seq * 3
        }]
    }
}

impl Index<Register> for MemoryControl {
    type Output = u32;
    fn index(&self, reg: Register) -> &u32 {
        &self.regs[reg as usize]
    }
}

impl Index<usize> for MemoryControl {
    type Output = u32;
    fn index(&self, idx: usize) -> &u32 {
        &self.regs[idx]
    }
}

impl IndexMut<usize> for MemoryControl {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.regs[index]
    }
}

bitfield! {
    #[derive(Clone, Copy)]
    pub struct MemDelay(u32);
    impl Debug;

    ///  0-3   Write Delay        (00h..0Fh=01h..10h Cycles)
    pub u8, write_delay, set_write_delay: 3, 0;
    ///  4-7   Read Delay         (00h..0Fh=01h..10h Cycles)
    pub u8, read_delay, set_read_delay: 7, 4;
    ///  8     Recovery Period    (0=No, 1=Yes, uses COM0 timings)
    pub bool, recovery_period, set_recovery_period: 8;
    ///  9     Hold Period        (0=No, 1=Yes, uses COM1 timings)
    pub bool, hold_period, set_hold_period: 9;
    ///  10    Floating Period    (0=No, 1=Yes, uses COM2 timings)
    pub bool, floating_period, set_floating_period: 10;
    ///  11    Pre-strobe Period  (0=No, 1=Yes, uses COM3 timings)
    pub bool, pre_strobe_period, set_pre_strobe_period: 11;
    ///  12    Data Bus-width     (0=8bits, 1=16bits)
    pub bool, data_bus_width, set_data_bus_width: 12;
    ///  13    Auto Increment     (0=No, 1=Yes)
    pub bool, auto_increment, set_auto_increment: 13;
    ///  14-15 Unknown (R/W)
    ///  16-20 Number of address bits (memory window size = 1 << N bytes)
    pub u8, num_addr_bits, set_num_addr_bits: 20, 16;
    ///  21-23 Unknown (always zero)
    ///  24-27 DMA timing override
    pub u8, dma_timing_override, set_dma_timing_override: 27, 24;
    ///  28    Address error flag. Write 1 to it to clear it.
    pub bool, addr_error_flag, set_addr_error_flag: 28;
    ///  29    DMA timing select  (0=use normal timings, 1=use bits 24-27)
    pub bool, dma_timing_select, set_dma_timing_select: 29;
    ///  30    Wide DMA           (0=use bit 12, 1=override to full 32 bits)
    pub bool, wide_dma, set_wide_dma: 30;
    ///  31    Wait               (1=wait on external device before being ready)
    pub bool, wait, set_wait: 31;
}

impl MemDelay {
    // 0 for 8-bit, 1 for 16-bit
    fn bus_width(&self) -> bool {
        self.data_bus_width()
    }
    
    fn use_com0(&self) -> bool {
        self.recovery_period()
    }

    fn use_com1(&self) -> bool {
        self.hold_period()
    }

    fn use_com2(&self) -> bool {
        self.floating_period()
    }

    fn use_com3(&self) -> bool {
        self.pre_strobe_period()
    }
}


bitfield! {
    #[derive(Clone, Copy)]
    pub struct ComDelay(u32);
    impl Debug;

    ///  0-3   COM0 - Recovery period cycles
    pub u8, recovery_period, set_recovery_period: 3, 0;
    ///  4-7   COM1 - Hold period cycles
    pub u8, hold_period, set_hold_period: 7, 4;
    ///  8-11  COM2 - Floating release cycles
    pub u8, floating_release, set_floating_period: 11, 8;
    ///  12-15 COM3 - Strobe active-going edge delay
    ///  16-31 Unknown/unused (read: always 0000h)
    pub u8, strobe_delay, set_strobe_delay: 15, 12;
}

impl ComDelay {
    /// Recovery period cycles
    fn com0(&self) -> u8 {
        self.recovery_period()
    }

    fn com1(&self) -> u8 {
        self.hold_period()
    }

    fn com2(&self) -> u8 {
        self.floating_release()
    }

    fn com3(&self) -> u8 {
        self.strobe_delay()
    }
}