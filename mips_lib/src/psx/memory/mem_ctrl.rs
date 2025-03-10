use crate::psx::addressable::AccessWidth;
use crate::psx::processor::cpu::ClockCycle;

pub struct MemoryControl {
    exp1_base: u32,
    exp2_base: u32,
    exp1_delay: MemDelay,
    exp3_delay: MemDelay,
    bios_delay: MemDelay,
    spu_delay: MemDelay,
    cd_delay: MemDelay,
    exp2_delay: MemDelay,
    com_delay: ComDelay,
    timings: [[ClockCycle; 3]; 4]
}

pub enum TimingOn {
    Bios = 0,
    Spu = 1,
    Cd = 2,
    Exp1 = 3
}

impl MemoryControl {
    pub fn new() -> MemoryControl {
        // Reset values based on Duckstation. Some of these are questionable
        // to me (like the delay sizes) because they don't appear explicitly
        // in the nocash specs.
        let mut mem_ctrl = MemoryControl {
            exp1_base: 0x1F00_0000,
            exp2_base: 0x1F80_2000,
            exp1_delay: MemDelay(0x0013_243F),
            exp3_delay: MemDelay(0x0000_3022),
            bios_delay: MemDelay(0x0013_243F),
            spu_delay: MemDelay(0x2009_31E1),
            cd_delay: MemDelay(0x0002_0843),
            exp2_delay: MemDelay(0x0007_0777),
            com_delay: ComDelay(0x0003_1125),
            timings: [[ClockCycle::default();3]; 4 ]
        };
        mem_ctrl.recalc_access_delays();
        mem_ctrl
    }
    
    pub fn access_delay(&self, device: TimingOn, access: AccessWidth) -> ClockCycle {
        self.timings[device as usize][access as usize]
    }
    
    fn recalc_access_delays(&mut self) {
        self.timings[TimingOn::Bios as usize] = self.calc_access_delays(&self.bios_delay);
        self.timings[TimingOn::Spu as usize] = self.calc_access_delays(&self.spu_delay);
        self.timings[TimingOn::Cd as usize] = self.calc_access_delays(&self.bios_delay);
        self.timings[TimingOn::Exp1 as usize] = self.calc_access_delays(&self.bios_delay);
    }

    fn calc_access_delays(&self, mem_delay: &MemDelay) -> [ClockCycle; 3]{
        let com_delay = &self.com_delay;
        let (mut first, mut seq, mut min) = (0, 0, 0);
        
        if mem_delay.use_com0() {
            let com0 = com_delay.com0() as i32;
            first +=  - 1;
            seq += com0 - 1;
        }
        if mem_delay.use_com2() {
            let com2 = com_delay.com2() as i32;
            first += com2;
            seq += com2;
        }
        if mem_delay.use_com3() {
            min = com_delay.com3() as i32;
        }
        if first < 6 {
            first += 1;
        }

        let read_delay = mem_delay.read_delay() as i32;
        first += read_delay + 2;
        seq += read_delay + 2;

        if first < (min + 6) {
            first = min + 6;
        }
        if seq < (min + 2) {
            seq = min + 2;
        }
        
        let bus_width = mem_delay.bus_width();
        [first, match bus_width {
            true => first,
            false => first + seq
        }, match bus_width {
            true => first + seq,
            false => first + seq * 3
        }]
    }
}

struct MemDelay(u32);

impl MemDelay {
    fn write_delay(self) -> u32 {
        self.0 & 0xF
    }
    
    fn read_delay(&self) -> u32 {
        (self.0 >> 4) & 0xF
    }
    
    // 0 for 8-bit, 1 for 16-bit
    fn bus_width(&self) -> bool {
        (self.0 & (1 << 12)) != 0
    }
    
    fn use_com0(&self) -> bool {
        (self.0 & (1 << 8)) != 0
    }

    fn use_com1(&self) -> bool {
        (self.0 & (1 << 9)) != 0
    }

    fn use_com2(&self) -> bool {
        (self.0 & (1 << 10)) != 0
    }

    fn use_com3(&self) -> bool {
        (self.0 & (1 << 11)) != 0
    }
}

struct ComDelay(u32);

impl ComDelay {
    /// Recovery period cycles
    fn com0(&self) -> u32 {
        self.0 & 0xF
    }

    fn com1(&self) -> u32 {
        (self.0 >> 4) & 0xF
    }

    fn com2(&self) -> u32 {
        (self.0 >> 8) & 0xF
    }

    fn com3(&self) -> u32 {
        (self.0 >> 12) & 0xF
    }
}