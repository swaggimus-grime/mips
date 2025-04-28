use bitfield::bitfield;
use crate::psx::processor::ClockCycle;

#[derive(Clone)]
pub struct Channel {
    pub ctrl: ChannelControl,
    /// Base address
    pub base: u32,
    /// Current address during DMA operation
    pub cur_address: u32,
    /// Block size (manual and request mode only)
    pub block_size: u16,
    /// Number of blocks being transferred (request mode only)
    pub block_count: u16,
    pub remaining_words: u16,
    pub clock_counter: ClockCycle,
}

impl Channel {
    pub fn new() -> Channel {
        Channel {
            ctrl: ChannelControl(0),
            base: 0,
            cur_address: 0,
            block_size: 0,
            block_count: 0,
            remaining_words: 0,
            clock_counter: 0,
        }
    }

    pub fn set_base_address(&mut self, address: u32) {
        self.base = address & 0xFF_FFFF;
    }

    pub fn block_control(&self) -> u32 {
        let bs = u32::from(self.block_size);
        let bc = u32::from(self.block_count);

        (bc << 16) | bs
    }

    pub fn set_block_control(&mut self, val: u32) {
        self.block_size = val as u16;
        self.block_count = (val >> 16) as u16;
    }
}

#[derive(PartialEq)]
pub enum SyncMode {
    Burst = 0,
    Request = 1,
    LinkedList = 2
}

/// DMA channel control register
bitfield! {
    #[derive(Copy, Clone)]
    pub struct ChannelControl(u32);
    impl Debug;

    /// 0 for device->RAM, 1 for vice-versa
    pub bool, transfer_dir, set_transfer_dir: 0;

    /// Increment mem addr reg by +4 if 0, -4 if 1
    pub bool, madr_decrement, set_madr_decrement: 1;

    // Padding bits 2–7

    /// From nocash:
    /// When 1:
    ///  -Burst mode: enable "chopping" (cycle stealing by CPU)
    ///  -Slice mode: Causes DMA to hang
    ///  -Linked-list mode: Transfer header before data?
    pub bool, on_sync, set_on_sync: 8;

    /// From nocash:
    /// 0 = Burst (transfer data all at once after DREQ is first asserted)  
    /// 1 = Slice (split data into blocks, transfer next block whenever DREQ is asserted)  
    /// 2 = Linked-list mode  
    /// 3 = Reserved
    pub u8, transfer_mode, set_transfer_mode: 10, 9;

    // Padding bits 11–15

    /// Chop DMA size (bits 18–16)
    pub u8, chop_dma_size, set_chop_dma_size: 18, 16;

    // Padding bit 19

    /// Chop CPU size (bits 22–20)
    pub u8, chop_cpu_size, set_chop_cpu_size: 22, 20;

    // Padding bit 23

    /// 1 for running
    pub bool, active_transfer, set_active_transfer: 24;

    // Padding bits 25–27

    /// Force start w/o data request (DREQ)
    pub bool, force_start, set_force_start: 28;

    /// From nocash:  
    /// In forced-burst mode, pauses transfer while set.  
    /// In other modes, stops bit 28 from being cleared after a slice is transferred.  
    /// No effect when transfer was caused by a DREQ.
    pub bool, on_force, set_on_force: 29;

    /// From nocash:  
    /// Perform bus snooping (allows DMA to read from -nonexistent- cache?)
    pub bool, bus_snooping, set_bus_snooping: 30;

    // Padding bit 31
}

impl ChannelControl {
    
    /// Returns true if the 'enable' bit is set
    pub fn active(&self) -> bool {
        self.active_transfer()
    }

    /// Called when the channel is stopped
    pub fn pause(&mut self) {
        // Clear enabled bit
        self.set_active_transfer(false);
    }

    /// Called when the channel is stopped
    pub fn stop(&mut self) {
        // Clear enabled bit
        self.set_active_transfer(false);
        // Clear start/trigger bit
        self.set_force_start(false);
    }

    /// Returns true if the 'start' bit is set
    pub fn started(&self) -> bool { self.force_start() }

    /// Returns true if transfer is from RAM to device, false otherwise
    pub fn ram_to_device(&self) -> bool {
        self.transfer_dir()
    }

    /// Returns true if transfer is from RAM to device, false otherwise
    pub fn device_to_ram(&self) -> bool {
        !self.transfer_dir()
    }

    /// Returns true if transfer is from RAM to device, false otherwise
    pub fn set_ram_to_device(&mut self, ram_to_dev: bool) {
        self.set_transfer_dir(ram_to_dev);
    }

    pub fn sync_mode(&self) -> SyncMode {
        match self.transfer_mode() {
            0 => SyncMode::Burst,
            1 => SyncMode::Request,
            2 => SyncMode::LinkedList,
            _ => unimplemented!("Unknown DMA sync mode"),
        }
    }

    pub fn chopped(&self) -> bool {
        self.on_sync()
    }

    pub fn madr_incrementing(&self) -> bool {
        !self.madr_decrement()
    }

    pub fn madr_decrementing(&self) -> bool {
        self.madr_decrement()
    }
    
    pub fn set_madr_decrementing(&mut self, val: bool) { self.set_madr_decrement(val); }
    
    pub fn get(self) -> u32 {
        self.0
    }
}