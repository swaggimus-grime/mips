//! The PlayStation DMA, that can be used to copy data between the RAM and various devices (GPU,
//! CD drive, MDEC, SPU etc...)

use std::ops::{Index, IndexMut};
use crate::psx::processor::{irq, ClockCycle};
use crate::psx::{cd, mdec, sync};
use crate::psx::addressable::Addressable;
use crate::psx::bus::Bus;
use crate::psx::graphics::gpu;
use crate::psx::processor::irq::IrqState;
use crate::psx::sound::spu;

const DMASYNC: sync::SyncToken = sync::SyncToken::Dma;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Dma {
    control: Control,
    irq_config: IrqConfig,
    /// The 7 DMA channels
    channels: [Channel; 7],
    /// Counter to keep track of our refresh cycle
    period_counter: ClockCycle,
}

impl Dma {
    pub fn new() -> Dma {
        Dma {
            control: Control::new(),
            irq_config: IrqConfig::new(),
            channels: [
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
                Channel::new(),
            ],
            period_counter: 0,
        }
    }

    /// Signal that channel `port` is done running. Returns true if an interrupt should be
    /// triggered
    fn end_of_dma(&mut self, port: Port) -> IrqState {
        self[port].control.stop();

        if self.irq_config.irq_enabled(port) {
            self.irq_config.flag_irq(port)
        } else {
            IrqState::Idle
        }
    }
}

impl Index<Port> for Dma {
    type Output = Channel;

    fn index(&self, port: Port) -> &Self::Output {
        &self.channels[port as usize]
    }
}

impl IndexMut<Port> for Dma {
    fn index_mut(&mut self, port: Port) -> &mut Self::Output {
        &mut self.channels[port as usize]
    }
}

pub fn run(bus: &mut Bus) {
    // XXX We probably should only call that if the GPU channel is active but that will make our
    // timings diverge from mednafen so let's not for the time being.
    gpu::run(bus);
    mdec::run(bus);

    let elapsed = sync::resync(bus, DMASYNC);

    bus.dma.period_counter += elapsed;
    bus.dma.period_counter %= DMA_REFRESH_PERIOD;

    run_channel(bus, Port::MDecIn, elapsed);
    run_channel(bus, Port::MDecOut, elapsed);
    run_channel(bus, Port::Gpu, elapsed);
    run_channel(bus, Port::CdRom, elapsed);
    run_channel(bus, Port::Spu, elapsed);
    run_channel(bus, Port::Pio, elapsed);
    run_channel(bus, Port::Otc, elapsed);

    refresh_cpu_halt(bus);

    // XXX This is probably heavy handed, we shouldn't have to run the DMA code when it's idle. But
    // if we change this we break timing compatibility with mednafen (because this handler calls
    // `gpu::run` above) so let's keep it that way.
    let next_sync = DMA_REFRESH_PERIOD - bus.dma.period_counter;
    sync::next_event(bus, DMASYNC, next_sync);
}

pub fn load<T: Addressable>(bus: &mut Bus, offset: u32) -> T {
    let channel = (offset & 0x70) >> 4;
    let reg = offset & 0xc;
    let align = offset & 3;

    let v = match channel {
        0..=6 => {
            // Channel configuration
            let port = Port::from_index(channel);
            let channel = &bus.dma[port];

            match reg {
                0 => channel.base,
                4 => channel.block_control(),
                8 => channel.control.get(),
                _ => unimplemented!("Read from channel {:?} register {:x}", port, reg),
            }
        }
        7 => match reg {
            0 => bus.dma.control.get(),
            4 => bus.dma.irq_config.get(),
            _ => unimplemented!("DMA register read from {:x}", reg),
        },
        _ => unreachable!(),
    };

    T::from_u32(v >> (align * 8))
}

pub fn store<T: Addressable>(bus: &mut Bus, offset: u32, val: T) {
    run(bus);

    let channel = (offset & 0x70) >> 4;
    let reg = offset & 0xc;
    let align = offset & 3;
    let val = val.as_u32() << (align * 8);

    match channel {
        0..=6 => {
            // Channel configuration
            let port = Port::from_index(channel);

            match reg {
                0 => bus.dma[port].base = val & 0xff_ffff,
                4 => bus.dma[port].set_block_control(val),
                8 => set_channel_control(bus, port, ChannelControl::new(val)),
                _ => unimplemented!("Write to channel {:?} register {:x}: {:x}", port, reg, val),
            }
        }
        7 => match reg {
            0 => {
                bus.dma.control.set(val);
                // XXX Not sure why mednafen calls this here since the DMA control register isn't
                // used. Maybe a leftover of some previous iteration?
                refresh_cpu_halt(bus);
            }
            4 => {
                let irq = bus.dma.irq_config.set(val);
                irq::set_level(bus, irq::Interrupt::Dma, irq.is_active());
            }
            _ => unimplemented!("DMA register write to {:x}", reg),
        },
        _ => unreachable!(),
    }
}

fn set_channel_control(bus: &mut Bus, port: Port, mut ctrl: ChannelControl) {
    if port == Port::Otc {
        // Since the OTC's port sole raison d'être is to initialize the GPU linked list in RAM it
        // doesn't support all the DMA control options
        ctrl.0 &= 0x5100_0000;
        // OTC is always backwards
        ctrl.0 |= 2;
    }

    let was_enabled = bus.dma[port].control.is_enabled();
    let is_enabled = ctrl.is_enabled();

    if was_enabled ^ is_enabled {
        // Channel was started or stopped
        if is_enabled {
            bus.dma[port].control = ctrl;
            start(bus, port);
        } else {
            bus.dma[port].control.disable();
            run_channel(bus, port, 128 * 16);
            bus.dma[port].control = ctrl;
        }
    } else {
        bus.dma[port].control = ctrl;
    }

    refresh_cpu_halt(bus);
}

fn refresh_cpu_halt(bus: &mut Bus) {
    let halt_cpu = bus.dma.channels.iter().any(|c| {
        let control = c.control;

        // The CPU is stopped it a channel is running with the "manual" sync mode without chopping
        control.is_enabled() && !control.is_chopped() && control.sync_mode() == SyncMode::Manual
    });

    let timing_penalty = if halt_cpu {
        // The CPU doesn't run, no need to slow it down
        0
    } else {
        // XXX Taken from mednafen, apparently it's only implemented properly for the GPU.
        // Probably because for the other ports the writing timings aren't implemented
        // properly.
        let gpu_chan = &bus.dma[Port::Gpu];
        let control = gpu_chan.control;

        let block_size = gpu_chan.block_size;

        let is_cpu_stalled = control.is_enabled()
            && !control.is_chopped()
            && control.sync_mode() == SyncMode::Request
            && block_size > 0
            && can_run(bus, Port::Gpu, control.is_from_ram());

        if is_cpu_stalled {
            ClockCycle::from(block_size - 1)
        } else {
            0
        }
    };

    bus.set_dma_timing_penalty(timing_penalty);
    bus.set_cpu_stalled_for_dma(halt_cpu);
}

/// Called when channel `port` starts
fn start(bus: &mut Bus, port: Port) {
    bus.dma[port].clock_counter = 0;
    bus.dma[port].remaining_words = 0;

    // Mednafen mentions that some (probably buggy) games like Viewpoint expect some small DMA
    // transfer to complete almost immediately and trigger a race condition if we lag a tiny bit.
    // Given that our implementation is not super timing-accurate we can just give the DMA a small
    // headstart here to take care of this situation
    run_channel(bus, port, 64);
}

/// Run channel `port` for `cycles` CPU cycles
fn run_channel(bus: &mut Bus, port: Port, cycles: ClockCycle) {
    let sync_mode = bus.dma[port].control.sync_mode();

    bus.dma[port].clock_counter += cycles;

    while bus.dma[port].clock_counter > 0 {
        let mut do_copy = true;
        let control = bus.dma[port].control;

        if bus.dma[port].remaining_words == 0 {
            if !control.is_enabled() {
                break;
            }

            if !can_run(bus, port, control.is_from_ram()) {
                // The device is not ready
                break;
            }

            // Initialize RAM pointer
            bus.dma[port].cur_address = bus.dma[port].base;
            let cur_addr = bus.dma[port].cur_address;

            match sync_mode {
                SyncMode::Manual => {
                    let channel = &mut bus.dma[port];

                    channel.remaining_words = channel.block_size;
                }
                SyncMode::Request => {
                    let channel = &mut bus.dma[port];

                    channel.remaining_words = channel.block_size;
                    channel.block_count -= 1;

                    // XXX From mednafen, only GPU timings are implemented so far
                    if port == Port::Gpu {
                        channel.clock_counter -= 7;
                    }
                }
                SyncMode::LinkedList => {
                    let overflow = cur_addr & 0x80_0000 != 0;

                    if overflow {
                        unimplemented!();
                    }

                    let header: u32 = bus.xmem.ram_load(cur_addr & 0x1f_fffc);
                    bus.dma[port].cur_address = (cur_addr + 4) & 0xff_ffff;
                    bus.dma[port].base = header & 0xff_ffff;

                    let remw = (header >> 24) as u16;
                    bus.dma[port].remaining_words = remw;

                    // Timings from mednafen
                    bus.dma[port].clock_counter -= if remw > 0 { 15 } else { 10 };

                    // Mednafen skips the copy in this case because `remaining_words` might be 0
                    // and that wouldn't work correctly because in other situations
                    // `remaining_words` == 0 is the same as setting it to 0x1_0000 (i.e. it wraps
                    // around).
                    do_copy = false;
                }
            }
        } else if control.is_chopped() {
            let channel = &mut bus.dma[port];
            channel.cur_address = channel.base;
            channel.remaining_words = channel.block_size;
        }

        if do_copy {
            let cur_addr = bus.dma[port].cur_address & 0x1f_fffc;

            let overflow = cur_addr & 0x80_0000 != 0;
            if overflow {
                unimplemented!("DMA address overflow");
            }

            let delay = if control.is_from_ram() {
                let v = bus.xmem.ram_load(cur_addr);
                port_store(bus, port, v)
            } else {
                let (v, offset, read_delay) = port_load(bus, port);
                bus.xmem
                    .ram_store((cur_addr.wrapping_add(offset)) & 0x1f_fffc, v);
                read_delay
            };

            bus.dma[port].clock_counter -= delay;

            bus.dma[port].cur_address = 0xff_ffff
                & if control.is_backwards() {
                bus.dma[port].cur_address.wrapping_sub(4)
            } else {
                bus.dma[port].cur_address.wrapping_add(4)
            };

            bus.dma[port].clock_counter -= 1;
            bus.dma[port].remaining_words -= 1;
        }

        if control.is_chopped() {
            let channel = &mut bus.dma[port];

            channel.base = channel.cur_address;
            channel.block_size = channel.remaining_words;
        }

        if bus.dma[port].remaining_words == 0 {
            if !control.is_enabled() {
                break;
            }

            let end_of_dma = match sync_mode {
                SyncMode::Manual => {
                    // We do the transfer all at once, we're done
                    true
                }
                SyncMode::Request => {
                    let channel = &mut bus.dma[port];

                    channel.base = channel.cur_address;

                    channel.block_count == 0
                }
                SyncMode::LinkedList => {
                    // Check for end-of-list marker
                    bus.dma[port].base == 0xff_ffff
                }
            };

            if end_of_dma {
                let irq = bus.dma.end_of_dma(port);
                irq::set_level(bus, irq::Interrupt::Dma, irq.is_active());
            }
        }
    }

    if bus.dma[port].clock_counter > 0 {
        bus.dma[port].clock_counter = 0;
    }
}

/// Check if the device targeted by `port` can either be read from of written to
fn can_run(bus: &mut Bus, port: Port, write: bool) -> bool {
    if write {
        match port {
            Port::Spu => true,
            Port::Gpu => gpu::dma_can_write(bus),
            Port::MDecIn => mdec::dma_can_write(bus),
            _ => unimplemented!("Can write {:?}?", port),
        }
    } else {
        match port {
            Port::Spu => true,
            // XXX This is from mednafen but I'm not entirely sure why this doesn't simply return
            // `true`
            Port::Otc => bus.dma[port].control.is_started(),
            // XXX Does this make sense? Can the CDC block the DMA if no sector has been read?
            Port::CdRom => true,
            Port::MDecOut => mdec::dma_can_read(bus),
            Port::Gpu => true,
            _ => unimplemented!("Can read {:?}?", port),
        }
    }
}

/// Perform a DMA port write. Returns the overhead of the write
fn port_store(bus: &mut Bus, port: Port, v: u32) -> ClockCycle {
    match port {
        Port::Spu => {
            spu::dma_store(bus, v);
            // XXX Mednafen has a long comment explaining where this value comes from (and mention
            // that the average should be closer to 96). This is of course a wildly inaccurate
            // approximation but let's not worry about that for the time being.
            47
        }
        Port::Gpu => {
            gpu::dma_store(bus, v);
            0
        }
        Port::MDecIn => {
            mdec::dma_store(bus, v);
            0
        }
        _ => unimplemented!("DMA port store {:?}", port),
    }
}

/// Perform a DMA port read and returns the value alongside with the write offset (for MDEC, 0
/// elsewhere) and the delay penalty for the read
fn port_load(bus: &mut Bus, port: Port) -> (u32, u32, ClockCycle) {
    let mut offset = 0;
    let mut delay = 0;

    let v = match port {
        Port::Otc => {
            let channel = &bus.dma[port];

            if channel.remaining_words == 1 {
                // Last entry contains the end of table marker
                0xff_ffff
            } else {
                // Pointer to the previous entry
                channel.cur_address.wrapping_sub(4) & 0x1f_ffff
            }
        }
        // XXX latency taken from mednafen
        Port::CdRom => {
            delay = 8;
            cd::dma_load(bus)
        }
        Port::Spu => spu::dma_load(bus),
        Port::MDecOut => {
            let (v, off) = mdec::dma_load(bus);
            offset = off;
            v
        }
        Port::Gpu => gpu::dma_load(bus),
        _ => unimplemented!("DMA port load {:?}", port),
    };

    (v, offset, delay)
}

/// The 7 DMA channels
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Port {
    /// Macroblock decoder input
    MDecIn = 0,
    /// Macroblock decoder output
    MDecOut = 1,
    /// Graphics Processing Unit
    Gpu = 2,
    /// CD-ROM drive
    CdRom = 3,
    /// Sound Processing Unit
    Spu = 4,
    /// Extension port
    Pio = 5,
    /// Used to clear the ordering table in RAM
    Otc = 6,
}

impl Port {
    pub fn from_index(index: u32) -> Port {
        match index {
            0 => Port::MDecIn,
            1 => Port::MDecOut,
            2 => Port::Gpu,
            3 => Port::CdRom,
            4 => Port::Spu,
            5 => Port::Pio,
            6 => Port::Otc,
            n => panic!("Invalid DMA channel {}", n),
        }
    }
}

/// DMA transfer synchronization mode
#[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Clone, Copy, Debug)]
pub enum SyncMode {
    /// Transfer starts when the CPU writes to the Trigger bit and transfers everything at once
    Manual = 0,
    /// Sync blocks to DMA requests
    Request = 1,
    /// Used to transfer GPU command lists
    LinkedList = 2,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct Channel {
    control: ChannelControl,
    /// Base address
    base: u32,
    /// Current address during DMA operation
    cur_address: u32,
    /// Block size (not used in LinkedList mode)
    block_size: u16,
    /// Number of blocks being transferred in Request mode
    block_count: u16,
    remaining_words: u16,
    clock_counter: ClockCycle,
}

impl Channel {
    fn new() -> Channel {
        Channel {
            control: ChannelControl::new(0),
            base: 0,
            cur_address: 0,
            block_size: 0,
            block_count: 0,
            remaining_words: 0,
            clock_counter: 0,
        }
    }

    fn block_control(&self) -> u32 {
        let bs = u32::from(self.block_size);
        let bc = u32::from(self.block_count);

        (bc << 16) | bs
    }

    fn set_block_control(&mut self, val: u32) {
        self.block_size = val as u16;
        self.block_count = (val >> 16) as u16;
    }
}

/// DMA channel control register
#[derive(serde::Serialize, serde::Deserialize, Copy, Clone)]
struct ChannelControl(u32);

impl ChannelControl {
    fn new(v: u32) -> ChannelControl {
        ChannelControl(v)
    }

    fn get(self) -> u32 {
        self.0
    }

    /// Returns true if the 'enable' bit is set
    fn is_enabled(self) -> bool {
        self.0 & (1 << 24) != 0
    }

    /// Clear the enable bit
    fn disable(&mut self) {
        self.0 &= !(1 << 24);
    }

    /// Returns true if the 'start' bit is set
    fn is_started(self) -> bool {
        self.0 & (1 << 28) != 0
    }

    /// Returns true if transfer is from RAM to device, false otherwise
    fn is_from_ram(self) -> bool {
        self.0 & 1 != 0
    }

    fn sync_mode(self) -> SyncMode {
        match (self.0 >> 9) & 3 {
            0 => SyncMode::Manual,
            1 => SyncMode::Request,
            2 => SyncMode::LinkedList,
            _ => unimplemented!("Unknown DMA sync mode"),
        }
    }

    fn is_chopped(self) -> bool {
        self.0 & (1 << 8) != 0
    }

    fn is_backwards(self) -> bool {
        self.0 & (1 << 1) != 0
    }

    /// Called when the channel is stopped
    fn stop(&mut self) {
        // Clear enabled bit
        self.0 &= !(1 << 24);
        // Clear start/trigger bit
        self.0 &= !(1 << 28);
    }
}

/// DMA control register
#[derive(serde::Serialize, serde::Deserialize)]
struct Control(u32);

impl Control {
    fn new() -> Control {
        Control(0)
    }

    fn set(&mut self, conf: u32) {
        self.0 = conf;
    }

    fn get(&self) -> u32 {
        self.0
    }
}

/// DMA IRQ config register
#[derive(serde::Serialize, serde::Deserialize)]
struct IrqConfig(u32);

impl IrqConfig {
    fn new() -> IrqConfig {
        IrqConfig(0)
    }

    fn set(&mut self, conf: u32) -> IrqState {
        let write_mask = 0x00ff_803f;
        self.0 &= !write_mask;
        self.0 |= conf & write_mask;

        // Writing 1 to the flag bits acks the interrupts
        let ack = conf & 0x7f00_0000;
        self.0 &= !ack;

        self.refresh_irq()
    }

    fn get(&self) -> u32 {
        self.0
    }

    fn irq_enabled(&self, port: Port) -> bool {
        let bit = 16 + port as usize;

        self.0 & (1 << bit) != 0
    }

    /// Set the IRQ flag for `Port` active
    fn flag_irq(&mut self, port: Port) -> IrqState {
        let bit = 24 + port as usize;

        self.0 |= 1 << bit;

        self.refresh_irq()
    }

    fn master_irq_forced(&self) -> bool {
        self.0 & (1 << 15) != 0
    }

    fn channel_irq_enable(&self) -> bool {
        self.0 & (1 << 23) != 0
    }

    /// Refresh the state of the master IRQ flag and returns IrqState::Triggered if an interrupt
    /// should be asserted
    fn refresh_irq(&mut self) -> IrqState {
        let flags = self.0 & 0x7f00_0000;

        let channel_irq_active = self.channel_irq_enable() && flags != 0;

        let new_master = channel_irq_active || self.master_irq_forced();

        if new_master {
            self.0 |= 1 << 31;
            IrqState::Active
        } else {
            self.0 &= !(1 << 31);
            IrqState::Idle
        }
    }
}

/// How often should we update the DMA state. The smaller this value the more accurate we'll be,
/// but very small values will just ruin performance
const DMA_REFRESH_PERIOD: ClockCycle = 128;
