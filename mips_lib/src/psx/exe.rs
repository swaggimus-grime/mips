//! Parallel I/O module used to load "naked" PlayStation
//! executables. This doesn't emulate any real world hardware, it's
//! inspired by mednafen's method of loading EXEs.

use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Read;
use log::info;
use crate::error::{MipsError, MipsResult};
use crate::psx::assembler::{Assembler, syntax::*};
use crate::psx::bios::bios::Bios;
use crate::psx::bus::Bus;
use crate::psx::cd::disc::Region;
use crate::psx::memory;
use crate::psx::processor::RegisterIndex;
use crate::util::fs::file::bin;

pub struct Exe {
    /// Base address/dest addr in ram for the executable
    base: u32,
    /// Executable entry point
    entry: u32,
    /// GP/R28 value before jumping to the entry point
    initial_gp: u32,
    /// SP value before jumping to the entry point
    initial_sp: u32,
    /// Base address of the 0-filled area
    memfill_base: u32,
    /// Length of the 0-filled area
    memfill_len: u32,
    /// Region of the executable. `None` if the region for the
    /// executable couldn't be determined.
    region: Option<Region>,
    /// "text" section of the executable
    text: Vec<u8>,
}

impl Exe {
    pub fn new(path: &Path)  -> MipsResult<Exe> {
        let mut bin =  bin::get_file(path)?;

        let mut buf = [0; 16];
        bin.read_exact(&mut buf);
        if &buf != b"PS-X EXE\0\0\0\0\0\0\0\0" {
            // Bad magic, this is not a PlayStation executable
            return Err(MipsError::BadExe)
        }

        let entry = read_u32(&mut bin)?;

        let initial_gp = read_u32(&mut bin)?;

        let base = read_u32(&mut bin)?;

        let text_len = read_u32(&mut bin)?;

        // Let's be on the safe side and reject anormaly big
        // programs. Since the PlayStation RAM is 2MB big it doesn't
        // make sense to have bigger programs
        if text_len > 2 * 1024 * 1024 {
            return Err(MipsError::BadExe);
        }

        // The next two words are Unknown/Unused in the No$ spec,
        // let's ignore them
        read_u32(&mut bin)?;
        read_u32(&mut bin)?;

        let memfill_base = read_u32(&mut bin)?;
        let memfill_len = read_u32(&mut bin)?;

        // For some reason the initial SP address comes with an
        // "offset" (per No$), not sure what that's for
        let initial_sp = read_u32(&mut bin)? + read_u32(&mut bin)?;

        // The next 20bytes are padding
        bin.read_exact(&mut [0; 20]);

        // Skip the first part of the license string to get to the region
        bin.read_exact(&mut [0; 37]);

        let mut region_str = [0; 5];

        bin.read_exact(&mut region_str);

        let region =
            match &region_str {
                b"Japan" => Some(Region::Japan),
                b"Europ" => Some(Region::Europe),
                b"North" => Some(Region::NorthAmerica),
                // Unknown or missing region
                _ => None,
            };

        // Read through all the huge padding
        bin.read_exact(&mut [0; 1930]);

        // Finally we can read the executable itself
        let mut text = vec![0; text_len as usize];

        bin.read_exact(&mut text);

        let mut exe = Exe {
            base,
            entry,
            initial_gp,
            initial_sp,
            memfill_base,
            memfill_len,
            region,
            text
        };

        info!("Loaded PS-EXE: BASE=0x{:08x} ENTRY=0x{:08x} LEN={}",
              base, entry, text_len);

        exe.assemble_loader();

        Ok(exe)
    }

    /// Assemble the code for the native loader whose purpose is to
    /// load the executable in RAM
    fn assemble_loader(&mut self) {
        let mut asm = Assembler::from_base(LOADER_ENTRY_ADDRESS);

        let irq_base = memory::map::IRQ_CONTROL.0;

        let cache_control = memory::map::CACHE_CONTROL.0;

        // First let's write a quick "clear_cache" function

        asm.assemble(&[
            // Let's mask all interrupts before we start, the
            // executable will be free to re-enable them.
            Li(T0, irq_base),
            Sh(R0, T0, 4),

            // Let's start by taking care of the "memfill" region
            Li(T0, self.memfill_base),
            Li(T1, self.memfill_len),
            // Skip memfil if len is 0
            Beqz(T1, Label::Local("memfill_done", 'f')),
            // Set T1 to the end address
            Add(T1, T0, T1),

            Local("memfill_loop"),
            // Do the memfill one byte at a time, not very efficient
            // but that way we don't have to worry about alignment
            Addiu(T0, T0, 1),
            Bne(T0, T1, Label::Local("memfill_loop", 'b')),
            Sb(R0, T0, -1),

            Local("memfill_done"),

            // Now we can move on to copying the code from the
            // EXE_FIFO
            Li(T0, self.base),
            Li(T1, self.text.len() as u32),

            // We should probably not have no text to copy, but let's
            // be cautious
            Beqz(T1, Label::Local("text_copy_done", 'f')),

            // Set T1 to the end address
            Add(T1, T0, T1),
            // T2 to the location of the EXE FIFO
            Li(T2, EXE_FIFO_ADDRESS),

            Local("text_copy_loop"),
            // Load next text byte
            Lb(T3, T2, 0),
            Addiu(T0, T0, 1),
            Bne(T0, T1, Label::Local("text_copy_loop", 'b')),
            Sb(T3, T0, -1),

            Local("text_copy_done"),

            // Call the "clear_cache" function
            Jal(Label::Global("clear_cache")),
            Nop,

            // Finally we can load the register values and jump into
            // the EXE
            Li(GP, self.initial_gp),
            Li(SP, self.initial_sp),
            Li(T0, self.entry),

            Jalr(RA, T0),
            Nop,

            // We're done, there's probably no point in returning to
            // the caller, let's loop infinitely. If we wanted to
            // return to the caller we'd have to save the registers
            // (particularly SP) before jumping into the EXE so that
            // we can restore them here.
            Local("infinite_loop"),
            B(Label::Local("infinite_loop", 'b')),
            Nop,

            // Clear cache function
            Global("clear_cache"),
            Addiu(SP, SP, -24),
            Sw(RA, SP, 20),
            Sw(FP, SP, 16),
            Move(FP, SP),

            // First we need to move to KSEG1 (uncached region)
            La(T0, Label::Local("uncached", 'f')),
            Lui(T1, 0xa000),
            Or(T0, T0, T1),
            Jr(T0),
            Nop,

            Local("uncached"),
            // We're now running from uncached memory

            Li(T0, cache_control),
            Lw(T5, T0, 0),
            // Enable i-cache, set "tag test mode"
            Li(T1, 0x804),
            Sw(T1, T0, 0),

            // Isolate the cache
            Mfc0(T1, 12),
            Li(T2, 0x00010000),
            Or(T1, T1, T2),
            Mtc0(T1, 12),

            // Write 0 to each 4th word from 0 to 4095 to invalidate
            // each cacheline in the 4KB i-cache.
            Li(T1, 0x1000),
            Li(T2, 0),

            Local("icache_invalidate_loop"),
            Addiu(T2, T2, 16),
            Bne(T2, T1, Label::Local("icache_invalidate_loop", 'b')),
            Sw(R0, T2, -16),

            // De-isolate the cache
            Mfc0(T1, 12),
            Li(T2, !0x00010000),
            And(T1, T1, T2),
            Mtc0(T1, 12),

            // Clear tag test mode
            Li(T1, 0x800),
            Sw(T1, T0, 0),

            // Re-isolate the cache
            Mfc0(T1, 12),
            Li(T2, 0x00010000),
            Or(T1, T1, T2),
            Mtc0(T1, 12),

            // Write 0 to each word from 0 to 4095 to invalidate each
            // word in the 4KB i-cache. I don't think this is truly
            // necessary but the BIOS does it.
            Li(T1, 0x1000),
            Li(T2, 0),

            Local("icache_zero_loop"),
            Addiu(T2, T2, 4),
            Bne(T2, T1, Label::Local("icache_zero_loop", 'b')),
            Sw(R0, T2, -4),

            // De-isolate the cache
            Mfc0(T1, 12),
            Li(T2, !0x00010000),
            And(T1, T1, T2),
            Mtc0(T1, 12),

            // Restore cache control
            Sw(T5, T0, 0),

            // Return
            Move(SP, FP),
            Lw(RA, SP, 20),
            Lw(FP, SP, 16),
            Jr(RA),
            Addiu(SP, SP, 24)
        ]).unwrap();

        let (mc, _) = asm.machine_code();

        //TODO: create loader
        //self.loader = mc;
    }

    pub fn region(&self) -> Option<Region> {
        self.region
    }

    /// Patch the BIOS animation jump to run the loader code
    /// instead. Returns an error if the patching failed.
    pub fn patch_bios(&self, bios: &mut Bios) {
        let mut asm = Assembler::from_base(0);

        // Assemble the jump instruction
        let instruction = Jal(Label::Absolute(LOADER_ENTRY_ADDRESS));

        asm.assemble(&[instruction]).unwrap();

        let (mc, _) = asm.machine_code();

        // It should only have generated a single instruction
        assert!(mc.len() == 4);

        // reassemble the instruction word
        let instruction = mc[0] as u32
            | ((mc[1] as u32) << 8)
            | ((mc[2] as u32) << 16)
            | ((mc[3] as u32) << 24);

        // Finally we can try to patch the BIOS
        bios.patch_animation_jump_hook(instruction);
    }
}

pub fn sideload(bus: &mut Bus) {
    if let Some(exe) = &bus.exe {
        bus.xmem.ram_store_block(exe.base, exe.text.as_slice(), exe.text.len());
        bus.cpu.set_reg(RegisterIndex(28), exe.initial_gp);
        if exe.initial_sp != 0 {
            bus.cpu.set_reg(RegisterIndex(29), exe.initial_sp);
            bus.cpu.set_reg(RegisterIndex(30), exe.initial_sp);
            bus.cpu.pc = exe.entry;
        }
    }
}

fn read_u32(f: &mut File) -> MipsResult<u32> {
    let mut b = [0; 4];

    f.read_exact(&mut b).unwrap();

    Ok(b[0] as u32
        | ((b[1] as u32) << 8)
        | ((b[2] as u32) << 16)
        | ((b[3] as u32) << 24))
}

/// Offset of the register containing the machine code FIFO for
/// loading the EXE
const EXE_FIFO_OFFSET: u32 = 0x100;

/// Absolute address of the register containing the machine code FIFO
/// for loading the EXE
const EXE_FIFO_ADDRESS: u32 = memory::map::EXPANSION_1.0 + EXE_FIFO_OFFSET;

/// Offset of the entry point for the loader code in the EXPANSION 1
/// memory range
const LOADER_ENTRY_OFFSET: u32 = 0x200;

/// Absolute address of the entry point for the loader code
const LOADER_ENTRY_ADDRESS: u32 =
    memory::map::EXPANSION_1.0 + LOADER_ENTRY_OFFSET;