mod command;
mod fifo;
mod idct_matrix;
mod macroblock;
mod output_buffer;
mod test;
mod util;

use std::cmp::min;
use crate::bitwise::Bitwise;
use crate::psx::addressable::{AccessWidth, Addressable};
use crate::psx::bus::Bus;
use crate::psx::mdec::command::Command;
use crate::psx::mdec::fifo::Fifo;
use crate::psx::mdec::idct_matrix::IdctMatrix;
use crate::psx::mdec::macroblock::{Macroblock, MacroblockCoeffs};
use crate::psx::mdec::output_buffer::OutputBuffer;
use crate::psx::mdec::util::{quantize, yuv_to_rgb};
use crate::psx::processor::ClockCycle;
use crate::psx::sync;

const MDECSYNC: sync::SyncToken = sync::SyncToken::MDec;

#[derive(serde::Serialize, serde::Deserialize, Copy, Clone, PartialEq, Eq, Debug)]
enum BlockType {
    Y1 = 0,
    Y2 = 1,
    Y3 = 2,
    Y4 = 3,
    /// Luma (Y) for monochrome, Cr otherwise
    CrMono = 4,
    Cb = 5,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum OutputDepth {
    D4 = 0,
    D8 = 1,
    D15 = 3,
    D24 = 2,
}


#[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Debug)]
enum State {
    Idle,
    HandleCommand,
    LoadQuantTable(u8),
    LoadIdctMatrix,
    Decoding,
    StallOutput,
    OutputBlock,
}

impl State {
    fn is_busy(&self) -> bool {
        !matches!(self, State::Idle)
    }
}

/// Motion Decoder (sometimes called macroblock or movie decoder).
#[derive(serde::Serialize, serde::Deserialize)]
pub struct MDec {
    /// Current state of the decoder
    state: State,
    /// Last received command
    command: Command,
    /// Input FIFO
    input_fifo: Fifo,
    /// Output FIFO
    output_fifo: Fifo,
    /// Output buffer containing a fully decoded block before the data is pushed to the
    /// `output_fifo` (since the FIFO is not deep enough to hold a full block)
    output_buffer: OutputBuffer,
    /// Remaining words expected for this command
    command_remaining: u16,
    dma_in_enabled: bool,
    dma_out_enabled: bool,
    /// Quantization matrices: 8x8 bytes, first 64bytes for luma, second 64bits for chroma.
    #[serde(with = "serde_big_array::BigArray")]
    quant_matrices: [u8; 128],
    idct_matrix: IdctMatrix,
    current_block: BlockType,
    /// Index in the current macroblock
    block_index: u8,
    /// Quantization factor for the current macroblock, used for the AC values only.
    qscale: u8,
    /// 16bit coefficients during macroblock decoding
    block_coeffs: MacroblockCoeffs,
    /// Buffer for the currently decoded luma macroblock.
    block_y: Macroblock,
    /// Buffer for the currently decoded Cb macroblock.
    block_u: Macroblock,
    /// Buffer for the currently decoded Cr macroblock.
    block_v: Macroblock,
    /// Rough emulation of the decoder time budget: new blocks cannot be decoded while it's <= 0
    decoder_cycle_budget: ClockCycle,
    /// DMA block line length in 32bit words
    dma_block_line_length: u8,
    /// Block line offset for DMA read
    dma_block_line: u8,
    /// Block column offset for DMA read
    dma_block_column: u8,
}

impl MDec {
    pub fn new() -> MDec {
        MDec {
            state: State::Idle,
            command: Command(0),
            input_fifo: Fifo::new(),
            output_fifo: Fifo::new(),
            output_buffer: OutputBuffer::new(),
            command_remaining: 0,
            dma_in_enabled: false,
            dma_out_enabled: false,
            quant_matrices: [0; 128],
            idct_matrix: IdctMatrix::new(),
            current_block: BlockType::CrMono,
            block_index: 0,
            qscale: 0,
            block_coeffs: MacroblockCoeffs::new(),
            block_y: Macroblock::new(),
            block_u: Macroblock::new(),
            block_v: Macroblock::new(),
            decoder_cycle_budget: 0,
            dma_block_line_length: 0,
            dma_block_line: 0,
            dma_block_column: 0,
        }
    }

    pub fn is_busy(&self) -> bool {
        self.state.is_busy()
    }

    pub fn run(&mut self, cycles: ClockCycle) {
        // My test setup decided to give up as I was about to do MDEC timing tests, so I just
        // lifted the code from Mednafen instead even though it's obviously not super accurate.
        //
        // I gather that the difficulty comes from the fact that MDEC decoding is pipelined to some
        // extent, so the timing to decode one lone block are not the same as decoding a bunch of
        // blocks back-to-back.
        self.decoder_cycle_budget += cycles;
        if self.decoder_cycle_budget > 128 {
            self.decoder_cycle_budget = 128;
        }

        loop {
            match self.state {
                State::Idle => {
                    if self.input_fifo.is_empty() {
                        return;
                    }

                    let next_word = self.input_fifo.pop();

                    self.command = Command(next_word);
                    self.decoder_cycle_budget -= 1;

                    self.state = State::HandleCommand;
                }
                State::HandleCommand => {
                    if self.decoder_cycle_budget <= 0 {
                        return;
                    }

                    match self.command.opcode() {
                        1 => {
                            // Decode macroblock
                            self.output_fifo.clear();
                            self.current_block = BlockType::CrMono;
                            self.block_index = 0;
                            self.command_remaining = self.command.block_len();
                            self.output_buffer.clear();
                            self.state = State::Decoding;

                            self.dma_block_line_length = match self.command.output_depth() {
                                OutputDepth::D4 | OutputDepth::D8 => 0,
                                OutputDepth::D15 => 4,
                                OutputDepth::D24 => 6,
                            };
                            self.dma_block_line = 0;
                            self.dma_block_column = self.dma_block_line_length;
                        }
                        2 => {
                            // Load quantization matrices
                            self.command_remaining =
                                if self.command.quant_color() { 32 } else { 16 };

                            self.state = State::LoadQuantTable(0);
                        }
                        3 => {
                            self.command_remaining = 32;
                            self.state = State::LoadIdctMatrix;
                        }
                        o => unimplemented!("MDEC command {:x}", o),
                    }
                }
                State::LoadQuantTable(index) => {
                    if self.input_fifo.is_empty() {
                        return;
                    }
                    let next_word = self.input_fifo.pop();

                    let matrix = (index >> 4) as usize;
                    let pos = ((index & 0xf) << 2) as usize;

                    for i in 0..4 {
                        let b = (next_word >> (i * 8)) as u8;

                        self.quant_matrices[(matrix << 6) + pos + i] = b;
                    }

                    self.command_remaining = self.command_remaining.wrapping_sub(1);
                    self.state = if self.command_remaining == 0 {
                        State::Idle
                    } else {
                        State::LoadQuantTable(index + 1)
                    };
                }
                State::LoadIdctMatrix => {
                    if self.input_fifo.is_empty() {
                        return;
                    }
                    let next_word = self.input_fifo.pop();

                    let index = (32 - self.command_remaining) as u8;

                    let index = index * 2;

                    let c1 = next_word as i16;
                    let c2 = (next_word >> 16) as i16;

                    // XXX The loss of precision in the bitshift looks suspicious to me but that's
                    // what mednafen does. Probably worth investigating on the real hardware.
                    self.idct_matrix.set(index, c1 >> 3);
                    self.idct_matrix.set(index + 1, c2 >> 3);

                    self.command_remaining = self.command_remaining.wrapping_sub(1);
                    if self.command_remaining == 0 {
                        self.state = State::Idle;
                    }
                }
                State::Decoding => {
                    if self.input_fifo.is_empty() {
                        return;
                    }

                    let next_word = self.input_fifo.pop();
                    self.command_remaining = self.command_remaining.wrapping_sub(1);

                    self.decode_rle(next_word as u16);
                    self.decode_rle((next_word >> 16) as u16);

                    if !self.output_buffer.is_empty() {
                        self.state = State::StallOutput;
                    } else if self.command_remaining == 0 {
                        self.state = State::Idle;
                    }
                }
                State::StallOutput => {
                    // I don't know if this is truly necessary or if it's just a side effect of
                    // mednafen's macro coding.
                    if self.decoder_cycle_budget <= 0 {
                        return;
                    }
                    self.state = State::OutputBlock;
                }
                State::OutputBlock => {
                    if self.output_fifo.is_full() {
                        return;
                    }

                    self.output_fifo.push(self.output_buffer.pop_word());

                    if self.output_buffer.is_empty() {
                        self.output_buffer.clear();
                        self.state = if self.command_remaining > 0 {
                            State::Decoding
                        } else {
                            State::Idle
                        };
                    }
                }
            }
        }
    }

    pub fn push_command(&mut self, cmd: u32) {
        if self.input_fifo.is_full() {
            unimplemented!("Input FIFO overflow");
        }

        self.input_fifo.push(cmd);
    }

    pub fn set_control(&mut self, control: u32) {
        if control >> 31 != 0 {
            // Reset
            self.input_fifo.clear();
            self.output_fifo.clear();
            self.state = State::Idle;
            self.command_remaining = 0;
            self.block_index = 0;
        }

        self.dma_out_enabled = (control & (1 << 29)) != 0;
        self.dma_in_enabled = (control & (1 << 30)) != 0;
    }

    pub fn status(&self) -> u32 {
        let mut r = 0u32;

        // Bits [15:0] contain the number of remaining parameter words minus 1, or 0xffff if no
        // parameter is expected.
        r |= u32::from(self.command_remaining.wrapping_sub(1));

        // TODO [16:18] Current block

        // Data output format for the current command
        r |= self.command.output_format() << 23;

        r.set_bit(27, self.dma_can_read());
        r.set_bit(28, self.dma_can_write());

        r.set_bit(29, self.state.is_busy());
        r.set_bit(30, !self.input_fifo.is_full());
        r.set_bit(31, !self.output_fifo.is_empty());

        r
    }

    pub fn dma_can_write(&self) -> bool {
        // XXX From Mednafen, does it really only work if the FIFO is empty?
        // -> Probably yes, since the DMA wants to write 0x20 words at once (block size)
        self.input_fifo.is_empty()
            && self.dma_in_enabled
            && self.state.is_busy()
            && self.command_remaining > 0
    }

    pub fn dma_can_read(&self) -> bool {
        // XXX From Mednafen, does it really only work if the FIFO is full?
        // -> Probably yes, since the DMA wants to read 0x20 words at once (block size)
        self.output_fifo.is_full() && self.dma_out_enabled
    }

    fn decode_rle(&mut self, rle: u16) {
        if self.block_index == 0 {
            if rle == 0xfe00 {
                // This is normally the end-of-block marker but if it occurs before the start of a
                // block it's just padding and we should ignore it.
                return;
            }

            // The first value in the block is the DC value (low 10 bits) and the AC quantization
            // scaling factor (high 6 bits)
            self.qscale = (rle >> 10) as u8;
            let dc = rle & 0x3ff;

            let dc = quantize(dc, self.quantization(), None);
            self.next_block_coeff(dc);
            self.block_index = 1;
        } else {
            if rle == 0xfe00 {
                // End-of-block marker
                while self.block_index < 8 * 8 {
                    self.next_block_coeff(0);
                }
            } else {
                // Decode RLE encoded block
                let zeroes = rle >> 10;
                let ac = rle & 0x3ff;

                // Fill the zeroes
                for _ in 0..zeroes {
                    self.next_block_coeff(0);
                }

                // Compute the value of the AC coefficient
                let ac = quantize(ac, self.quantization(), Some(self.qscale));
                self.next_block_coeff(ac);
            }

            if self.block_index == 8 * 8 {
                // Block full, moving on
                self.decode_block();
            }
        }
    }

    /// Return the quantization factor for the current block_index
    fn quantization(&self) -> u8 {
        let index = self.block_index as usize;

        let matrix = if self.command.is_monochrome() {
            0
        } else {
            match self.current_block {
                BlockType::CrMono | BlockType::Cb => 1,
                _ => 0,
            }
        };

        self.quant_matrices[(matrix << 6) + index]
    }

    /// Set the value of the current block coeff pointed to by `block_index` and increment
    /// `block_index`.
    fn next_block_coeff(&mut self, coeff: i16) {
        self.block_coeffs.set_zigzag(self.block_index, coeff);
        self.block_index += 1;
    }

    fn decode_block(&mut self) {
        if self.command.is_monochrome() {
            self.idct_matrix.idct(&self.block_coeffs, &mut self.block_y);

            unimplemented!();
        } else {
            let finished_block = self.current_block;

            let generate_pixels = {
                let (idct_target, generate_pixels, next_block) = match self.current_block {
                    BlockType::Y1 => (&mut self.block_y, true, BlockType::Y2),
                    BlockType::Y2 => (&mut self.block_y, true, BlockType::Y3),
                    BlockType::Y3 => (&mut self.block_y, true, BlockType::Y4),
                    BlockType::Y4 => (&mut self.block_y, true, BlockType::CrMono),
                    BlockType::CrMono => (&mut self.block_v, false, BlockType::Cb),
                    BlockType::Cb => (&mut self.block_u, false, BlockType::Y1),
                };

                self.idct_matrix.idct(&self.block_coeffs, idct_target);

                self.current_block = next_block;

                generate_pixels
            };

            // Taken straight from mednafen. Original comment reads:
            //
            //   Timing in the PS1 MDEC is complex due to (apparent) pipelining, but the average
            //   when decoding a large number of blocks is about 512.
            self.decoder_cycle_budget -= 512;

            if generate_pixels {
                // We have Y, U and V macroblocks, we can convert the value and generate RGB pixels
                if self.command.output_depth() == OutputDepth::D15 {
                    self.generate_pixels_rgb15(finished_block);
                } else {
                    self.generate_pixels_rgb24(finished_block);
                }
            }
        }

        // We're ready for the next block's coefficients
        self.block_index = 0;
    }

    fn generate_pixels_rgb15(&mut self, block_type: BlockType) {
        let t = block_type as usize;

        let mut xor_mask = if self.command.output_signed() {
            // By changing the MSB of every component we effectively subtract 0x80, therefore
            // "centering" the value around 0.
            (1 << 4) | (1 << (5 + 4)) | (1 << (10 + 4))
        } else {
            0
        };

        // This should be a simple OR but since we know that the MSB is going to be zero we can
        // combine it into the XOR for the same result
        xor_mask |= (self.command.d15_msb() as u16) << 15;

        for y in 0..8 {
            let uv_y = (y >> 1) | (t & 2) << 1;
            let uv_x = (t & 1) << 2;

            for x in 0..8 {
                let uv_x = uv_x + (x >> 1);

                let l = self.block_y[y * 8 + x];
                let u = self.block_u[uv_y * 8 + uv_x];
                let v = self.block_v[uv_y * 8 + uv_x];

                let (r, g, b) = yuv_to_rgb(l, u, v);

                // Convert to RGB555 with rounding
                let r = min((u16::from(r) + 4) >> 3, 0x1f);
                let g = min((u16::from(g) + 4) >> 3, 0x1f);
                let b = min((u16::from(b) + 4) >> 3, 0x1f);

                let v = (r | (g << 5) | (b << 10)) ^ xor_mask;

                self.output_buffer.push_halfword(v);
            }
        }
    }

    fn generate_pixels_rgb24(&mut self, block_type: BlockType) {
        let t = block_type as usize;

        // By changing the MSB of every component we effectively subtract 0x80, therefore
        // "centering" the value around 0.
        let xor_mask = (self.command.output_signed() as u8) << 7;

        for y in 0..8 {
            let uv_y = (y >> 1) | (t & 2) << 1;
            let uv_x = (t & 1) << 2;

            for x in 0..8 {
                let uv_x = uv_x + (x >> 1);

                let l = self.block_y[y * 8 + x];
                let u = self.block_u[uv_y * 8 + uv_x];
                let v = self.block_v[uv_y * 8 + uv_x];

                let (r, g, b) = yuv_to_rgb(l, u, v);

                self.output_buffer.push_byte(r ^ xor_mask);
                self.output_buffer.push_byte(g ^ xor_mask);
                self.output_buffer.push_byte(b ^ xor_mask);
            }
        }
    }
}

pub fn run(bus: &mut Bus) {
    let elapsed = sync::resync(bus, MDECSYNC);

    bus.mdec.run(elapsed);

    // Since we don't have any IRQs we don't have to actually schedule an event, so I just set one
    // at a low frequency here just to prevent the sync counter from overflowing when we're
    // eventually called.
    sync::next_event(bus, MDECSYNC, 1_000_000);
}

pub fn store<T: Addressable>(bus: &mut Bus, off: u32, val: T) {
    run(bus);

    if T::width() != AccessWidth::Word {
        unimplemented!("Unhandled MDEC store ({:?})", T::width());
    }

    let val = val.as_u32();

    match off {
        0 => {
            bus.mdec.push_command(val);
            if !bus.mdec.is_busy() && bus.mdec.decoder_cycle_budget < 1 {
                bus.mdec.decoder_cycle_budget = 1;
            }
        }
        4 => bus.mdec.set_control(val),
        _ => unimplemented!("Unhandled MDEC store: {:08x} {:08x}", off, val),
    }
}

pub fn load<T: Addressable>(bus: &mut Bus, off: u32) -> T {
    run(bus);

    if T::width() != AccessWidth::Word {
        unimplemented!("Unhandled MDEC load ({:?})", T::width());
    }

    let v = match off {
        4 => bus.mdec.status(),
        _ => unimplemented!("Unhandled MDEC load: {:08x}", off),
    };

    T::from_u32(v)
}

pub fn dma_can_write(bus: &mut Bus) -> bool {
    run(bus);

    bus.mdec.dma_can_write()
}

pub fn dma_store(bus: &mut Bus, v: u32) {
    run(bus);

    bus.mdec.push_command(v);
}

pub fn dma_can_read(bus: &mut Bus) -> bool {
    run(bus);

    bus.mdec.dma_can_read()
}

/// Retuns the word being loaded as well as the offset in VRAM
pub fn dma_load(bus: &mut Bus) -> (u32, u32) {
    let mdec = &mut bus.mdec;

    debug_assert!(!mdec.output_fifo.is_empty());

    let v = mdec.output_fifo.pop();

    let line = mdec.dma_block_line as u32;
    let line_length = mdec.dma_block_line_length as u32;

    let mut offset = (line & 7) * line_length;

    if line & 8 != 0 {
        offset = offset.wrapping_sub(line_length * 7);
    }

    mdec.dma_block_column -= 1;
    if mdec.dma_block_column == 0 {
        mdec.dma_block_column = mdec.dma_block_line_length;
        mdec.dma_block_line = mdec.dma_block_line.wrapping_add(1);
    }

    // Run to keep feeding the output FIFO if we still have data
    run(bus);

    (v, offset << 2)
}
