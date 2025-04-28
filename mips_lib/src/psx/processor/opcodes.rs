use log::{info, warn};
use crate::psx::bus::Bus;
use crate::psx::processor::cop0::Exception;
use crate::psx::processor::{cop0, cpu, ClockCycle, RegisterIndex};
use crate::psx::processor::cpu::{exception, load, store};
use crate::psx::processor::instruction::Instruction;

/// Handler table for the main opcodes (instruction bits [31:26])
#[rustfmt::skip]
pub const OPCODE_HANDLERS: [fn(&mut Bus, Instruction); 128] = [
    // 0x00
    op_function, op_bxx,      op_j,        op_jal,
    op_beq,      op_bne,      op_blez,     op_bgtz,
    op_addi,     op_addiu,    op_slti,     op_sltiu,
    op_andi,     op_ori,      op_xori,     op_lui,
    // 0x10
    op_cop0,     op_cop1,     op_cop2,     op_cop3,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    // 0x20
    op_lb,       op_lh,       op_lwl,      op_lw,
    op_lbu,      op_lhu,      op_lwr,      op_illegal,
    op_sb,       op_sh,       op_swl,      op_sw,
    op_illegal,  op_illegal,  op_swr,      op_illegal,
    // 0x30
    op_lwc0,     op_lwc1,     op_lwc2,     op_lwc3,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    op_swc0,     op_swc1,     op_swc2,     op_swc3,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,

    // This second half is called when an interrupt is active and `opcode_table_offset` is set to
    // 64. You'll notice that the interrupt code is called every time *except* for COP2 opcodes.
    // That's because GTE operations behave weirdly when at interrupt occurs. No$ says that they
    // get repeated, mednafen "cheats" and just postpone the interrupt if it was to occur on a GTE
    // instruction. Here we use mednafen's approach and ignore the interrupts for GTE operations.

    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,

    op_irq,      op_irq,      op_cop2,     op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,

    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,

    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
    op_irq,      op_irq,      op_irq,      op_irq,
];

/// Handler table for the function codes (instruction bits [31:26] when opcode is 0)
#[rustfmt::skip]
const FUNCTION_HANDLERS: [fn(&mut Bus, Instruction); 64] = [
    // 0x00
    op_sll,      op_illegal,  op_srl,      op_sra,
    op_sllv,     op_illegal,  op_srlv,     op_srav,
    op_jr,       op_jalr,     op_illegal,  op_illegal,
    op_syscall,  op_break,    op_illegal,  op_illegal,
    // 0x10
    op_mfhi,     op_mthi,     op_mflo,     op_mtlo,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    op_mult,     op_multu,    op_div,      op_divu,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    // 0x20
    op_add,      op_addu,     op_sub,      op_subu,
    op_and,      op_or,       op_xor,      op_nor,
    op_illegal,  op_illegal,  op_slt,      op_sltu,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    // 0x30
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
    op_illegal,  op_illegal,  op_illegal,  op_illegal,
];

pub fn run_instruction(bus: &mut Bus, i: Instruction) {
    let idx = i.opcode() | bus.cpu.opcode_table_offset as usize;
    let op = OPCODE_HANDLERS[idx];
    op(bus, i);
}

/// Handle pipeline timings for register dependencies. Should be called for every CPU registers
/// used as an input or output. Returns `r` to allow chaining.
fn reg_dep(bus: &mut Bus, r: RegisterIndex) -> RegisterIndex {
    // R0 is always "free" to read or write, so it doesn't force a sync when used as a register. In
    // order to emulate this we can just save and restore the value of the cycle counter for R0 to
    // make this function a NOP if `r` is R0 without having to use any branching.
    let c0 = bus.cpu.free_cycles[0];

    // If the register was executing a load we have to wait for it to complete before we can
    // continue (this is true even if `r` is used as an output register).
    bus.cpu.free_cycles[r.0 as usize] = 0;

    bus.cpu.free_cycles[0] = c0;

    r
}

/// When the main opcode is 0 we need to dispatch through a secondary table based on bits [5:0] of
/// the instruction
fn op_function(bus: &mut Bus, instruction: Instruction) {
    let handler = FUNCTION_HANDLERS[instruction.function()];

    handler(bus, instruction);
}

/// Shift Left Logical
///
/// `SLL $r0, $r0, 0` (machine code 0x0000_0000) is the idiomatic way of encoding a NOP
fn op_sll(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.shift();
    let t = reg_dep(bus, instruction.t());
    let d = reg_dep(bus, instruction.d());

    let v = bus.cpu.reg(t) << i;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Shift Right Logical
fn op_srl(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.shift();
    let t = reg_dep(bus, instruction.t());
    let d = reg_dep(bus, instruction.d());

    let v = bus.cpu.reg(t) >> i;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Shift Right Arithmetic
fn op_sra(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.shift();
    let t = reg_dep(bus, instruction.t());
    let d = reg_dep(bus, instruction.d());

    let v = (bus.cpu.reg(t) as i32) >> i;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v as u32);
}

/// Shift Left Logical Variable
fn op_sllv(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    // Shift amount is truncated to 5 bits
    let v = bus.cpu.reg(t) << (bus.cpu.reg(s) & 0x1f);

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Shift Right Logical Variable
fn op_srlv(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    // Shift amount is truncated to 5 bits
    let v = bus.cpu.reg(t) >> (bus.cpu.reg(s) & 0x1f);

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Shift Right Arithmetic Variable
fn op_srav(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    // Shift amount is truncated to 5 bits
    let v = (bus.cpu.reg(t) as i32) >> (bus.cpu.reg(s) & 0x1f);

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v as u32);
}

/// Jump Register
fn op_jr(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());

    bus.cpu.next_pc = bus.cpu.reg(s);
    bus.cpu.branch = true;

    bus.cpu.delayed_load();
}

/// Jump And Link Register
fn op_jalr(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let d = reg_dep(bus, instruction.d());

    let ra = bus.cpu.next_pc;

    bus.cpu.next_pc = bus.cpu.reg(s);
    bus.cpu.branch = true;

    bus.cpu.delayed_load();

    // Store return address in `d`
    bus.cpu.set_reg(d, ra);
}

/// System Call
fn op_syscall(bus: &mut Bus, _: Instruction) {
    exception(bus, Exception::SysCall);
}

/// Break
fn op_break(bus: &mut Bus, _: Instruction) {
    #[cfg(feature = "debugger")]
    {
        if bus.cpu.debug_on_break {
            info!("BREAK instruction while debug_on_break is active");
            //debugger::trigger_break(psx);
            return;
        }
    }
    exception(bus, Exception::Break);
}

/// Block if the current DIV(U) or MULT(U) instruction has not yet finished
fn sync_mult_div(bus: &mut Bus) {
    let block_for = bus.cpu.mult_div_end - bus.cycles;

    if block_for == 1 {
        // XXX timing hack from mednafen, if we only have one cycle left we ignore it. We should
        // really just implement proper timing from psxact
        return;
    }

    if block_for > 0 {
        bus.cycles = bus.cpu.mult_div_end;

        let ri = bus.cpu.free_cycles_reg.0 as usize;

        if ClockCycle::from(bus.cpu.free_cycles[ri]) <= block_for {
            bus.cpu.free_cycles[ri] = 0;
        } else {
            bus.cpu.free_cycles[ri] -= block_for as u8;
        }
    }
}

/// Move From HI
fn op_mfhi(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());

    let hi = bus.cpu.hi;

    bus.cpu.delayed_load();

    sync_mult_div(bus);

    bus.cpu.set_reg(d, hi);
}

/// Move to HI
fn op_mthi(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());

    bus.cpu.hi = bus.cpu.reg(s);

    bus.cpu.delayed_load();
}

/// Move From LO
fn op_mflo(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());

    let lo = bus.cpu.lo;

    bus.cpu.delayed_load();

    sync_mult_div(bus);

    bus.cpu.set_reg(d, lo);
}

/// Move to LO
fn op_mtlo(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());

    bus.cpu.lo = bus.cpu.reg(s);

    bus.cpu.delayed_load();
}

/// Multiplication timings, based on the number of leading zeroes in the first multiplicand.
///
/// XXX I don't know how accurate these timings are. Is it really only dependent on the magnitude
/// of the first multiplicand?
const MULT_TIMINGS: [u8; 33] = [
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 10, 10, 10, 10, 10, 10, 10, 10, 10, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7,
];

/// Multiply (signed)
fn op_mult(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let a = bus.cpu.reg(s) as i32;
    let b = bus.cpu.reg(t) as i32;

    let res = i64::from(a) * i64::from(b);
    let res = res as u64;

    bus.cpu.delayed_load();

    bus.cpu.hi = (res >> 32) as u32;
    bus.cpu.lo = res as u32;

    let timing_index = if a < 0 {
        (!a).leading_zeros()
    } else {
        a.leading_zeros()
    };

    let penalty = ClockCycle::from(MULT_TIMINGS[timing_index as usize]);

    bus.cpu.mult_div_end = bus.cycles + penalty;
}

/// Multiply Unsigned
fn op_multu(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let a = bus.cpu.reg(s);
    let b = bus.cpu.reg(t);

    let res = u64::from(a) * u64::from(b);

    bus.cpu.delayed_load();

    bus.cpu.hi = (res >> 32) as u32;
    bus.cpu.lo = res as u32;

    let penalty = ClockCycle::from(MULT_TIMINGS[a.leading_zeros() as usize]);

    bus.cpu.mult_div_end = bus.cycles + penalty;
}

/// Divide (signed)
fn op_div(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let n = bus.cpu.reg(s) as i32;
    let d = bus.cpu.reg(t) as i32;

    bus.cpu.delayed_load();

    if d == 0 {
        // Division by zero, results are bogus
        bus.cpu.hi = n as u32;

        if n >= 0 {
            bus.cpu.lo = 0xffff_ffff;
        } else {
            bus.cpu.lo = 1;
        }
    } else if n as u32 == 0x8000_0000 && d == -1 {
        // Result is not representable in a 32bit signed integer
        bus.cpu.hi = 0;
        bus.cpu.lo = 0x8000_0000;
    } else {
        bus.cpu.hi = (n % d) as u32;
        bus.cpu.lo = (n / d) as u32;
    }

    bus.cpu.mult_div_end = bus.cycles + 37;
}

/// Divide Unsigned
fn op_divu(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let n = bus.cpu.reg(s);
    let d = bus.cpu.reg(t);

    bus.cpu.delayed_load();

    if d == 0 {
        // Division by zero, results are bogus
        bus.cpu.hi = n;
        bus.cpu.lo = 0xffff_ffff;
    } else {
        bus.cpu.hi = n % d;
        bus.cpu.lo = n / d;
    }

    bus.cpu.mult_div_end = bus.cycles + 37;
}

/// Add and check for signed overflow
fn op_add(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());
    let d = reg_dep(bus, instruction.d());

    let s = bus.cpu.reg(s) as i32;
    let t = bus.cpu.reg(t) as i32;

    bus.cpu.delayed_load();

    match s.checked_add(t) {
        Some(v) => bus.cpu.set_reg(d, v as u32),
        None => exception(bus, Exception::Overflow),
    }
}

/// Add Unsigned
fn op_addu(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());
    let d = reg_dep(bus, instruction.d());

    let v = bus.cpu.reg(s).wrapping_add(bus.cpu.reg(t));

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Subtract and check for signed overflow
fn op_sub(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());
    let d = reg_dep(bus, instruction.d());

    let s = bus.cpu.reg(s) as i32;
    let t = bus.cpu.reg(t) as i32;

    bus.cpu.delayed_load();

    match s.checked_sub(t) {
        Some(v) => bus.cpu.set_reg(d, v as u32),
        None => exception(bus, Exception::Overflow),
    }
}

/// Subtract Unsigned
fn op_subu(bus: &mut Bus, instruction: Instruction) {
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());
    let d = reg_dep(bus, instruction.d());

    let v = bus.cpu.reg(s).wrapping_sub(bus.cpu.reg(t));

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Bitwise And
fn op_and(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let v = bus.cpu.reg(s) & bus.cpu.reg(t);

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Bitwise Or
fn op_or(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let v = bus.cpu.reg(s) | bus.cpu.reg(t);

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Bitwise Exclusive Or
fn op_xor(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let v = bus.cpu.reg(s) ^ bus.cpu.reg(t);

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Bitwise Not Or
fn op_nor(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let v = !(bus.cpu.reg(s) | bus.cpu.reg(t));

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v);
}

/// Set on Less Than (signed)
fn op_slt(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let s = bus.cpu.reg(s) as i32;
    let t = bus.cpu.reg(t) as i32;

    let v = s < t;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v as u32);
}

/// Set on Less Than Unsigned
fn op_sltu(bus: &mut Bus, instruction: Instruction) {
    let d = reg_dep(bus, instruction.d());
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let v = bus.cpu.reg(s) < bus.cpu.reg(t);

    bus.cpu.delayed_load();

    bus.cpu.set_reg(d, v as u32);
}

/// Various branch instructions: BGEZ, BLTZ, BGEZAL, BLTZAL. Bits [20:16] are used to figure out
/// which one to use
fn op_bxx(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = reg_dep(bus, instruction.s());

    let instruction = instruction.0;

    let is_bgez = (instruction >> 16) & 1;
    // It's not enough to test for bit 20 to see if we're supposed
    // to link, if any bit in the range [19:17] is set the link
    // doesn't take place and RA is left untouched.
    let is_link = (instruction >> 17) & 0xf == 0x8;

    let v = bus.cpu.reg(s) as i32;

    // Test "less than zero"
    let test = (v < 0) as u32;

    // If the test is "greater than or equal to zero" we need to
    // negate the comparison above ("a >= 0" <=> "!(a < 0)"). The
    // xor takes care of that.
    let test = test ^ is_bgez;

    bus.cpu.delayed_load();

    // If linking is requested it occurs unconditionally, even if
    // the branch is not taken
    if is_link {
        let ra = bus.cpu.next_pc;

        // Store return address in R31
        bus.cpu.set_reg(RegisterIndex(31), ra);
    }

    if test != 0 {
        bus.cpu.branch(i);
    }
}

/// Jump
fn op_j(bus: &mut Bus, instruction: Instruction) {
    let target = instruction.imm_jump();

    // In order to fit the immediate target in the instruction the bottom two bits are stripped
    // (see the implementation of `imm_jump`) but that still only leaves 26 bits to store 30 bits.
    // As a workaround the 4 MSBs are simply copied from the PC. That means that the effective
    // range of this instruction is limited and it can't reach any location in memory, in
    // particular it can't be used to switch from one area to an other (like, say, from KUSEG to
    // KSEG0).
    bus.cpu.next_pc = (bus.cpu.pc & 0xf000_0000) | target;
    bus.cpu.branch = true;

    bus.cpu.delayed_load();
}

/// Jump And Link
fn op_jal(bus: &mut Bus, instruction: Instruction) {
    let ra = bus.cpu.next_pc;
    let target = instruction.imm_jump();

    reg_dep(bus, RegisterIndex(31));

    bus.cpu.next_pc = (bus.cpu.pc & 0xf000_0000) | target;
    bus.cpu.branch = true;

    bus.cpu.delayed_load();

    // Store return address in R31
    bus.cpu.set_reg(RegisterIndex(31), ra);
}

/// Branch if Equal
fn op_beq(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    if bus.cpu.reg(s) == bus.cpu.reg(t) {
        bus.cpu.branch(i);
    }

    bus.cpu.delayed_load();
}

/// Branch if Not Equal
fn op_bne(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    if bus.cpu.reg(s) != bus.cpu.reg(t) {
        bus.cpu.branch(i);
    }

    bus.cpu.delayed_load();
}

/// Branch if Less than or Equal to Zero
fn op_blez(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = reg_dep(bus, instruction.s());

    let v = bus.cpu.reg(s) as i32;

    if v <= 0 {
        bus.cpu.branch(i);
    }

    bus.cpu.delayed_load();
}

/// Branch if Greater Than Zero
fn op_bgtz(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = reg_dep(bus, instruction.s());

    let v = bus.cpu.reg(s) as i32;

    if v > 0 {
        bus.cpu.branch(i);
    }

    bus.cpu.delayed_load();
}

/// Add Immediate and check for signed overflow
fn op_addi(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se() as i32;
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let s = bus.cpu.reg(s) as i32;

    bus.cpu.delayed_load();

    match s.checked_add(i) {
        Some(v) => bus.cpu.set_reg(t, v as u32),
        None => exception(bus, Exception::Overflow),
    }
}

/// Add Immediate Unsigned
fn op_addiu(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let v = bus.cpu.reg(s).wrapping_add(i);

    bus.cpu.delayed_load();

    bus.cpu.set_reg(t, v);
}

/// Set if Less Than Immediate (signed)
fn op_slti(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se() as i32;
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let v = (bus.cpu.reg(s) as i32) < i;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(t, v as u32);
}

/// Set if Less Than Immediate Unsigned
fn op_sltiu(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let s = reg_dep(bus, instruction.s());
    let t = reg_dep(bus, instruction.t());

    let v = bus.cpu.reg(s) < i;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(t, v as u32);
}

/// Bitwise And Immediate
fn op_andi(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let v = bus.cpu.reg(s) & i;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(t, v);
}

/// Bitwise Or Immediate
fn op_ori(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let v = bus.cpu.reg(s) | i;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(t, v);
}

/// Bitwise eXclusive Or Immediate
fn op_xori(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let v = bus.cpu.reg(s) ^ i;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(t, v);
}

/// Load Upper Immediate
fn op_lui(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm();
    let t = reg_dep(bus, instruction.t());

    // Low 16bits are set to 0
    let v = i << 16;

    bus.cpu.delayed_load();

    bus.cpu.set_reg(t, v);
}

/// Coprocessor 0 opcode
fn op_cop0(bus: &mut Bus, instruction: Instruction) {
    match instruction.cop_opcode() {
        0b00000 => op_mfc0(bus, instruction),
        0b00100 => op_mtc0(bus, instruction),
        0b10000 => op_rfe(bus, instruction),
        _ => panic!("Unhandled cop0 instruction {}", instruction),
    }
}

/// Move To Coprocessor 0
fn op_mtc0(bus: &mut Bus, instruction: Instruction) {
    // Mednafen doesn't force the register sync if a load was in progress here. It doesn't make a
    // lot of sense to me, maybe it's a mistake or maybe it compensates from something else. For
    // the time being just do whatever mednafen does.
    let cpu_r = instruction.t();
    let cop_r = instruction.d();

    let v = bus.cpu.reg(cpu_r);

    bus.cpu.delayed_load();

    cop0::mtc0(bus, cop_r, v);
}

/// Move From Coprocessor 0
fn op_mfc0(bus: &mut Bus, instruction: Instruction) {
    let cpu_r = reg_dep(bus, instruction.t());
    let cop_r = instruction.d();

    let v = cop0::mfc0(bus, cop_r);

    bus.cpu.delayed_load_chain(cpu_r, v, 0, false);
}

/// Return From Exception. Doesn't actually jump anywhere but tells the coprocessor to return to
/// the mode it was in when the exception occurred.
fn op_rfe(bus: &mut Bus, instruction: Instruction) {
    bus.cpu.delayed_load();

    // There are other instructions with the same encoding but all are virtual memory related and
    // the PlayStation doesn't implement them. Still, let's make sure we're not running buggy code.
    if instruction.0 & 0x3f != 0b01_0000 {
        panic!("Invalid cop0 instruction: {}", instruction);
    }

    cop0::return_from_exception(bus);
}

/// Coprocessor 1 opcode (does not exist on the PlayStation)
fn op_cop1(bus: &mut Bus, _: Instruction) {
    bus.cpu.delayed_load();

    warn!("Encountered Cop1 instruction");

    exception(bus, Exception::CoprocessorError);
}

/// Coprocessor 2 opcode (GTE)
fn op_cop2(bus: &mut Bus, instruction: Instruction) {
    if bus.cpu.gte_command_end > bus.cycles {
        bus.cycles = bus.cpu.gte_command_end;
    }

    // XXX: we should check that the GTE is enabled in cop0's status register, otherwise the cop2
    // instructions seem to freeze the CPU (or maybe raise an exception?). Furthermore it seems
    // that one has to wait at least two cycles (tested with two nops) after raising the flag in
    // the status register before the GTE can be accessed.
    let cop_opcode = instruction.cop_opcode();

    if cop_opcode & 0x10 != 0 {
        // GTE command

        bus.cpu.delayed_load();

        bus.cpu.gte_command_end = bus.cycles + bus.gte.command(instruction.0);
    } else {
        match cop_opcode {
            0b00000 => op_mfc2(bus, instruction),
            0b00010 => op_cfc2(bus, instruction),
            0b00100 => op_mtc2(bus, instruction),
            0b00110 => op_ctc2(bus, instruction),
            n => unimplemented!("GTE opcode {:x}", n),
        }
    }
}

/// Move From Coprocessor 2 Data register
fn op_mfc2(bus: &mut Bus, instruction: Instruction) {
    let block_for = bus.cpu.gte_command_end - bus.cycles;

    let delay = if block_for > 0 {
        bus.cycles = bus.cpu.gte_command_end;
        block_for as u8
    } else {
        0
    };

    let cpu_r = instruction.t();
    let cop_r = instruction.d().0;

    let v = bus.gte.data(cop_r);

    bus.cpu.delayed_load_chain(cpu_r, v, delay, false);
}

/// Move From Coprocessor 2 Control register
fn op_cfc2(bus: &mut Bus, instruction: Instruction) {
    let block_for = bus.cpu.gte_command_end - bus.cycles;

    let delay = if block_for > 0 {
        bus.cycles = bus.cpu.gte_command_end;
        block_for as u8
    } else {
        0
    };

    let cpu_r = instruction.t();
    let cop_r = instruction.d().0;

    let v = bus.gte.control(cop_r);

    bus.cpu.delayed_load_chain(cpu_r, v, delay, false);
}

/// Move To Coprocessor 2 Data register
fn op_mtc2(bus: &mut Bus, instruction: Instruction) {
    if bus.cpu.gte_command_end > bus.cycles {
        bus.cycles = bus.cpu.gte_command_end;
    }

    // Mednafen doesn't force the register sync if a load was in progress here. It doesn't make a
    // lot of sense to me, maybe it's a mistake or maybe it compensates from something else. For
    // the time being just do whatever mednafen does.
    let cpu_r = instruction.t();
    let cop_r = instruction.d().0;

    let v = bus.cpu.reg(cpu_r);

    bus.cpu.delayed_load();

    bus.gte.set_data(cop_r, v);
}

/// Move To Coprocessor 2 Control register
fn op_ctc2(bus: &mut Bus, instruction: Instruction) {
    if bus.cpu.gte_command_end > bus.cycles {
        bus.cycles = bus.cpu.gte_command_end;
    }

    // Mednafen doesn't force the register sync if a load was in progress here. It doesn't make a
    // lot of sense to me, maybe it's a mistake or maybe it compensates from something else. For
    // the time being just do whatever mednafen does.
    let cpu_r = instruction.t();
    let cop_r = instruction.d().0;

    let v = bus.cpu.reg(cpu_r);

    bus.cpu.delayed_load();

    bus.gte.set_control(cop_r, v);
}

/// Coprocessor 3 opcode (does not exist on the PlayStation)
fn op_cop3(bus: &mut Bus, _: Instruction) {
    bus.cpu.delayed_load();

    warn!("Encountered Cop3 instruction");

    exception(bus, Exception::CoprocessorError);
}

/// Load Byte (signed)
fn op_lb(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    let (v, duration) = load::<u8>(bus, addr, false);

    // Cast as i8 to force sign extension
    let v = v as i8;

    // Put the load in the delay slot
    bus.cpu.delayed_load_chain(t, v as u32, duration, true);
}

/// Load Halfword (signed)
fn op_lh(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    if addr % 2 == 0 {
        let (v, duration) = load::<u16>(bus, addr, false);

        // Cast as i16 to force sign extension
        let v = v as i16;

        // Put the load in the delay slot
        bus.cpu.delayed_load_chain(t, v as u32, duration, true);
    } else {
        bus.cpu.delayed_load();
        exception(bus, Exception::LoadAddressError);
    }
}

/// Load Word Left
fn op_lwl(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    let mut cur_v = bus.cpu.reg(t);

    // This instruction bypasses the load delay restriction: this instruction will merge the new
    // contents with the value currently being loaded if need be.
    if let Some((pending_reg, pending_value, _)) = bus.cpu.load {
        if pending_reg == t {
            cur_v = pending_value;
        }
    }

    // Next we load the *aligned* word containing the first addressed byte
    let aligned_addr = addr & !3;
    let (aligned_word, duration) = load::<u32>(bus, aligned_addr, false);

    // Depending on the address alignment we fetch the 1, 2, 3 or 4 *most* significant bytes and
    // put them in the target register.
    let v = match addr & 3 {
        0 => (cur_v & 0x00ff_ffff) | (aligned_word << 24),
        1 => (cur_v & 0x0000_ffff) | (aligned_word << 16),
        2 => (cur_v & 0x0000_00ff) | (aligned_word << 8),
        3 => aligned_word,
        _ => unreachable!(),
    };

    // Put the load in the delay slot
    bus.cpu.delayed_load_chain(t, v, duration, true);
}

/// Load Word
fn op_lw(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    // Address must be 32bit aligned
    if addr % 4 == 0 {
        let (v, duration) = load(bus, addr, false);

        bus.cpu.delayed_load_chain(t, v, duration, true);
    } else {
        bus.cpu.delayed_load();
        exception(bus, Exception::LoadAddressError);
    }
}

/// Load Byte Unsigned
fn op_lbu(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    let (v, duration) = load::<u8>(bus, addr, false);

    // Put the load in the delay slot
    bus.cpu.delayed_load_chain(t, u32::from(v), duration, true);
}

/// Load Halfword Unsigned
fn op_lhu(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    // Address must be 16bit aligned
    if addr % 2 == 0 {
        let (v, duration) = load::<u16>(bus, addr, false);

        // Put the load in the delay slot
        bus.cpu.delayed_load_chain(t, u32::from(v), duration, true);
    } else {
        bus.cpu.delayed_load();
        exception(bus, Exception::LoadAddressError);
    }
}

/// Load Word Right
fn op_lwr(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    let mut cur_v = bus.cpu.reg(t);

    // This instruction bypasses the load delay restriction: this instruction will merge the new
    // contents with the value currently being loaded if need be.
    if let Some((pending_reg, pending_value, _)) = bus.cpu.load {
        if pending_reg == t {
            cur_v = pending_value;
        }
    }

    // Next we load the *aligned* word containing the first addressed byte
    let aligned_addr = addr & !3;
    let (aligned_word, duration) = load::<u32>(bus, aligned_addr, false);

    // Depending on the address alignment we fetch the 1, 2, 3 or 4 *least* significant bytes and
    // put them in the target register.
    let v = match addr & 3 {
        0 => aligned_word,
        1 => (cur_v & 0xff00_0000) | (aligned_word >> 8),
        2 => (cur_v & 0xffff_0000) | (aligned_word >> 16),
        3 => (cur_v & 0xffff_ff00) | (aligned_word >> 24),
        _ => unreachable!(),
    };

    // Put the load in the delay slot
    bus.cpu.delayed_load_chain(t, v, duration, true);
}

/// Store Byte
fn op_sb(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);
    let v = bus.cpu.reg(t);

    bus.cpu.delayed_load();

    store(bus, addr, v as u8);
}

/// Store Halfword
fn op_sh(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);
    let v = bus.cpu.reg(t);

    bus.cpu.delayed_load();

    // Address must be 16bit aligned
    if addr % 2 == 0 {
        store(bus, addr, v as u16);
    } else {
        exception(bus, Exception::StoreAddressError);
    }
}

/// Store Word Left
fn op_swl(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);
    let v = bus.cpu.reg(t);

    let aligned_addr = addr & !3;
    // Load the current value for the aligned word at the target address
    let (cur, _) = load::<u32>(bus, aligned_addr, false);

    let new = match addr & 3 {
        0 => (cur & 0xffff_ff00) | (v >> 24),
        1 => (cur & 0xffff_0000) | (v >> 16),
        2 => (cur & 0xff00_0000) | (v >> 8),
        3 => v,
        _ => unreachable!(),
    };

    bus.cpu.delayed_load();

    store(bus, aligned_addr, new);
}

/// Store Word
fn op_sw(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);
    let v = bus.cpu.reg(t);

    bus.cpu.delayed_load();

    // Address must be 32bit aligned
    if addr % 4 == 0 {
        store(bus, addr, v);
    } else {
        exception(bus, Exception::StoreAddressError);
    }
}

/// Store Word Right
fn op_swr(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let t = reg_dep(bus, instruction.t());
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);
    let v = bus.cpu.reg(t);

    let aligned_addr = addr & !3;
    // Load the current value for the aligned word at the target address
    let (cur, _) = load::<u32>(bus, aligned_addr, false);

    let new = match addr & 3 {
        0 => v,
        1 => (cur & 0x0000_00ff) | (v << 8),
        2 => (cur & 0x0000_ffff) | (v << 16),
        3 => (cur & 0x00ff_ffff) | (v << 24),
        _ => unreachable!(),
    };

    bus.cpu.delayed_load();

    store(bus, aligned_addr, new);
}

/// Load Word in Coprocessor 0
fn op_lwc0(bus: &mut Bus, _: Instruction) {
    bus.cpu.delayed_load();

    warn!("Encountered LWC0 instruction");

    // Not supported by this coprocessor
    exception(bus, Exception::CoprocessorError);
}

/// Load Word in Coprocessor 1
fn op_lwc1(bus: &mut Bus, _: Instruction) {
    bus.cpu.delayed_load();

    warn!("Encountered LWC1 instruction");

    // Not supported by this coprocessor
    exception(bus, Exception::CoprocessorError);
}

/// Load Word in Coprocessor 2
fn op_lwc2(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let cop_r = instruction.t().0;
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    bus.cpu.delayed_load();

    // Address must be 32bit aligned
    if addr % 4 == 0 {
        // XXX how should we handle duration here? No absorb?
        let (v, _duration) = load::<u32>(bus, addr, true);

        bus.gte.set_data(cop_r, v);
    } else {
        exception(bus, Exception::LoadAddressError);
    }
}

/// Load Word in Coprocessor 3
fn op_lwc3(bus: &mut Bus, _: Instruction) {
    bus.cpu.delayed_load();

    warn!("Encountered LWC3 instruction");

    // Not supported by this coprocessor
    exception(bus, Exception::CoprocessorError);
}

/// Store Word in Coprocessor 0
fn op_swc0(bus: &mut Bus, _: Instruction) {
    bus.cpu.delayed_load();

    warn!("Encountered SWC0 instruction");

    // Not supported by this coprocessor
    exception(bus, Exception::CoprocessorError);
}

/// Store Word in Coprocessor 1
fn op_swc1(bus: &mut Bus, _: Instruction) {
    bus.cpu.delayed_load();

    warn!("Encountered SWC1 instruction");

    // Not supported by this coprocessor
    exception(bus, Exception::CoprocessorError);
}

/// Store Word in Coprocessor 2
fn op_swc2(bus: &mut Bus, instruction: Instruction) {
    let i = instruction.imm_se();
    let cop_r = instruction.t().0;
    let s = reg_dep(bus, instruction.s());

    let addr = bus.cpu.reg(s).wrapping_add(i);

    // XXX read from GTE
    let v = bus.gte.data(cop_r);

    bus.cpu.delayed_load();

    // Address must be 32bit aligned
    if addr % 4 == 0 {
        store(bus, addr, v);
    } else {
        exception(bus, Exception::LoadAddressError);
    }
}

/// Store Word in Coprocessor 3
fn op_swc3(bus: &mut Bus, _: Instruction) {
    bus.cpu.delayed_load();

    warn!("Encountered SWC3 instruction");

    // Not supported by this coprocessor
    exception(bus, Exception::CoprocessorError);
}

/// Illegal instruction
fn op_illegal(bus: &mut Bus, instruction: Instruction) {
    bus.cpu.delayed_load();

    warn!(
        "Illegal instruction {} at PC 0x{:08x}!",
        instruction, bus.cpu.current_pc
    );

    exception(bus, Exception::IllegalInstruction);
}

fn op_irq(bus: &mut Bus, _instruction: Instruction) {
    bus.cpu.delayed_load();

    exception(bus, Exception::Interrupt);
}
