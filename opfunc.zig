// (C) 2021 Ronsor Labs.

const std = @import("std");
const decoder = @import("decoder.zig");
const CPU = @import("cpu.zig").CPU;
const Instruction = decoder.Instruction;

pub const OpFuncError = error{
    Unimplemented,
};

pub const OpFunc = fn (cpu: *CPU, inst: []Instruction, cur: usize) anyerror!void;

/// Execute next instruction.
pub inline fn next(cpu: *CPU, inst: []Instruction, oldcur: usize) !void {
    var cur = oldcur +% 1;
    cpu.pc +%= 4;
    cpu.cache.exec_counter += 1;

    if (cpu.cache.exec_counter > cpu.cache.max_exec) return;
    if (cur == cpu.cache.inst.?.len) return;
    return @call(.{}, cpu.cache.funcs.?[cur], .{ cpu, inst, cur });
}

/// Attempt to "goto" cached instruction, otherwise return as usual.
pub inline fn goto(cpu: *CPU, inst: []Instruction, oldcur: usize, newpc: CPU.XLEN) !void {
    // TODO: support compressed instructions
    if (newpc >= cpu.cache.base_pc and newpc < cpu.cache.max_pc and (newpc % 4) == 0) {
        cpu.pc = newpc -% 4;
        return next(cpu, inst, @truncate(usize, (newpc - cpu.cache.base_pc) / 4) -% 1);
    }

    cpu.pc = newpc;
    cpu.cache.exec_counter += 1;
    return;
}

// -- ALU functions with only registers --

pub fn add(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].add;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) +% cpu.getReg(params.rs2));
    return next(cpu, inst, cur);
}

pub fn sub(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sub;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) -% cpu.getReg(params.rs2));
    return next(cpu, inst, cur);
}

pub fn xor(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].xor;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) ^ cpu.getReg(params.rs2));
    return next(cpu, inst, cur);
}

pub fn @"or"(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].@"or";
    cpu.setReg(params.rd, cpu.getReg(params.rs1) | cpu.getReg(params.rs2));
    return next(cpu, inst, cur);
}

pub fn @"and"(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].@"and";
    cpu.setReg(params.rd, cpu.getReg(params.rs1) & cpu.getReg(params.rs2));
    return next(cpu, inst, cur);
}

pub fn sll(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sll;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) << @truncate(u6, cpu.getReg(params.rs2)));
    return next(cpu, inst, cur);
}

pub fn srl(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].srl;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) >> @truncate(u6, cpu.getReg(params.rs2)));
    return next(cpu, inst, cur);
}

pub fn sra(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sra;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) >> @truncate(u6, cpu.getReg(params.rs2)));
    return next(cpu, inst, cur);
}

// -- ALU functions with an immediate value --
// Most of these functions are pretty monotonous, always following the same format
// Immediates are always sign-extended, unless otherwise specified
// at least that was my understanding of the specification.

pub fn addi(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].addi;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()));
    return next(cpu, inst, cur);
}

pub fn xori(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].xori;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) ^ CPU.util.sext(params.immSigned()));
    return next(cpu, inst, cur);
}

pub fn ori(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].ori;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) | CPU.util.sext(params.immSigned()));
    return next(cpu, inst, cur);
}

pub fn andi(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].andi;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) & CPU.util.sext(params.immSigned()));
    return next(cpu, inst, cur);
}

pub fn slli(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].slli;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) << @intCast(u6, params.imm & 0b1111));
    return next(cpu, inst, cur);
}

pub fn srli(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].srli;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) >> @intCast(u6, params.imm & 0b1111));
    return next(cpu, inst, cur);
}

pub fn srai(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].srai;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) >> @intCast(u6, params.imm & 0b1111));
    return next(cpu, inst, cur);
}

pub fn slti(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].slti;
    // Why @bitCast() & @intCast()? RISC-V manual: "immediates are always sign extended"
    cpu.setReg(params.rd, if (cpu.getRegSigned(params.rs1) < @intCast(CPU.SXLEN, params.immSigned())) 1 else 0);
    return next(cpu, inst, cur);
}

pub fn sltiu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sltiu;
    // Explicitly unsigned, so only zero-extend
    // Since this isn't hardware, a simple @intCast() works
    cpu.setReg(params.rd, if (cpu.getReg(params.rs1) < @intCast(CPU.XLEN, params.imm)) 1 else 0);
    return next(cpu, inst, cur);
}

// -- Load/store --

pub fn lb(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].lb;
    cpu.setReg(params.rd, CPU.util.sext(try cpu.getMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), i8, .none)));
    return next(cpu, inst, cur);
}

pub fn lh(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].lh;
    cpu.setReg(params.rd, CPU.util.sext(try cpu.getMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), i16, .none)));
    return next(cpu, inst, cur);
}

pub fn lw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].lw;
    cpu.setReg(params.rd, CPU.util.sext(try cpu.getMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), i32, .none)));
    return next(cpu, inst, cur);
}

pub fn ld(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].ld;
    cpu.setReg(params.rd, CPU.util.sext(try cpu.getMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), i64, .none)));
    return next(cpu, inst, cur);
}

pub fn lbu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].lbu;
    cpu.setReg(params.rd, @intCast(CPU.XLEN, try cpu.getMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), u8, .none)));
    return next(cpu, inst, cur);
}

pub fn lhu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].lhu;
    cpu.setReg(params.rd, @intCast(CPU.XLEN, try cpu.getMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), u16, .none)));
    return next(cpu, inst, cur);
}

pub fn lwu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].lhu;
    cpu.setReg(params.rd, @intCast(CPU.XLEN, try cpu.getMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), u32, .none)));
    return next(cpu, inst, cur);
}

pub fn sb(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sb;
    try cpu.setMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), @truncate(u8, cpu.getReg(params.rs2)));
    return next(cpu, inst, cur);
}

pub fn sh(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sh;
    try cpu.setMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), @truncate(u16, cpu.getReg(params.rs2)));
    return next(cpu, inst, cur);
}

pub fn sw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sw;
    try cpu.setMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), @truncate(u32, cpu.getReg(params.rs2)));
    return next(cpu, inst, cur);
}

pub fn sd(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sd;
    try cpu.setMem(cpu.getReg(params.rs1) +% CPU.util.sext(params.immSigned()), @truncate(u64, cpu.getReg(params.rs2)));
    return next(cpu, inst, cur);
}

// -- Branch functions --

pub fn beq(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].beq;
    if (cpu.getReg(params.rs1) == cpu.getReg(params.rs2)) return goto(cpu, inst, cur, cpu.pc +% @intCast(CPU.XLEN, params.imm));
    return next(cpu, inst, cur);
}

pub fn bne(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].bne;
    if (cpu.getReg(params.rs1) != cpu.getReg(params.rs2)) return goto(cpu, inst, cur, cpu.pc +% @intCast(CPU.XLEN, params.imm));
    return next(cpu, inst, cur);
}

pub fn blt(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].blt;
    if (cpu.getRegSigned(params.rs1) < cpu.getRegSigned(params.rs2)) return goto(cpu, inst, cur, cpu.pc +% @intCast(CPU.XLEN, params.imm));
    return next(cpu, inst, cur);
}

pub fn bge(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].bge;
    if (cpu.getRegSigned(params.rs1) > cpu.getRegSigned(params.rs2)) return goto(cpu, inst, cur, cpu.pc +% @intCast(CPU.XLEN, params.imm));
    return next(cpu, inst, cur);
}

pub fn bltu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].bltu;
    if (cpu.getReg(params.rs1) < cpu.getReg(params.rs2)) return goto(cpu, inst, cur, cpu.pc +% @intCast(CPU.XLEN, params.imm));
    return next(cpu, inst, cur);
}

pub fn bgeu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].bgeu;
    if (cpu.getReg(params.rs1) > cpu.getReg(params.rs2)) return goto(cpu, inst, cur, cpu.pc +% @intCast(CPU.XLEN, params.imm));
    return next(cpu, inst, cur);
}

// -- Jump functions --

pub fn jal(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].jal;
    cpu.setReg(params.rd, cpu.pc + 4);
    return goto(cpu, inst, cur, cpu.pc +% @intCast(CPU.XLEN, params.imm));
}

pub fn jalr(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].jalr;
    cpu.setReg(params.rd, cpu.pc + 4);
    return goto(cpu, inst, cur, cpu.getReg(params.rs1) +% @intCast(CPU.XLEN, params.imm));
}

// -- Misc functions --

pub fn lui(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].lui;
    cpu.setReg(params.rd, CPU.util.sext(params.imm));
    return next(cpu, inst, cur);
}

pub fn auipc(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].auipc;
    cpu.setReg(params.rd, cpu.pc + CPU.util.sext(params.imm));
    return next(cpu, inst, cur);
}

// -- Specifically 32-bit ALU functions --

pub fn addw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].addw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) +% @truncate(u32, cpu.getReg(params.rs2))));
    return next(cpu, inst, cur);
}

pub fn subw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].subw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) -% @truncate(u32, cpu.getReg(params.rs2))));
    return next(cpu, inst, cur);
}

pub fn sllw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sllw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) << @truncate(u5, cpu.getReg(params.rs2))));
    return next(cpu, inst, cur);
}

pub fn srlw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].srlw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) >> @truncate(u5, cpu.getReg(params.rs2))));
    return next(cpu, inst, cur);
}

pub fn sraw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sraw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) >> @truncate(u5, cpu.getReg(params.rs2))));
    return next(cpu, inst, cur);
}

// -- 32-bit ALU functions with an immediate value --

pub fn addiw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].addiw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) +% @bitCast(u32, @intCast(i32, params.immSigned()))));
    return next(cpu, inst, cur);
}

pub fn slliw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].slliw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) << @truncate(u5, params.shamt)));
    return next(cpu, inst, cur);
}

pub fn srliw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].srliw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) >> @truncate(u5, params.shamt)));
    return next(cpu, inst, cur);
}

pub fn sraiw(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].sraiw;
    cpu.setReg(params.rd, CPU.util.sext(@truncate(u32, cpu.getReg(params.rs1)) >> @truncate(u5, params.shamt)));
    return next(cpu, inst, cur);
}

// -- Multiplication (M) extension functions --

pub fn mul(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].mul;
    cpu.setReg(params.rd, @intCast(CPU.XLEN, @truncate(u32, cpu.getReg(params.rs1) *% cpu.getReg(params.rs2))));
    return next(cpu, inst, cur);
}

pub fn mulh(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].mulh;
    cpu.setReg(params.rd, (@bitCast(CPU.XLEN, cpu.getRegSigned(params.rs1) *% cpu.getRegSigned(params.rs2)) & 0xFFFFFFFF_00000000) >> 32);
    return next(cpu, inst, cur);
}

pub fn mulsu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].mulsu;
    cpu.setReg(params.rd, @intCast(CPU.XLEN, ((cpu.getReg(params.rs1) *% cpu.getReg(params.rs2)) & 0xFFFFFFFF_00000000) >> 32));
    return next(cpu, inst, cur);
}

pub fn mulu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].mulu;
    cpu.setReg(params.rd, @intCast(CPU.XLEN, ((cpu.getReg(params.rs1) *% cpu.getReg(params.rs2)) & 0xFFFFFFFF_00000000) >> 32));
    return next(cpu, inst, cur);
}

pub fn div(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].div;
    cpu.setReg(params.rd, @bitCast(CPU.XLEN, try std.math.divFloor(i64, cpu.getRegSigned(params.rs1), cpu.getRegSigned(params.rs2))));
    return next(cpu, inst, cur);
}

pub fn divu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].divu;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) / cpu.getReg(params.rs2));
    return next(cpu, inst, cur);
}

pub fn rem(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].rem;
    cpu.setReg(params.rd, @bitCast(CPU.XLEN, try std.math.rem(i64, cpu.getRegSigned(params.rs1), cpu.getRegSigned(params.rs2))));
    return next(cpu, inst, cur);
}

pub fn remu(cpu: *CPU, inst: []Instruction, cur: usize) !void {
    const params = inst[cur].remu;
    cpu.setReg(params.rd, cpu.getReg(params.rs1) % cpu.getReg(params.rs2));
    return next(cpu, inst, cur);
}

// -- Utilities --

/// Get the function corresponding to an instruction.
pub fn fromInstruction(inst: Instruction) !OpFunc {
    return switch (inst) {
        .add => add,
        .sub => sub,
        .xor => xor,
        .@"or" => @"or",
        .@"and" => @"and",
        .sll => sll,
        .srl => srl,
        .sra => sra,

        .addi => addi,
        .xori => xori,
        .ori => ori,
        .andi => andi,
        .slli => slli,
        .srli => srli,
        .srai => srai,
        .slti => slti,
        .sltiu => sltiu,

        .lb => lb,
        .lh => lh,
        .lw => lw,
        .ld => ld,
        .lbu => lbu,
        .lhu => lhu,
        .lwu => lwu,

        .sb => sb,
        .sh => sh,
        .sw => sw,
        .sd => sd,

        .beq => beq,
        .bne => bne,
        .blt => blt,
        .bge => bge,
        .bltu => bltu,
        .bgeu => bgeu,

        .jal => jal,
        .jalr => jalr,

        .lui => lui,
        .auipc => auipc,

        .addw => addw,
        .subw => subw,
        .sllw => sllw,
        .srlw => srlw,
        .sraw => sraw,

        .addiw => addiw,
        .slliw => slliw,
        .srliw => srliw,
        .sraiw => sraiw,

        .mul => mul,
        .mulh => mulh,
        .mulsu => mulsu,
        .mulu => mulu,
        .div => div,
        .divu => divu,
        .rem => rem,
        .remu => remu,

        else => OpFuncError.Unimplemented,
    };
}
