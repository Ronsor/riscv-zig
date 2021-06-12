// (C) 2021 Ronsor Labs.

const std = @import("std");
const decoder = @import("decoder.zig");
const opfunc = @import("opfunc.zig");

pub const debug = false;

pub const CPUError = error{
    OutOfBoundsAccess,
    IllegalOpcode,
};

pub const CPUConfig = struct {
    // Abstract pointer to data that the memRead/memWrite functions may use
    memCookie: ?*c_void = null,
    // Reads buf.length bytes of memory
    memRead: ?fn (config: *CPUConfig, addr: CPU.XLEN, buf: []u8) anyerror!void = null,
    // Writes buf.length bytes of memory
    memWrite: ?fn (config: *CPUConfig, addr: CPU.XLEN, buf: []u8) anyerror!void = null,
};

fn simpleMemRead(config: *CPUConfig, addr: CPU.XLEN, buf: []u8) !void {
    const slice = @ptrCast(*[]u8, @alignCast(@alignOf(*[]u8), config.memCookie.?)).*;
    const off = @intCast(usize, addr);
    const size = buf.len;
    if (off + size > slice.len) return CPUError.OutOfBoundsAccess;
    std.mem.copy(u8, buf, slice[off .. off + size]);
}

fn simpleMemWrite(config: *CPUConfig, addr: CPU.XLEN, buf: []u8) !void {
    const slice = @ptrCast(*[]u8, @alignCast(@alignOf(*[]u8), config.memCookie.?)).*;
    const off = @intCast(usize, addr);
    const size = buf.len;
    if (off + size > slice.len) return CPUError.OutOfBoundsAccess;
    std.mem.copy(u8, slice[off .. off + size], buf);
}

/// Enable simple memory I/O functions backed by a []u8
pub fn useSimpleMemIO(config: *CPUConfig, memory: *[]u8) void {
    config.memCookie = @ptrCast(*c_void, memory);
    config.memRead = simpleMemRead;
    config.memWrite = simpleMemWrite;
}

// CPU instruction cache
const CPUCache = struct {
    // Opcode function pointers and instruction data
    funcs: ?[]opfunc.OpFunc = null,
    inst: ?[]decoder.Instruction = null,

    // Range of compiled instructions.
    base_pc: CPU.XLEN = 0,
    max_pc: CPU.XLEN = 0,

    // Maximum number of instructions to execute and
    // number of instructions executed (per call to execute())
    max_exec: usize = 0,
    exec_counter: usize = 0,
};

pub const CPU = struct {
    pub const XLEN = u64;
    pub const SXLEN = i64;

    pub const util = struct {
        // Before anyone complains about the name, this is the standard abbreviation.
        /// Sign extension.
        pub inline fn sext(in: anytype) XLEN {
            return @bitCast(XLEN, @intCast(SXLEN, @bitCast(std.meta.Int(.signed, @bitSizeOf(@TypeOf(in))), in)));
        }
    };

    allocator: *std.mem.Allocator,
    config: CPUConfig,

    registers: [32]XLEN = undefined,
    pc: XLEN,
    cache: CPUCache,

    /// Create and configure a CPU object.
    pub fn init(allocator: *std.mem.Allocator, config: CPUConfig) CPU {
        return .{
            .allocator = allocator,
            .config = config,
            .registers = std.mem.zeroes([32]XLEN),
            .pc = 0,
            .cache = .{},
        };
    }

    /// Dump the CPU state.
    pub fn dump(self: *CPU) void {
        std.debug.print("PC={}\n", .{self.pc});
        var instr: u32 = self.getMem(self.pc, u32, .fetch_instruction) catch 0;
        std.debug.print("> {x:08}\n", .{instr});
        for (self.registers[0..]) |val, i| {
            std.debug.print("reg: x{} = {}\n", .{ i, val });
        }
    }

    /// Set the value of a register.
    pub inline fn setReg(self: *CPU, reg: u5, val: XLEN) void {
        if (reg != 0) self.registers[@intCast(usize, reg)] = val;
    }

    /// Get the value of a register.
    /// Per specification, x0 is always 0.
    pub inline fn getReg(self: *CPU, reg: u5) XLEN {
        if (reg == 0) return 0;
        return self.registers[@intCast(usize, reg)];
    }

    /// Same as getReg(), but returns a signed integer
    pub inline fn getRegSigned(self: *CPU, reg: u5) SXLEN {
        return @bitCast(SXLEN, self.getReg(reg));
    }

    /// Store *val*, a *size* integer, in the CPU's configured memory.
    /// *size* must be either i8/u8, i16/u16, i32/u32, or i64/u64.
    pub inline fn setMem(self: *CPU, addr: XLEN, val: anytype) !void {
        // TODO: handle endians
        var t: @TypeOf(val) = val;
        var b = @ptrCast(*[@sizeOf(@TypeOf(val))]u8, &t);
        try self.config.memWrite.?(&self.config, addr, b[0..]);
    }

    /// Retrieve a *size* integer from the CPU's configured memory.
    /// *size* must be either i8/u8, i16/u16, i32/u32, or i64/u64.
    pub inline fn getMem(self: *CPU, addr: u64, comptime size: type, flags: anytype) !size {
        var b: [@sizeOf(size)]u8 = undefined;
        try self.config.memRead.?(&self.config, addr, b[0..]);
        return @bitCast(size, switch (size) {
            u8, i8 => b[0],
            u16, i16 => b,
            u32, i32 => b,
            u64, i64 => b,
            else => unreachable,
        });
    }

    /// Execute up to *max* CPU instructions.
    pub fn execute(self: *CPU, max: usize) !usize {
        self.cache.funcs = try self.allocator.alloc(opfunc.OpFunc, max);
        defer self.allocator.free(self.cache.funcs.?);
        self.cache.inst = try self.allocator.alloc(decoder.Instruction, max);
        defer self.allocator.free(self.cache.inst.?);

        self.cache.base_pc = self.pc;

        // Instruction decoding
        // We try to decode as many instructions as possible, up to *max*.
        // If an instruction can't be decoded, then we stop decoding.
        // *i* contains the number of instructions actually decoded.
        var i: usize = 0;
        var failed: anyerror = undefined;
        while (i < max) {
            var rawInst = self.getMem(self.pc +% @intCast(XLEN, i) * 4, u32, .fetch_instruction) catch |err| {
                failed = err;
                break;
            };
            self.cache.inst.?[i] = decoder.Instruction.decode32(rawInst) catch |err| {
                failed = err;
                break;
            };

            //if (debug) std.debug.print("execute: decoded and 'compiled' {}\n", .{self.cache.inst.?[i]});

            self.cache.funcs.?[i] = opfunc.fromInstruction(self.cache.inst.?[i]) catch |err| {
                failed = err;
                break;
            };
            i += 1;
        }
        // Couldn't decode any instructions? That's an error
        if (i == 0) return failed;
        self.cache.max_pc = self.cache.base_pc + @intCast(u64, i) * 4;

        self.cache.max_exec = max;
        self.cache.exec_counter = 0;

        try self.cache.funcs.?[0](self, self.cache.inst.?[0..i], 0);
        return self.cache.exec_counter;
    }
};

// These tests ensure that sample programs provided produce the correct output
const expect = std.testing.expect;

test "asm.simple" {
    var memory = std.mem.zeroes([4096]u8);
    var memSlice: []u8 = memory[0..];
    std.mem.copy(u8, memSlice, @embedFile("test/asm/simple.bin"));

    // Configure CPU to use memory provided by memSlice
    var conf: CPUConfig = undefined;
    useSimpleMemIO(&conf, &memSlice);

    var cpu = CPU.init(std.testing.allocator, conf);
    // Execute 32 instructions
    _ = try cpu.execute(32);

    try expect(cpu.getReg(10) == 1);
}

test "c.test1" {
    var memory = std.mem.zeroes([4096]u8);
    var memSlice: []u8 = memory[0..];
    std.mem.copy(u8, memSlice, @embedFile("test/c/test1.bin"));

    var conf: CPUConfig = undefined;
    useSimpleMemIO(&conf, &memSlice);

    var cpu = CPU.init(std.testing.allocator, conf);
    // Set up stack pointer
    cpu.setReg(2, 4096);

    _ = try cpu.execute(32);

    try expect(cpu.getReg(10) == 13);
}

test "c.test2" {
    var memory = std.mem.zeroes([4096]u8);
    var memSlice: []u8 = memory[0..];
    std.mem.copy(u8, memSlice, @embedFile("test/c/test2.bin"));

    var conf: CPUConfig = undefined;
    useSimpleMemIO(&conf, &memSlice);

    var cpu = CPU.init(std.testing.allocator, conf);
    cpu.setReg(2, 4096);

    _ = try cpu.execute(64);

    try expect(cpu.getReg(10) == 54);
}
