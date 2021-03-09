// (C) 2021 Ronsor Labs.
const tup = @import("util.zig").tuplicate;

pub const DecoderError = error {
    IllegalOpcode
};

pub const RType = struct {
    rs2: u5,
    rs1: u5,
    rd: u5,

    pub fn decode(raw: u32) RType {
        return .{.rs2 = @intCast(u5, (raw >> 20) & 0x1f), .rs1 = @intCast(u5, (raw >> 15) & 0x1f), .rd = @intCast(u5, (raw >> 7) & 0x1f)};
    }
};

pub const IType = struct {
    imm: u12,
    rs1: u5,
    rd: u5,

    pub fn decode(raw: u32) IType {
        return .{.imm = @intCast(u12, (raw >> 20)), .rs1 = @intCast(u5, (raw >> 15) & 0x1f), .rd = @intCast(u5, (raw >> 7) & 0x1f)};
    }

    pub inline fn immSigned(self: IType) i12 {
        return @bitCast(i12, self.imm);
    }
};

pub const SType = struct {
    imm: u12,
    rs1: u5,
    rs2: u5,

    pub fn decode(raw: u32) SType {
        return .{.imm = @intCast(u12, ((raw >> 20) & 0xfe0) | ((raw >> 7) & 0x1f)), .rs1 = @intCast(u5, (raw >> 15) & 0x1f), .rs2 = @intCast(u5, (raw >> 20) & 0x1f)};
    }

    pub inline fn immSigned(self: SType) i12 {
        return @bitCast(i12, self.imm);
    }
};

pub const BType = struct {
    imm: u12,
    rs1: u5,
    rs2: u5,

    pub fn decode(raw: u32) BType {
        return .{
            .imm = @intCast(u12, ((raw & 0x8000_0000) >> 19) | ((raw & 0x7e00_0000) >> 20) |
                   ((raw & 0x0000_0f00) >> 7) | ((raw & 0x0000_0080) << 4)),
            .rs1 = @intCast(u5, (raw >> 15) & 0x1f),
            .rs2 = @intCast(u5, (raw >> 20) & 0x1f),
        };
    }
};

pub const JType = struct {
    imm: u20,
    rd: u5,

    pub fn decode(raw: u32) JType {
        return .{
            .imm = @intCast(u20, ((raw & 0x8000_0000) >> 11) | ((raw & 0x7fe0_0000) >> 20) |
                   ((raw & 0x0010_0000) >> 9) | (raw & 0x000f_f000)),
            .rd = @intCast(u5, (raw >> 7) & 0x1f),
        };
    }
};

pub const UType = struct {
    imm: u32,
    rd: u5,

    pub fn decode(raw: u32) UType {
        return .{.imm = raw & 0xfffff000, .rd = @intCast(u5, (raw >> 7) & 0x1f)};
    }
};

pub const ShiftType = struct {
    shamt: u6,
    rs1: u5,
    rd: u5,

    pub fn decode(raw: u32) ShiftType {
        return .{.shamt = @intCast(u6, (raw >> 20) & 0x3f), .rs1 = @intCast(u5, (raw >> 15) & 0x1f), .rd = @intCast(u5, (raw >> 7) & 0x1f)};
    }
};

pub const Instruction = union(enum(u8)) {
    add: RType,
    sub: RType,
    xor: RType,
    @"or": RType,
    @"and": RType,
    sll: RType,
    srl: RType,
    sra: RType,
    slt: RType,
    sltu: RType,

    addi: IType,
    xori: IType,
    ori: IType,
    andi: IType,
    slli: IType,
    srli: IType,
    srai: IType,
    slti: IType,
    sltiu: IType,

    lb: IType,
    lh: IType,
    lw: IType,
    ld: IType,
    lbu: IType,
    lhu: IType,
    lwu: IType,

    sb: SType,
    sh: SType,
    sw: SType,
    sd: SType,

    beq: BType,
    bne: BType,
    blt: BType,
    bge: BType,
    bltu: BType,
    bgeu: BType,

    jal: JType,
    jalr: IType,

    lui: UType,
    auipc: UType,

    ecall: IType,

    addw: RType,
    subw: RType,
    sllw: RType,
    srlw: RType,
    sraw: RType,

    addiw: IType,
    slliw: ShiftType,
    srliw: ShiftType,
    sraiw: ShiftType,

    mul: RType,
    mulh: RType,
    mulsu: RType,
    mulu: RType,
    div: RType,
    divu: RType,
    rem: RType,
    remu: RType,

    pub fn init(op: anytype, val: anytype) Instruction {
        return @unionInit(Instruction, @tagName(op), val);
    }

    pub fn decode32(raw: u32) anyerror!Instruction {
        var opcode = (raw >> 2) & 0b11111;
        return switch (opcode) {
            0b01100 => decodeAluOp(raw),
            0b00100 => decodeAluImmOp(raw),
            0b00000 => decodeLoad(raw),
            0b01000 => decodeStore(raw),
            0b11000 => decodeBranch(raw),
            0b11011 => init(.jal, JType.decode(raw)),
            0b11001 => init(.jalr, IType.decode(raw)),
            0b01011 => init(.lui, UType.decode(raw)),
            0b00101 => init(.auipc, UType.decode(raw)),
            0b11100 => init(.ecall, IType.decode(raw)),
            0b01110 => decodeAluOp32(raw),
            0b00110 => decodeAluImmOp32(raw),
            else => DecoderError.IllegalOpcode,
        };
    }

    fn decodeAluOp(raw: u32) !Instruction {
        var funct7and3 = tup(raw >> 25, (raw >> 12) & 0b111);
        return switch (funct7and3) {
            tup(0b0000000, 0b000) => init(.add, RType.decode(raw)),
            tup(0b0100000, 0b000) => init(.sub, RType.decode(raw)),
            tup(0b0000000, 0b001) => init(.sll, RType.decode(raw)),
            tup(0b0000000, 0b010) => init(.slt, RType.decode(raw)),
            tup(0b0000000, 0b011) => init(.sltu, RType.decode(raw)),
            tup(0b0000000, 0b100) => init(.xor, RType.decode(raw)),
            tup(0b0000000, 0b101) => init(.srl, RType.decode(raw)),
            tup(0b0100000, 0b101) => init(.sra, RType.decode(raw)),
            tup(0b0000000, 0b110) => init(.@"or", RType.decode(raw)),
            tup(0b0000000, 0b111) => init(.@"and", RType.decode(raw)),
            tup(0b0000001, 0b000) => init(.mul, RType.decode(raw)),
            tup(0b0000001, 0b001) => init(.mulh, RType.decode(raw)),
            tup(0b0000001, 0b010) => init(.mulsu, RType.decode(raw)),
            tup(0b0000001, 0b011) => init(.mulu, RType.decode(raw)),
            tup(0b0000001, 0b100) => init(.div, RType.decode(raw)),
            tup(0b0000001, 0b101) => init(.divu, RType.decode(raw)),
            tup(0b0000001, 0b110) => init(.rem, RType.decode(raw)),
            tup(0b0000001, 0b111) => init(.remu, RType.decode(raw)),
            else => DecoderError.IllegalOpcode,
        };
    }

    fn decodeAluImmOp(raw: u32) !Instruction {
        var funct3 = (raw >> 12) & 0b111;
        return switch (funct3) {
            0b000 => init(.addi, IType.decode(raw)),
            0b001 => init(.slli, IType.decode(raw)),
            0b010 => init(.slti, IType.decode(raw)),
            0b011 => init(.sltiu, IType.decode(raw)),
            0b100 => init(.xori, IType.decode(raw)),
            0b101 => switch (raw >> 26) {
                0b000000 => init(.srli, IType.decode(raw)),
                0b010000 => init(.srai, IType.decode(raw)),
                else => DecoderError.IllegalOpcode,
            },
            0b110 => init(.ori, IType.decode(raw)),
            0b111 => init(.andi, IType.decode(raw)),
            else => DecoderError.IllegalOpcode,
        };
    }

    fn decodeLoad(raw: u32) !Instruction {
        var funct3 = (raw >> 12) & 0b111;
        return switch (funct3) {
            0b000 => init(.lb, IType.decode(raw)),
            0b001 => init(.lh, IType.decode(raw)),
            0b010 => init(.lw, IType.decode(raw)),
            0b011 => init(.ld, IType.decode(raw)),
            0b100 => init(.lbu, IType.decode(raw)),
            0b101 => init(.lhu, IType.decode(raw)),
            0b110 => init(.lwu, IType.decode(raw)),
            else => DecoderError.IllegalOpcode,
        };
    }

    fn decodeStore(raw: u32) !Instruction {
        var funct3 = (raw >> 12) & 0b111;
        return switch (funct3) {
            0b000 => init(.sb, SType.decode(raw)),
            0b001 => init(.sh, SType.decode(raw)),
            0b010 => init(.sw, SType.decode(raw)),
            0b011 => init(.sd, SType.decode(raw)),
            else => DecoderError.IllegalOpcode,
        };
    }

    fn decodeBranch(raw: u32) !Instruction {
        var funct3 = (raw >> 12) & 0b111;
        return switch (funct3) {
            0b000 => init(.beq, BType.decode(raw)),
            0b001 => init(.bne, BType.decode(raw)),
            0b100 => init(.blt, BType.decode(raw)),
            0b101 => init(.bge, BType.decode(raw)),
            0b110 => init(.bltu, BType.decode(raw)),
            0b111 => init(.bgeu, BType.decode(raw)),
            else => DecoderError.IllegalOpcode,
        };
    }

    fn decodeAluOp32(raw: u32) !Instruction {
        var funct7and3 = tup(raw >> 25, (raw >> 12) & 0b111);
        return switch (funct7and3) {
            tup(0b0000000, 0b000) => init(.addw, RType.decode(raw)),
            tup(0b0100000, 0b000) => init(.subw, RType.decode(raw)),
            tup(0b0000000, 0b001) => init(.sllw, RType.decode(raw)),
            tup(0b0000000, 0b101) => init(.srlw, RType.decode(raw)),
            tup(0b0100000, 0b101) => init(.sraw, RType.decode(raw)),
            else => DecoderError.IllegalOpcode,
        };
    }

    fn decodeAluImmOp32(raw: u32) DecoderError!Instruction {
        var funct3 = (raw >> 12) & 0b111;
        var funct7and3 = tup(raw >> 25, (raw >> 12) & 0b111);
        return switch (funct3) {
            0b000 => init(.addiw, IType.decode(raw)),
            else => switch (funct7and3) {
                 tup(0b0000000, 0b001) => init(.slliw, ShiftType.decode(raw)),
                 tup(0b0000000, 0b101) => init(.srliw, ShiftType.decode(raw)),
                 tup(0b0100000, 0b101) => init(.sraiw, ShiftType.decode(raw)),
                 else => DecoderError.IllegalOpcode,
            }
        };
    }

};

