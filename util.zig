// (C) 2021 Ronsor Labs.

pub inline fn tuplicate(a: u32, b: u32) u64 {
    return @as(u64, @intCast(a)) << 32 | @as(u64, @intCast(b));
}
