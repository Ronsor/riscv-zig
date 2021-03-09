// (C) 2021 Ronsor Labs.

pub inline fn tuplicate(a: u32, b: u32) u64 {
    return @intCast(u64, a) << 32 | @intCast(u64, b);
}
