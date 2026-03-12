//load constants from spirv registry
//at compile time

const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const IR = @import("IR.zig");
const Spirv = @This();
const grammar = @import("spirvGrammar.zig");

const glsl_inst_set_id: u32 = 1;

allocator: Allocator,
output: List(u32) = .empty,

id: u32 = 2,

pub fn generate(ir: *IR, allocator: Allocator, settings: hgsl.Settings) Error![]u32 {
    var spirv: Spirv = .{ .allocator = allocator };
    _ = ir;
    _ = &spirv;
    _ = settings;
    return &.{};
}
pub fn stringLiteral(comptime str: []const u8) [(str.len + 4) / 4]u32 {
    const num_words = (str.len + 4) / 4;
    var words: [num_words]u32 = @splat(0);
    for (str, 0..) |c, i|
        words[i / 4] |= @as(u32, c) << @intCast((i & 3) * 8);
    return words;
}

const List = std.ArrayList;
const Error = hgsl.Error;
const Allocator = std.mem.Allocator;
