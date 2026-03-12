//load constants from spirv registry
//at compile time

const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const IR = @import("IR.zig");
const Spirv = @This();
const grammar = @import("spirvGrammar.zig");

allocator: Allocator,
output: List(u32) = .empty,

id: u32 = 1,

pub fn generate(ir: *IR, allocator: Allocator, settings: hgsl.Settings) Error![]u32 {
    var spirv: Spirv = .{ .allocator = allocator };
    _ = ir;
    _ = &spirv;
    _ = settings;
    return &.{};
}

const List = std.ArrayList;
const Error = hgsl.Error;
const Allocator = std.mem.Allocator;
