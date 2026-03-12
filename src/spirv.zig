//load constants from spirv registry
//at compile time

const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const IR = @import("IR.zig");
const Spirv = @This();
const g = @import("spirvGrammar.zig");

allocator: Allocator,
words: List(u32) = .empty,
glsl_inst_set_id: u32 = undefined,

id: u32 = 1,

pub fn generate(ir: *IR, allocator: Allocator, settings: hgsl.Settings) Error!Result {
    if (ir.entry_points.items.len == 0)
        return .{ .bytes = &.{} };
    if (settings.target != .vulkan)
        @panic("target mismatch");

    var spirv: Spirv = .{ .allocator = allocator };
    //minimum 5 words for header
    try spirv.words.ensureTotalCapacity(allocator, 32);

    const vulkan_settings = settings.target.vulkan;
    //header: |spirv-magic|version|generator-magic|bound|***|
    spirv.words.appendSliceAssumeCapacity(
        &.{ g.spirv_magic, @intFromEnum(vulkan_settings.spirv_version), g.generator_magic, 0, 0 },
    );

    return .{
        .bytes = @ptrCast(@alignCast(try spirv.words.toOwnedSlice(allocator))),
    };
}

pub fn versionWord(major: u8, minor: u8) u32 {
    return (@as(u32, major) << 16) | (@as(u32, minor) << 8);
}
pub fn stringLiteral(comptime str: []const u8) [(str.len + 4) / 4]u32 {
    const num_words = (str.len + 4) / 4;
    var words: [num_words]u32 = @splat(0);
    for (str, 0..) |c, i|
        words[i / 4] |= @as(u32, c) << @intCast((i & 3) * 8);
    return words;
}

const Result = hgsl.Result;
const List = std.ArrayList;
const Error = hgsl.Error;
const Allocator = std.mem.Allocator;
