const std = @import("std");

pub fn compile(allocator: std.mem.Allocator, source: []const u8, out: *std.Io.Writer) !void {
    _ = &.{ allocator, source, out };
    try out.printAscii(source, .{});
    try out.flush();
}
