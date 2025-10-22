const std = @import("std");
pub fn main() void {
    const T = @Vector(2, f64);
    const val: T = .{ 1, 2 };
    std.debug.print("size(t): {d}, align(T): {d}, {d}, {d}\n", .{
        @sizeOf(T),
        @alignOf(T),
        @intFromPtr(&val),
        @intFromPtr(&(val[1])),
    });
}
