const std = @import("std");
pub fn main() !void {
    std.debug.print("S: {d}\n", .{@sizeOf(@Vector(3, f64))});
    std.debug.print("b: {}\n", .{@as(f32, -0.0) == @as(f32, 0.0)});
    //2, f32 => 64
    //3-4, f32 => 128
    //2, f16 => 128
    //3-4, f16 => 128
}
fn f() void {
    return function();
}
fn function() void {
    std.debug.print("aerstnaroi\n", .{});
}
