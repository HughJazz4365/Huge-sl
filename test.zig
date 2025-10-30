const std = @import("std");
pub fn main() !void {
    std.debug.print("S: {d}\n", .{@sizeOf(@Vector(3, f64))});
    std.debug.print("b: {b}\n", .{@as(u32, @bitCast(@as(f32, -0.0)))});
    //2, f32 => 64
    //3-4, f32 => 128
    //2, f16 => 128
    //3-4, f16 => 128

}
