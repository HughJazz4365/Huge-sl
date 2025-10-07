const std = @import("std");
const hgsl = @import("hgsl");

pub fn main() !void {
    var buf: [128]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&buf);
    _ = &out_writer;

    var timer = try std.time.Timer.start();

    // const path = "test.hgsl";
    const path = "source.hgsl";

    const allocator = std.heap.page_allocator;

    const compiled = try hgsl.compileFile(allocator, path, &out_writer.interface);
    defer allocator.free(compiled);
    const measure = timer.read();
    // _ = measure;
    std.debug.print("time {any} ms.\n", .{@as(f64, @floatFromInt(measure)) / 1_000_000});

    //write spirv binary to a file
    const out_file = try std.fs.cwd().openFile("out.spv", .{ .mode = .write_only });
    defer out_file.close();
    var out_buf: [128]u8 = undefined;
    var writer = out_file.writer(&out_buf);

    try writer.interface.writeSliceEndian(u32, compiled, .little);
    try writer.interface.flush();
}
