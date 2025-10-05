const std = @import("std");
const sl = @import("sl");
const shaderc = @import("shaderc.zig");

pub fn main() !void {
    var buf: [128]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&buf);

    var timer = try std.time.Timer.start();

    const path = "test.hgsl";
    // const path = "source.hgsl";

    const allocator = std.heap.page_allocator;
    const compiled = try sl.compileFile(allocator, path);

    const out_file = try std.fs.cwd().openFile("out.spv", .{ .mode = .write_only });
    defer out_file.close();
    var out_buf: [128]u8 = undefined;
    var writer = out_file.writer(&out_buf);

    try writer.interface.writeSliceEndian(u32, compiled, .little);
    try writer.interface.flush();

    defer allocator.free(compiled);

    try out_writer.interface.print("compiled: {any}\n", .{compiled});
    try out_writer.interface.flush();
    const measure = timer.read();
    // _ = measure;
    std.debug.print("time {any} ms.\n", .{@as(f64, @floatFromInt(measure)) / 1_000_000});

    if (false)
        std.debug.print("dissasembly:\n{s}\n", .{try shaderc.glslSpirvDissasembly(
            "test.glsl",
            .vertex,
            "main",
            // true,
            false,
        )});
}
pub fn kek() !void {
    const out_file = try std.fs.cwd().openFile("out.spv", .{ .mode = .read_write });
    defer out_file.close();
    var buf: [128]u8 = undefined;

    const source_file = try std.fs.cwd().openFile("source.sl", .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    const source = alloc_writer.writer.buffered();

    // var out_writer = out_file.writer(&buf);
    var out_writer = std.fs.File.stdout().writer(&buf);

    var timer = try std.time.Timer.start();
    try sl.compile(std.heap.page_allocator, source, &out_writer.interface);
    const measure = timer.read();
    // _ = measure;
    std.debug.print("time {d} ms.\n", .{@as(f64, @floatFromInt(measure)) / 1_000_000});
}
