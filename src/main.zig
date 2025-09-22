const std = @import("std");
const sl = @import("sl");

pub fn main() !void {
    var buf: [128]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&buf);

    var timer = try std.time.Timer.start();

    const path = "source.sl";
    try sl.compileFile(std.heap.page_allocator, path, &out_writer.interface);

    const measure = timer.read();
    // _ = measure;
    std.debug.print("time {d} ms.\n", .{@as(f64, @floatFromInt(measure)) / 1_000_000});
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
