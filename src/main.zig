const std = @import("std");
const sl = @import("sl");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {
    const source_path = "source.sl";
    const source_file = try std.fs.cwd().openFile(source_path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    // const source = "const kek = 1 + -52.8 * 9\n";
    const source = alloc_writer.writer.buffered();

    var timer = try std.time.Timer.start();
    var tokenizer: Tokenizer = .new(source);
    // var token = try tokenizer.next();
    // std.debug.print("{f}\n", .{token});

    // while (token != .eof) {
    //     token = try tokenizer.next();
    //     std.debug.print("{f}\n", .{token});
    // }

    // if (true) return;

    var parser: Parser = .{};
    parser.init(std.heap.page_allocator, &tokenizer);
    defer parser.deinit();

    try parser.testicle();

    const measure = timer.read();
    _ = measure;
    // std.debug.print("time {d} ms.\n", .{@as(f64, @floatFromInt(measure)) / 1_000_000});
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

    var writer = out_file.writer(&buf);
    // var writer = std.fs.File.stdout().writer(&buf);

    try sl.compile(std.heap.page_allocator, source, &writer.interface);
}
