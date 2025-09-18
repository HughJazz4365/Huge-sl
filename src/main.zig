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
    var tokenizer: Tokenizer = .new(source);

    var parser: Parser = .{};
    parser.init(std.heap.page_allocator, &tokenizer);
    defer parser.deinit();

    try parser.testicle();
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
