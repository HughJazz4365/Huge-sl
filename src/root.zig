const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");
const Generator = @import("Generator.zig");

pub fn compile(allocator: std.mem.Allocator, source: []const u8, output: *std.Io.Writer) !void {
    var tokenizer: Tokenizer = .new(source);
    var parser: Parser = try .parse(allocator, &tokenizer);
    defer parser.deinit();

    var generator: Generator = .new(&parser, allocator, output);
    try generator.generate();
}

pub fn compileFile(allocator: std.mem.Allocator, path: []const u8, output: *std.Io.Writer) !void {
    const source_file = try std.fs.cwd().openFile(path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    const source = alloc_writer.writer.buffered();
    try compile(allocator, source, output);
}
