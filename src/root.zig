const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");
const SpirvGen = @import("spirvgen.zig");

pub fn compileFile(
    allocator: std.mem.Allocator,
    path: []const u8,
    err_writer: ?*std.Io.Writer,
) ![]u32 {
    const source_file = try std.fs.cwd().openFile(path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    const source = alloc_writer.writer.buffered();
    return try compile(allocator, source, path, err_writer);
}

pub fn compile(
    allocator: std.mem.Allocator,
    source: []const u8,
    path: []const u8,
    err_writer: ?*std.Io.Writer,
) ![]u32 {
    var error_ctx: @import("errorctx.zig") = .{};
    error_ctx.init(source, path, err_writer);

    var tokenizer: Tokenizer = .new(source, &error_ctx);
    var parser = Parser.parse(allocator, &tokenizer) catch |err| return error_ctx.outputUpdateIfEmpty(err);

    defer parser.deinit();

    // var generator: SpirvGen = .new(&parser);
    // return try generator.generate();
    return &.{};
}
