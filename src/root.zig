const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");
const SpirvGen = @import("spirvgen.zig");

pub fn compileFile(allocator: std.mem.Allocator, path: []const u8) ![]u32 {
    const source_file = try std.fs.cwd().openFile(path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    const source = alloc_writer.writer.buffered();
    return try compile(allocator, source, path);
}

pub fn compile(allocator: std.mem.Allocator, source: []const u8, path: []const u8) ![]u32 {
    var error_ctx: @import("errorctx.zig") = .{};
    error_ctx.init(source, path);

    var tokenizer: Tokenizer = .new(source, &error_ctx);
    var parser = Parser.parse(allocator, &tokenizer) catch return error_ctx.outputReturnErr();

    defer parser.deinit();

    // var generator: SpirvGen = .new(&parser);
    // return try generator.generate();
    return &.{};
}
