const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const SpirvGen = @import("spirvgen.zig");
const ErrCtx = @import("errorctx.zig");

pub const Error = error{};
pub fn compileFile(
    allocator: Allocator,
    path: []const u8,
    err_writer: ?*std.Io.Writer,
) ![]u32 {
    const source = try readFile(allocator, path);
    return try compile(allocator, source, path, err_writer);
}

pub fn compile(
    allocator: Allocator,
    source: []const u8,
    path: []const u8,
    err_writer: ?*std.Io.Writer,
) ![]u32 {
    var error_ctx: ErrCtx = .{};
    error_ctx.init(source, path, err_writer);

    var tokenizer: Tokenizer = .new(source, &error_ctx);
    var parser = Parser.parse(allocator, &tokenizer) catch |err| return error_ctx.outputUpdateIfEmpty(err);
    defer parser.deinit();

    return try SpirvGen.generate(&parser);
    //   return &.{};
}
pub const Compiler = struct {
    allocator: Allocator,

    err_ctx: ErrCtx = .{},

    pub fn compileFile(self: *Compiler, path: []const u8) ![]u32 {
        const source = try readFile(self.allocator, path);

        // const hash = std.hash.Fnv1a_128.hash(source);
        // std.debug.print("HASH: {d}\n", .{hash});

        self.err_ctx.reinit(source, path);
        var tokenizer: Tokenizer = .new(source, &self.err_ctx);
        var parser = Parser.parse(self.allocator, &tokenizer) catch |err| return self.err_ctx.outputUpdateIfEmpty(err);
        defer parser.deinit();

        return try SpirvGen.generate(&parser);
    }
    pub fn new(allocator: ?Allocator, err_writer: ?*std.Io.Writer) Compiler {
        return .{
            .allocator = if (allocator) |a| a else std.heap.page_allocator,
            .err_ctx = .{ .out_writer = err_writer },
        };
    }
};
pub const minimal =
    \\const vert = entrypoint(.vertex){}
    \\const frag = entrypoint(.fragment){out col: vec4 = .{1,0,1,0}}
;
const Allocator = std.mem.Allocator;
pub fn readFile(allocator: Allocator, path: []const u8) ![]const u8 {
    const source_file = try std.fs.cwd().openFile(path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    return alloc_writer.writer.buffered();
}
