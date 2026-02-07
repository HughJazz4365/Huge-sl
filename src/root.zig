const std = @import("std");
const util = @import("util.zig");
const error_message = @import("errorMessage.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub const CI = i128;
pub const CF = f64;

pub const Stage = enum { fragment, vertex, compute };

pub const Error = error{
    OutOfMemory,
    FileReadFailed,
    WriteFailed,

    SyntaxError,
    ParsingError,
};

pub fn test_() !void {
    inline for (&[_]type{
        Parser.NodeEntry,
        Parser.StructEntry,
        Parser.ScopeEntry,
        Parser.TypeEntry,
        Parser.FunctionEntry,
        Tokenizer.TokenEntry,
    }) |_| {}
    // }) |T|
    // std.debug.print("size of {s}: {d}\n", .{ @typeName(T), @sizeOf(T) });

    var threaded_io = std.Io.Threaded.init_single_threaded;
    const io = threaded_io.io();
    const allocator = std.heap.page_allocator;

    var buf: [128]u8 = undefined;
    var file_writer = std.Io.File.stdout().writer(io, &buf);

    const path = "test.hgsl";
    const source = readFile(io, allocator, path) catch
        return Error.FileReadFailed;

    var timer = try std.time.Timer.start();
    var tok: Tokenizer = .{ .full_source = source, .path = path };
    tok.tokenize(allocator) catch |err| {
        try error_message.printErrorMessage(&file_writer.interface, tok.error_info, tok);
        return err;
    };

    var p = try Parser.new(allocator);
    p.parse(tok) catch |err| {
        try error_message.printErrorMessage(&file_writer.interface, p.error_info, tok);
        return err;
    };
    const measure = timer.read();
    std.debug.print(
        "time: {d} mcs\n",
        .{@as(f64, @floatFromInt(measure)) / 1_000.0},
    );

    p.dump();
    // _ = p;

}
pub fn compile(io: std.Io, allocator: Allocator, path: []const u8, error_writer: *std.Io.Writer) Error![]u32 {
    const source = readFile(io, allocator, path) catch
        return Error.FileReadFailed;
    defer allocator.free(source);

    var parser = try Parser.new(.{
        .source = source,

        .full_source = source,
        .path = path,
    }, allocator);
    parser.parse() catch |err| {
        try @import("errorMessage.zig").printErrorMessage(error_writer, parser.tokenizer.error_info);
        return err;
    };
    return &.{};
}

pub fn readFile(io: std.Io, allocator: Allocator, path: []const u8) ![]const u8 {
    const source_file = try std.Io.Dir.cwd().openFile(io, path, .{});
    defer source_file.close(io);

    var reader = source_file.reader(io, &.{});

    var alloc_writer = std.Io.Writer.Allocating.init(allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    return alloc_writer.writer.buffered();
}
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
