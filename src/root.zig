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
    WriteFailed,

    FileReadFailed, //handled?

    //==============

    SyntaxError,
    CompilationError,
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
    // std.debug.print("size of {s}: {d}, align: {d}\n", .{ @typeName(T), @sizeOf(T), @alignOf(T) });

    var threaded_io = std.Io.Threaded.init_single_threaded;
    const io = threaded_io.io();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();

    var buf: [128]u8 = undefined;
    var file_writer = std.Io.File.stdout().writer(io, &buf);

    const path = "test.hgsl";
    const source = readFile(io, allocator, path) catch
        return Error.FileReadFailed;

    const clock = std.Io.Clock.awake;
    var timestamp = clock.now(io);
    var measure: u64 = 0;
    const test_count = 1;
    for (0..test_count) |_| {
        timestamp = clock.now(io);
        var tok: Tokenizer = .{ .full_source = source, .path = path };
        tok.tokenize(allocator) catch |err| return if (err == Error.SyntaxError)
            error_message.errorOutTokenizer(tok, &file_writer.interface)
        else
            err;

        // for (0..tok.list.len) |i| {
        //     std.debug.print("TOKEN: {f}\n", .{Parser.FatToken{ .token = @truncate(i), .self = tok }});
        // }
        var parser = try Parser.new(allocator);
        parser.parse(tok) catch |err| return if (err == Error.CompilationError)
            error_message.errorOutParser(&parser, &file_writer.interface)
        else
            err;

        const new_timestamp = clock.now(io);
        measure += @intCast(timestamp.durationTo(new_timestamp).nanoseconds);
        timestamp = new_timestamp;
        parser.dump();
    }
    std.debug.print(
        "time: {d} mcs(tc: {d})\n",
        .{ @as(f64, @floatFromInt(measure)) / 1_000.0 / test_count, test_count },
    );
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
