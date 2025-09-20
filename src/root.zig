const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");

pub fn compile(allocator: std.mem.Allocator, source: []const u8, out: ?*std.Io.Writer) !void {
    // const len = 19;
    // const @"type": tp.Type = .{ .array = .{
    //     .len = len,
    //     .child = @constCast(&tp.Type{ .number = .{ .width = .word, .type = .float } }),
    // } };
    // std.debug.print("size: {d}, {d}\n", .{ @"type".size(), @sizeOf([len]f32) });
    _ = out;
    var timer = try std.time.Timer.start();

    var tokenizer: Tokenizer = .new(source);
    var parser: Parser = try .parse(allocator, &tokenizer);
    defer parser.deinit();

    const measure = timer.read();
    _ = measure;
    // std.debug.print("time {d} ms.\n", .{@as(f64, @floatFromInt(measure)) / 1_000_000});
}

pub fn compileFile(allocator: std.mem.Allocator, path: []const u8, out: ?*std.Io.Writer) !void {
    const source_file = try std.fs.cwd().openFile(path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    // const source = "const kek = 1 + -52.8 * 9\n";
    const source = alloc_writer.writer.buffered();
    try compile(allocator, source, out);

    // var token = try tokenizer.next();
    // std.debug.print("{f}\n", .{token});

    // while (token != .eof) {
    //     token = try tokenizer.next();
    //     std.debug.print("{f}\n", .{token});
    // }

    // if (true) return;
}
