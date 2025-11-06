const std = @import("std");

var buf: [256]u8 = @splat(0);
var writer: *std.Io.Writer = undefined;

var allocator: Allocator = undefined;

pub fn mn() !void {
    var file_writer = std.fs.File.stdout().writer(&buf);
    writer = &file_writer.interface;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    allocator = arena.allocator();

    try encodeMessage(.{ .raset = "reistn" });
    try writer.print("\nLEN: {any}\n", .{try decodeMessage(
        struct { poop: []const u8 },
        "Content-Length: 15\r\n\r\n{\"poop\":\"poop\"}",
    )});

    try file_writer.interface.flush();
}
fn decodeMessage(T: type, msg: []const u8) !T {
    const sep = "\r\n\r\n";

    const index = std.mem.indexOf(u8, msg, sep) orelse
        return error.NoRnRn;

    const content_length = "Content-Length: ";
    if (content_length.len > index) return error.InvalidHeader;
    const length = try std.fmt.parseInt(u32, msg[content_length.len..index], 10);
    const content = msg[index + sep.len ..];
    if (length != content.len) {
        std.debug.print(
            "Content length dont match the header:\nexpected: {d}, got: {d}\n",
            .{ length, content.len },
        );
        return error.ContentLengthDontMatch;
    }

    const parsed = try std.json.parseFromSlice(T, allocator, content, .{});
    defer parsed.deinit();

    return parsed.value;
}

fn encodeMessage(msg: anytype) !void {
    const formatter = std.json.fmt(msg, .{});

    var aw = std.Io.Writer.Allocating.init(allocator);
    defer aw.deinit();
    try formatter.format(&aw.writer);
    const bytes = aw.written();

    try writer.print("Content-Length: {d}\r\n\r\n{s}", .{ bytes.len, bytes });
}

const Allocator = std.mem.Allocator;
