const std = @import("std");
const hgsl = @import("hgsl");
const net = std.net;

const Tokenizer = hgsl.Tokenizer;

pub fn main() !void {
    try @import("server.zig").mn();
    //     .{ 127, 0, 0, 1 },
    //     443,
    // ) };
    // var server: net.Server = .{
    //     .listen_address = address,
    //     .stream = try std.net.tcpConnectToAddress(),
    // };
    // try server.deinit();
}
