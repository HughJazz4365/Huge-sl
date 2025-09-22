const std = @import("std");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Generator = @This();

const Error = error{} || Writer.Error;
allocator: Allocator,
parser: *Parser,
output: *Writer,
pub fn new(parser: *Parser, allocator: Allocator, output: *Writer) Generator {
    return .{
        .parser = parser,
        .allocator = allocator,
        .output = output,
    };
}
pub fn generate(self: *Generator) Error!void {
    // try self.output.print("WRITE: {d}\n", .{52});
    try self.output.flush();
}

const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
