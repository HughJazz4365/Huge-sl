const std = @import("std");
const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Generator = @This();

const Error = error{} || Writer.Error;
allocator: Allocator,
parser: *Parser,
output: *Writer,

types: List(Type) = undefined,
//order of things:
//1. capabilities => flag struct
//2. extensions => flag struct
//3. memory model
//4. decorations, entry point => list
//5. types => list
//6. body => buf

pub fn new(parser: *Parser, allocator: Allocator, output: *Writer) Generator {
    return .{
        .parser = parser,
        .allocator = allocator,
        .output = output,
    };
}
pub fn generate(self: *Generator) Error!void {
    // try self.output.print("WRITE: {d}\n", .{52});
    //algorithm:
    //go through global scope statements
    //if its a var decl of type entrypoint generate code for it

    //generate for entry point:
    //when encounter a new type add it to the used_types list
    //when encounter a new function generate an output for it
    try self.output.flush();
}

const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const List = std.ArrayList;
const Type = tp.Type;
