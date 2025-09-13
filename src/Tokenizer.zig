const std = @import("std");
const Tokenizer = @This();
const Parser = @import("Parser.zig");

const Error = error{InvalidInput};

source: []const u8,
last: Token = .eof,

pub fn next(self: *Tokenizer) Error!Token {}

pub fn new(source: []const u8) Tokenizer {
    return .{
        .source = source,
    };
}

pub const TokenData = std.meta.Tuple(&.{ Token, usize });
pub const Token = union(enum) {
    eof,
    endl,

    identifier: []const u8,

    type_literal : Parser.Type,
        int_literal: i128,
    float_literal: f128,

    bin_op,
    un_op,

    
};
