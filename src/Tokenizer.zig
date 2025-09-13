const std = @import("std");
const util = @import("util.zig");
const Tokenizer = @This();
const Parser = @import("Parser.zig");

const Error = error{InvalidInput};

source: []const u8,
last: Token = .eof,

start_ptr: [*]const u8,

pub fn next(self: *Tokenizer) Error!Token {
    const token: Token = switch (self.nextBytes()) {
        .endl => .endl,
        .eof => .eof,
        .valid => |v| .{ .identifier = v },
    };
    return token;
}

pub fn nextBytes(self: *Tokenizer) RawToken {
    const comment_symbol = "//"; /////////

    var is_endl = false;
    var in_comment = false;
    var count: usize = 0;

    while (self.source.len > 0) {
        const char = self.source[0];

        const is_valid: std.meta.Tuple(&.{ bool, u32 }) = blk: {
            if (util.strStartsComp(self.source, comment_symbol)) {
                in_comment = true;
                break :blk .{ false, comment_symbol.len };
            }
            if (char == '\n' or util.strStartsComp(self.source, "\r\n")) {
                is_endl = !in_comment;
                in_comment = false;
                break :blk .{ false, if (char == '\n') 1 else 2 };
            }
            break :blk .{ !isWhitespace(char) and !in_comment, 1 };
        };
        if (is_valid[0]) {
            if (is_endl) break;
            count += 1;
            self.shift(1);
        } else {
            if (count > 0) break;
            self.shift(is_valid[1]);
        }
    }
    return if (count == 0)
        if (is_endl) .endl else .eof
    else
        .{ .valid = blk: {
            var cpy = self.source;
            cpy.ptr -= count;
            cpy.len += count;
            break :blk cpy[0..count];
        } };
}
fn isWhitespace(char: u8) bool {
    return char == ' ' or char == '\t';
}

inline fn shift(self: *Tokenizer, amount: usize) void {
    self.source = self.source[amount..];
}
const RawToken = union(enum) { eof, endl, valid: []const u8 };

pub fn new(source: []const u8) Tokenizer {
    return .{
        .source = source,
        .start_ptr = source.ptr,
    };
}

pub const Token = union(enum) {
    eof,
    endl,

    identifier: []const u8,

    type_literal: Parser.Type,
    int_literal: i128,
    float_literal: f128,

    bin_op,
    un_op,

    eql,
    lambda,
    //punctuation
    dot,
    comma,
    colon,
    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",
    //keyword
    _if,
    _else,
    _for,
    _while,
    _fn,
    _struct,
    _enum,
    _image,
    _defer,
};
