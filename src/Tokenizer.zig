const std = @import("std");
const util = @import("util.zig");
const Tokenizer = @This();
const Parser = @import("Parser.zig");

const Error = error{InvalidInput};

source: []const u8,
last: Token = .eof,

start_ptr: [*]const u8,

pub fn next(self: *Tokenizer) Error!Token {
    const p = try self.peekRaw();
    self.last = p.token;
    self.shift(p.len);
    return p.token;
}
fn peekRaw(self: *Tokenizer) Error!PeekRawResult {
    const bytes = switch (self.nextBytes()) {
        .endl => return .{ .len = 0, .token = .endl },
        .eof => return .{ .len = 0, .token = .eof },
        .valid => |v| v,
    };
    inline for (keywords) |tag| {
        const offset = @intFromBool(@intFromEnum(tag) >= @intFromEnum(TokenTag._const));
        if (util.strExtract(bytes, @tagName(tag)[offset..])) return .{
            .len = @tagName(tag).len - offset,
            .token = @unionInit(Token, @tagName(tag), {}),
        };
    }

    const bin_op_match = util.matchToEnum(BinaryOperator, bytes);
    const u_op_match = util.matchToEnum(UnaryOperator, bytes);
    switch (self.last) {
        .@"=", .@"=>", .bin_op, .u_op => {
            if (u_op_match) |x| return .{ .len = @tagName(x).len, .token = .{ .u_op = x } };
            if (bin_op_match) |x| return .{ .len = @tagName(x).len, .token = .{ .bin_op = x } };
        },
        else => {
            if (bin_op_match) |x| return .{ .len = @tagName(x).len, .token = .{ .bin_op = x } };
            if (u_op_match) |x| return .{ .len = @tagName(x).len, .token = .{ .u_op = x } };
        },
    }
    // parse operators
    // parse type literal
    // parse number
    // parse swizzle
    // strip valid identifier

    return .{
        .len = bytes.len,
        .token = .{ .identifier = bytes },
    };
}
const PeekRawResult = struct { len: usize, token: Token };

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
            self.source.ptr -= count;
            self.source.len += count;
            break :blk self.source[0..count];
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

    bin_op: BinaryOperator,
    u_op: UnaryOperator,

    swizzle: []const u8,
    @"=>",
    @"=",
    //punctuation
    @".",
    @",",
    @":",

    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",
    //keywords
    _const,
    _var,
    _in,
    _out,
    _uniform,
    _property,
    _shared,

    _if,
    _else,
    _switch,
    _return,
    _break,
    _for,
    _while,

    _fn,
    _entrypoint,

    _struct,
    _enum,
    _image,
    _defer,

    pub fn print(self: Token) void {
        switch (self) {
            .identifier => |id| std.debug.print("[id]: {s}\n", .{id}),
            .type_literal => std.debug.print("[type_literal]: \n", .{}),
            inline else => |value, tag| std.debug.print("[{s}]: {any}\n", .{ @tagName(tag), value }),
        }
    }
};
const TokenTag = @typeInfo(Token).@"union".tag_type.?;
const keywords: []const TokenTag = blk: {
    const from = "=>";
    var encountered = false;
    var slice: []const TokenTag = &.{};
    for (@typeInfo(Token).@"union".fields, 0..) |uf, i| {
        if (util.strEql(uf.name, from)) encountered = true;
        if (!encountered) continue;
        slice = slice ++ &[1]TokenTag{@enumFromInt(i)};
    }
    break :blk slice;
};
const BinaryOperator = util.SortEnumDecending(
    enum {
        @"*",
        @"+",
        @"-",
        @"^",
    },
);
const UnaryOperator = util.SortEnumDecending(
    enum {
        @"-",
    },
);
