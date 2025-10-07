const std = @import("std");
const util = @import("util.zig");
const tp = @import("type.zig");
const Tokenizer = @This();
const Parser = @import("Parser.zig");
const ErrorCtx = @import("errorctx.zig");

pub const Error = error{InvalidInput};
const State = struct {
    source: []const u8,
    last: Token = .eof,
    last_ptr: [*]const u8 = undefined,
};
state: State,

full_source: []const u8,
err_ctx: *ErrorCtx,

pub fn skipErr(self: *Tokenizer) Error!void {
    _ = try self.next();
}

///assumes token was already peeked
pub fn skip(self: *Tokenizer) void {
    _ = self.next() catch unreachable;
}

pub fn peekTimes(self: *Tokenizer, num: usize) Error!Token {
    const save_state = self.state;
    defer self.state = save_state;

    for (0..num -| 1) |_| try self.skipErr();
    const token = try self.peek();

    return token;
}
pub fn peek(self: *Tokenizer) Error!Token {
    const save_state = self.state;
    defer self.state = save_state;

    return self.next();
}
pub fn next(self: *Tokenizer) Error!Token {
    const fat = try self.nextFat();
    self.shift(fat.len);
    self.state.last = fat.token;
    return fat.token;
}

fn nextFat(self: *Tokenizer) Error!FatToken {
    const bytes = switch (self.nextBytes()) {
        .endl => return .{ .len = 0, .token = .endl },
        .eof => return .{ .len = 0, .token = .eof },
        .valid => |v| v,
    };
    inline for (keywords) |tag| {
        if (if (@intFromEnum(tag) >= @intFromEnum(TokenTag.@"const"))
            strExtract(bytes, @tagName(tag))
        else
            util.strStarts(bytes, @tagName(tag))) return .{
            .len = @tagName(tag).len,
            .token = @unionInit(Token, @tagName(tag), {}),
        };
    }

    //before op s ??
    if (getNumberLiteralRaw(bytes)) |l| return l;

    const bin_op_match = util.matchToEnum(BinaryOperator, bytes);
    const u_op_match = util.matchToEnum(UnaryOperator, bytes);
    switch (self.state.last) {
        .identifier,
        .type_literal,
        .compfloat,
        .compint,
        .true,
        .false,
        .@")",
        .@"]",
        .@"}",
        => {
            if (bin_op_match) |x| return .{ .len = @tagName(x).len, .token = .{ .bin_op = x } };
            if (u_op_match) |x| return .{ .len = @tagName(x).len, .token = .{ .u_op = x } };
        },
        else => {
            if (u_op_match) |x| return .{ .len = @tagName(x).len, .token = .{ .u_op = x } };
            if (bin_op_match) |x| return .{ .len = @tagName(x).len, .token = .{ .bin_op = x } };
        },
    }

    if (getTypeLiteralRaw(bytes)) |t| return t;

    // strip valid identifier
    const valid_identifier = try self.stripValidIdentifier(bytes);

    return .{
        .len = valid_identifier.len,
        .token = .{ .identifier = valid_identifier },
    };
}

const FatToken = struct { len: usize, token: Token };
fn getNumberLiteralRaw(bytes: []const u8) ?FatToken {
    const neg = bytes[0] == '-';
    var count: usize = @intFromBool(neg);
    var dot = false;

    while (bytes.len > count) {
        const char = bytes[count];
        if (char == '.') {
            if (dot) break;
            dot = true;
            count += 1;
        } else if (switch (char) {
            '0'...'9' => true,
            else => false,
        }) count += 1 else break;
    }
    if (count - @as(usize, @intFromBool(neg)) - @as(usize, @intFromBool(dot)) == 0)
        return null;
    return if (dot)
        .{ .len = count, .token = .{
            .compfloat = std.fmt.parseFloat(f128, bytes[0..count]) catch unreachable,
        } }
    else
        .{ .len = count, .token = .{
            .compint = std.fmt.parseInt(i128, bytes[0..count], 10) catch unreachable,
        } };
}

fn getTypeLiteralRaw(bytes: []const u8) ?FatToken {
    inline for (.{ "void", "bool", "type", "compint", "compfloat" }) |s| {
        if (strExtract(bytes, s))
            return .{ .len = s.len, .token = .{ .type_literal = @unionInit(tp.Type, s, {}) } };
    }
    inline for (comptime tp.Vector.allVectors()) |v| {
        const vec_literal = comptime v.literalComp();
        if (strExtract(bytes, vec_literal))
            return .{ .len = vec_literal.len, .token = .{ .type_literal = .{ .vector = v } } };
    }
    inline for (comptime tp.Number.allNumbers()) |v| {
        const num_literal = comptime v.literalComp();
        if (strExtract(bytes, num_literal))
            return .{ .len = num_literal.len, .token = .{ .type_literal = .{ .number = v } } };
    }
    return null;
}

pub fn nextBytes(self: *Tokenizer) RawToken {
    const comment_symbol = "//"; /////////

    var is_endl = false;
    var in_comment = false;
    var count: usize = 0;

    while (self.state.source.len > count) {
        const char = self.state.source[count];

        const is_valid: std.meta.Tuple(&.{ bool, usize }) = blk: {
            if (util.strStartsComp(self.state.source[count..], comment_symbol)) {
                in_comment = true;
                break :blk .{ false, comment_symbol.len };
            }
            if (util.startingEndlLength(self.state.source)) |l| {
                is_endl = true;
                in_comment = false;
                break :blk .{ false, l };
            }
            break :blk .{ !isWhitespace(char) and !in_comment, 1 };
        };
        if (is_valid[0]) {
            if (is_endl) break;
            count += 1;
        } else {
            if (count > 0) break;
            self.shift(is_valid[1]);
        }
    }
    if (count == 0) return if (is_endl) .endl else .eof;
    self.state.last_ptr = self.state.source.ptr;
    return .{ .valid = self.state.source[0..count] };
}
fn isWhitespace(char: u8) bool {
    return char == ' ' or char == '\t';
}
fn strExtract(haystack: []const u8, needle: []const u8) bool {
    if (!util.strStarts(haystack, needle)) return false;
    if (haystack.len == needle.len) return true;
    return !isIdentifierChar(haystack[needle.len]);
}
fn stripValidIdentifier(self: *Tokenizer, bytes: []const u8) Error![]const u8 {
    var letter = false;
    var global = false;
    const end = for (bytes, 0..) |char, i| {
        if (!switch (char) {
            'a'...'z', 'A'...'Z' => blk: {
                letter = true;
                break :blk true;
            },
            '_' => true,
            '@' => blk: {
                global = true;
                break :blk i == 0;
            },
            '0'...'9' => i != @as(usize, @intFromBool(global)),
            else => false,
        }) break i;
    } else bytes.len;
    if (!letter) {
        // _ = self;
        const offset = @intFromPtr(bytes.ptr) - @intFromPtr(self.full_source.ptr);
        self.err_ctx.printError(offset, Error.InvalidInput, "", .{});
        // if (end == 0) {
        //     self.err_ctx.printError(offset, Error.InvalidInput, "Invalid character: \'{c}\'", .{bytes[0]});
        // } else self.err_ctx.printError(offset, Error.InvalidInput, "Invalid identifier: \'{s}\'", .{bytes[0..1]});
        return Error.InvalidInput;
    }
    //  return self.errorOut();

    return bytes[0..end];
}
fn isIdentifierChar(char: u8) bool {
    return switch (char) {
        'a'...'z',
        'A'...'Z',
        '0'...'9',
        '_',
        '@',
        => true,
        else => false,
    };
}

fn shift(self: *Tokenizer, amount: usize) void {
    self.state.source = self.state.source[amount..];
}
const RawToken = union(enum) { eof, endl, valid: []const u8 };

pub fn new(source: []const u8, err_ctx: *ErrorCtx) Tokenizer {
    return .{
        .state = .{
            .source = source,
            .last = .eof,
        },
        .err_ctx = err_ctx,
        .full_source = source,
    };
}

pub const Token = union(enum) {
    eof,
    endl,

    identifier: []const u8,

    type_literal: tp.Type,
    compint: i128,
    compfloat: f128,

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
    @"const",
    @"var",
    in,
    out,
    uniform,
    push,
    shared,

    @"if",
    @"else",
    @"switch",
    @"return",
    @"break",
    @"for",
    @"while",

    @"fn",
    entrypoint,

    @"struct",
    @"enum",
    image,
    @"defer",

    true,
    false,

    pub const format = @import("debug.zig").formatToken;
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
pub const BinaryOperator = util.SortEnumDecending(
    enum {
        @"*",
        @"+",
        @"-",
        @"^",
    },
);
pub fn bindingPower(bin_op: BinaryOperator) u8 {
    return switch (bin_op) {
        .@"^" => 15,
        .@"*" => 10,
        .@"+", .@"-" => 5,
    };
}
pub const UnaryOperator = util.SortEnumDecending(
    enum {
        @"-",
        @"+",
        @"|", //normalize

    },
);
