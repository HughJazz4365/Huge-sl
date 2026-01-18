const std = @import("std");
const hgsl = @import("root.zig");
const util = @import("util.zig");
const tp = @import("type.zig");
const Tokenizer = @This();
const error_message = @import("errorMessage.zig");

const comment_symbol = "//";

error_info: ErrorInfo = .unknown,
source: []const u8,
last: FatToken = .{},

//needed for error messages
full_source: []const u8,
path: []const u8,

pub fn skipEndl(self: *Tokenizer) Error!void {
    self.last = try self.peekRaw();
}
pub fn skipErr(self: *Tokenizer) Error!void {
    self.bump((try self.peekRaw()).slice.len);
}
pub fn skip(self: *Tokenizer) void {
    self.bump((self.peekRaw() catch unreachable).slice.len);
}
pub fn peekPastEndl(self: *Tokenizer) Error!Token {
    const first = try self.peek();
    if (first != .endl) return first;

    const t = self.*;
    defer self.* = t;

    self.skip();
    return try self.peek();
}
pub fn peekSlice(self: *Tokenizer, slice: []Token) Error!void {
    const t = self.*;
    defer self.* = t;
    for (slice) |*token| token.* = try self.next();
}
pub fn peek(self: *Tokenizer) Error!Token {
    return (try self.peekFat()).token;
}
pub fn peekFat(self: *Tokenizer) Error!FatToken {
    const s = self.source;
    defer self.source = s;
    return self.peekRaw();
}

pub fn next(self: *Tokenizer) Error!Token {
    self.last = try self.peekRaw();
    self.bump(self.last.slice.len);
    return self.last.token;
}

fn peekRaw(self: *Tokenizer) Error!FatToken {
    var is_endl = false;
    var in_comment = false;
    var count: usize = 0;

    while (self.source.len > count) {
        const char = self.source[count];

        const is_valid: std.meta.Tuple(&.{ bool, usize }) = blk: {
            if (util.strStartsComp(self.source[count..], comment_symbol)) {
                in_comment = true;
                break :blk .{ false, comment_symbol.len };
            }
            if (util.startingEndlLength(self.source)) |l| {
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
            self.bump(is_valid[1]);
        }
    }
    if (count == 0) return if (is_endl)
        .{ .token = .endl, .slice = self.source[0..0] }
    else
        .{ .token = .eof, .slice = self.source[0..0] };

    const bytes = self.source[0..count];
    inline for (keywords) |tag| {
        if (if (@intFromEnum(tag) >= @intFromEnum(TokenTag.@"const"))
            strExtract(bytes, @tagName(tag))
        else
            util.strStarts(bytes, @tagName(tag))) return .{
            .token = @unionInit(Token, @tagName(tag), {}),
            .slice = bytes[0..@tagName(tag).len],
        };
    }

    if (try self.getNumberLiteralFat(bytes)) |l| return l;

    const bin_op_match = util.matchToEnum(BinaryOperator, bytes);
    const u_op_match = util.matchToEnum(UnaryOperator, bytes);
    switch (self.last.token) {
        .identifier,
        .type_literal,
        .int_literal,
        .float_literal,
        .true,
        .false,
        .@")",
        .@"]",
        .@"}",
        => {
            if (bin_op_match) |x| return .{ .token = .{ .bin_op = x }, .slice = bytes[0..@tagName(x).len] };
            if (u_op_match) |x| return .{ .token = .{ .u_op = x }, .slice = bytes[0..@tagName(x).len] };
        },
        else => {
            if (u_op_match) |x| return .{ .token = .{ .u_op = x }, .slice = bytes[0..@tagName(x).len] };
            if (bin_op_match) |x| return .{ .token = .{ .bin_op = x }, .slice = bytes[0..@tagName(x).len] };
        },
    }

    if (try self.getTypeLiteralFat(bytes)) |t| return t;

    const is_builtin = bytes[0] == '@';
    const valid_identifier = try self.stripValidIdentifier(bytes[@intFromBool(is_builtin)..]);

    return if (is_builtin)
        .{ .token = .{ .builtin = valid_identifier }, .slice = bytes[0 .. valid_identifier.len + 1] }
    else
        .{ .token = .{ .identifier = valid_identifier }, .slice = bytes[0..valid_identifier.len] };
}

fn stripValidIdentifier(self: *Tokenizer, bytes: []const u8) Error![]const u8 {
    var letter = false;
    const end = for (bytes, 0..) |char, i| {
        if (!switch (char) {
            'a'...'z', 'A'...'Z', '_' => blk: {
                letter = true;
                break :blk true;
            },
            '0'...'9' => i != 0,
            else => false,
        }) break i;
    } else bytes.len;
    if (!letter) {
        //TODO: invalid character error
        return self.errorOut(.unknown);
    }

    return bytes[0..end];
}
fn getNumberLiteralFat(self: *Tokenizer, bytes: []const u8) Error!?FatToken {
    //redo that
    const neg = bytes[0] == '-';

    var count: usize = @intFromBool(neg);

    const base: u8 =
        if (util.strStartsComp(bytes[count..], "0x")) 16 //
        else if (util.strStartsComp(bytes[count..], "0b")) 2 //
        else if (util.strStartsComp(bytes[count..], "0o")) 8 //
        else 10;
    if (base != 10) count += 2;

    const off = count;
    var dot = false;

    while (bytes.len > count) {
        const char = bytes[count];
        if (char == '.') {
            if (dot or
                (count - off == 0) or
                base != 10) break;
            dot = true;
            count += 1;
        } else if (switch (char) {
            '_' => count - off != 0,
            '0', '1' => true,
            '2'...'7' => base >= 8,
            '8', '9' => base >= 10,
            'a'...'f', 'A'...'F' => base == 16,
            else => false,
        }) count += 1 else break;
    }
    while (count - off > 0 and (bytes[count - 1] == '_' or bytes[count - 1] == '.')) count -= 1;

    if (count - off - @as(usize, @intFromBool(dot)) == 0)
        return null;
    return .{
        .slice = bytes[0..count],
        .token = if (dot) .{
            .float_literal = blk: {
                const f = std.fmt.parseFloat(hgsl.CF, bytes[off..count]) catch
                    return self.errorOut(.unknown);
                // return self.err_msg.errorInvalidNumericLiteral(self.err_msg.offFromPtr(bytes.ptr), true);
                break :blk if (neg) -f else f;
            },
        } else .{
            .int_literal = blk: {
                const i = std.fmt.parseInt(hgsl.CI, bytes[off..count], base) catch
                    return self.errorOut(.unknown);
                // return self.err_msg.errorInvalidNumericLiteral(self.err_msg.offFromPtr(bytes.ptr), false);
                break :blk if (neg) -i else i;
            },
        },
    };
}
fn getTypeLiteralFat(self: *Tokenizer, bytes: []const u8) Error!?FatToken {
    inline for (.{ "void", "bool", "type", "compint", "compfloat" }) |s| {
        if (strExtract(bytes, s))
            return .{
                .token = .{ .type_literal = @unionInit(tp.Type, s, {}) },
                .slice = bytes[0..s.len],
            };
    }
    const scalar: tp.Scalar = .{
        .layout = switch (bytes[0]) {
            'i' => .int,
            'u' => .uint,
            'f' => .float,
            else => return null,
        },
        .width = inline for (@typeInfo(tp.Scalar.Width).@"enum".fields) |ef| {
            if (util.strStarts(bytes[1..], ef.name[1..]))
                break @enumFromInt(ef.value);
        } else return null,
    };
    if (scalar.layout == .float and scalar.width == ._8)
        return self.errorOut(.float8);

    const scalar_off: usize = if (scalar.width == ._8) 2 else 3;
    if (bytes.len <= scalar_off) return .{
        .token = .{ .type_literal = .{ .scalar = scalar } },
        .slice = bytes,
    };

    var lengthes: [2]tp.Vector.Len = undefined;
    var dim: usize = 0;
    for (0..2) |i| {
        const off = scalar_off + i * 2 + 1;
        if (bytes.len <= off) break;
        if (bytes[off - 1] == 'x') {
            if (bytes.len <= off) return null;
            lengthes[i] = switch (bytes[off]) {
                '2'...'4' => @enumFromInt(bytes[off] - '0'),
                else => return null,
            };
            dim += 1;
        }
    }
    const len = scalar_off + dim * 2;
    if (bytes.len > len and isIdentifierChar(bytes[len])) return null;

    return .{
        .token = .{ .type_literal = switch (dim) {
            0 => .{ .scalar = scalar },
            1 => .{ .vector = .{ .len = lengthes[0], .component = scalar } },
            2 => .{ .matrix = .{ .m = lengthes[0], .n = lengthes[1], .component = scalar } },
            else => unreachable,
        } },
        .slice = bytes[0..len],
    };
}

fn strExtract(haystack: []const u8, needle: []const u8) bool {
    if (!util.strStarts(haystack, needle)) return false;
    if (haystack.len == needle.len) return true;
    return !isIdentifierChar(haystack[needle.len]);
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

fn bump(self: *Tokenizer, amount: usize) void {
    self.source = self.source[amount..];
}
const RawToken = union(enum) { eof, endl, valid: []const u8 };
fn isWhitespace(char: u8) bool {
    return char == ' ' or char == '\t';
}

pub const Token = union(enum) {
    eof,
    endl,

    identifier: []const u8,
    builtin: []const u8,

    type_literal: tp.Type,

    int_literal: hgsl.CI,
    float_literal: hgsl.CF,

    bin_op: BinaryOperator,
    u_op: UnaryOperator,

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

    @"const",
    mut,
    in,
    out,
    push,
    shared,

    @"if",
    @"else",
    @"switch",
    @"for",

    @"return",
    @"break",
    @"continue",
    discard,

    @"defer",

    @"fn",
    entrypoint,
    @"anytype",

    @"struct",
    @"enum",

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
    enum(u8) {
        //regular
        @"+",
        @"-",
        @"*",
        @"/",

        @"^", //pow
        @"%", //mod

        //bitwise
        @"|", // bitwise/logical or
        @"&", // bitwise/logical and
        @"^^", // bitwise/logical xor
        @"<<",
        @">>",

        //relational
        @"==",
        @"!=",
        @">",
        @"<",
        @"<=",
        @">=",

        //custom
        @"<>", //distance
        @"><", //cross
        @"**", //dot
        @"***", //dot clamped
    },
);
pub fn bindingPower(bin_op: BinaryOperator) u8 {
    return switch (bin_op) {
        .@"><" => 55,
        .@"***", .@"**" => 50,
        .@"<>" => 40,
        .@"^" => 15,
        .@"*" => 10,
        .@"<<", .@">>" => 9,
        .@"&" => 8,
        .@"|", .@"^^" => 7,
        .@"+", .@"-" => 5,
        else => 1,
    };
}
pub const UnaryOperator = util.SortEnumDecending(
    enum {
        //regular
        @"-",
        @"+",

        //bitwise
        @"!", //bitwise/logical not

        //custom
        @";", //saturate
        @"\\/", //abs
        @"\\", //sqrt
        @"\\\\", //inverse sqrt

        @"|", //normalize
        @"~", //magnitude|
        @"~~", //sqr magnitude

        @"*", //pointer type
    },
);

pub fn errorOut(self: *Tokenizer, error_info: ErrorInfo) Error {
    self.error_info = error_info;
    return error_message.errorOut(error_info);
}
pub const FatToken = struct { token: Token = .eof, slice: []const u8 = "" };
const ErrorInfo = error_message.ErrorInfo;
const Error = hgsl.Error;
