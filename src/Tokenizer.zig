const std = @import("std");
const hgsl = @import("root.zig");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
// const tp = @import("type.zig");
const Tokenizer = @This();
const error_message = @import("errorMessage.zig");

const comment_symbol = "//";

var current: *Tokenizer = undefined;
list: List(TokenEntry) = .empty,

source: []const u8 = undefined,

path: []const u8,
full_source: []const u8,

error_info: ErrorInfo = .unknown,

pub fn deinit(self: *Tokenizer, allocator: std.mem.Allocator) void {
    self.list.deinit(allocator);
}
pub fn tokenize(self: *Tokenizer, allocator: std.mem.Allocator) Error!void {
    current = self;

    self.source = self.full_source;
    if (self.source.len == 0) return;

    self.list = try .initCapacity(allocator, 32);

    while (try self.getNextEntry()) |e| {
        try self.list.append(allocator, e);
        if (e.kind != .endl) self.bump(e.len());
    }
}
fn createEntry(self: Tokenizer, kind: TokenKind, bytes: []const u8) TokenEntry {
    return .{
        .kind = kind,
        .offset = @truncate(bytes.ptr - self.full_source.ptr),
        ._len = @truncate(bytes.len),
    };
}
fn getNextEntry(self: *Tokenizer) Error!?TokenEntry {
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
        self.createEntry(.endl, self.source[0..0])
    else
        null;

    const bytes = self.source[0..count];
    for ( //
        @intFromEnum(TokenKind.@"=>").. //
        @intFromEnum(TokenKind.false) + 1) |i|
    {
        const tn = @tagName(@as(TokenKind, @enumFromInt(i)));
        if (if (i >= @intFromEnum(TokenKind.@"const"))
            strSep(bytes, tn)
        else
            util.strStarts(bytes, tn))
            return self.createEntry(@enumFromInt(i), bytes[0..tn.len]);
    }

    switch (bytes[0]) {
        '0'...'9' => return self.createEntry(.int_literal, bytes[0..1]),
        else => {},
    }
    // if (try self.getNumberLiteralFat(bytes)) |l| return l;

    if (self.matchOperator(
        bytes,
        if (self.list.items.len == 0) false else switch (self.list.items[self.list.items.len - 1].kind) {
            .identifier, .int_literal, .float_literal, .true, .false, .@")", .@"]", .@"}" => true,
            //type literal
            else => false,
        },
    )) |t| return t;

    if (getNumericTypeLiteral(bytes)) |tl| {
        if (tl.scalar.width == ._8 and tl.scalar.layout == .float)
            return self.errorOut(.float8);
        return .{
            .offset = @truncate(bytes.ptr - self.full_source.ptr),
            ._len = @bitCast(tl),
            .kind = .type_literal,
        };
    }

    const valid_identifier = try self.stripValidIdentifier(bytes[@intFromBool(bytes[0] == '@')..]);

    return self.createEntry(if (bytes[0] == '@') .builtin else .identifier, valid_identifier);
}
fn getNumericTypeLiteral(bytes: []const u8) ?TypeLiteral {
    if (bytes.len < 2) return null;
    const scalar: Parser.Scalar = .{
        .layout = switch (bytes[0]) {
            'i' => .int,
            'u' => .uint,
            'f' => .float,
            else => return null,
        },
        .width = inline for (@typeInfo(Parser.Scalar.Width).@"enum".fields) |ef| {
            if (util.strStarts(bytes[1..], ef.name[1..]))
                break @enumFromInt(ef.value);
        } else return null,
    };

    const scalar_off: usize = if (scalar.width == ._8) 2 else 3;
    if (bytes.len <= scalar_off) return .{ .scalar = scalar };

    var lengthes: [2]u2 = .{ 0, 0 };
    var dim: usize = 0;
    for (0..2) |i| {
        const off = scalar_off + i * 2 + 1;
        if (bytes.len <= off) break;
        if (bytes[off - 1] == 'x') {
            if (bytes.len <= off) return null;
            lengthes[i] = switch (bytes[off]) {
                '2'...'4' => @intCast(bytes[off] - '1'),
                else => return null,
            };
            dim += 1;
        }
    }
    const len = scalar_off + dim * 2;
    if (bytes.len > len and isIdentifierChar(bytes[len])) return null;

    return .{
        .scalar = scalar,
        .vector_len = lengthes[0],
        .column_count = lengthes[1],
    };
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
fn matchOperator(self: *Tokenizer, bytes: []const u8, bin_priority: bool) ?TokenEntry {
    return for (&[_]@Tuple(&.{ TokenKind, []const u8 }){
        .{ .indexable_ptr, "[]" },
        .{ .dotsat, "|*|" },
        .{ .shl, "<<" },
        .{ .shr, ">>" },
        .{ .eql, "==" },
        .{ .not_eql, "!=" },
        .{ .goq, ">=" },
        .{ .loq, "<=" },
        .{ .dist, "<>" },
        .{ .cross, "><" },
        .{ .dot, "**" },
        .{ .xor, "^^" },

        .{ .greater, ">" },
        .{ .less, "<" },
        .{ .div, "/" },
        .{ .pow, "^" },
        .{ .mod, "%" },

        .{ .@"and", "&" },
        .{ .@"or", "|" },

        .{ .not, "!" },
        .{ .sat, ";" },
        .{ .invsqrt, "\\\\" },
        .{ .abs, "\\/" },
        .{ .sqrt, "\\" },
        .{ .sqrmag, "~~" },
        .{ .mag, "~" },
    }) |pair| {
        if (util.strStarts(bytes, pair[1])) break self.createEntry(pair[0], bytes[0..pair[1].len]);
    } else self.createEntry(switch (bytes[0]) {
        '+' => if (bin_priority) .add else .pos,
        '-' => if (bin_priority) .sub else .neg,
        '*' => if (bin_priority) .mul else .ptr,
        '|' => if (bin_priority) .@"or" else .sat,
        else => return null,
    }, bytes[0..1]);
}

fn strSep(haystack: []const u8, needle: []const u8) bool {
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
fn isWhitespace(char: u8) bool {
    return char == ' ' or char == '\t';
}
pub const Token = u32;
pub inline fn entry(self: Tokenizer, token: Token) TokenEntry {
    return if (token < self.list.items.len)
        self.list.items[token]
    else blk: {
        @branchHint(.unlikely);
        break :blk .eof;
    };
}
pub inline fn slice(self: Tokenizer, token: Token) []const u8 {
    return self.entry(token).slice(self);
}

pub const TypeLiteral = packed struct(u24) {
    scalar: Parser.Scalar,

    vector_len: u2 = 0, //1-4
    column_count: u2 = 0, //1-4

    rest: u16 = 0,
    pub fn len(self: TypeLiteral) u32 {
        return 2 + @as(u32, @intFromBool(self.scalar.width != ._8)) +
            2 * @as(u32, @intFromBool(self.vector_len != 0)) +
            2 * @as(u32, @intFromBool(self.column_count != 0));
    }
    pub fn entry(self: TypeLiteral) Parser.TypeEntry {
        return if (self.column_count != 0) //
            .{ .matrix = .{
                .scalar = self.scalar,
                .m = @enumFromInt(self.vector_len - 1),
                .n = @enumFromInt(self.column_count - 1),
            } }
        else if (self.vector_len != 0) //
            .{ .vector = .{
                .scalar = self.scalar,
                .len = @enumFromInt(self.vector_len - 1),
            } }
        else
            .{ .scalar = self.scalar };
    }
};
pub const TokenEntry = packed struct(u64) {
    offset: u32,
    _len: u24,
    kind: TokenKind,
    pub inline fn slice(self: TokenEntry, tokenizer: Tokenizer) []const u8 {
        return tokenizer.full_source[self.offset .. self.offset + self.len()];
    }
    pub fn len(self: TokenEntry) u32 {
        return if (self.kind == .type_literal)
            @as(TypeLiteral, @bitCast(self._len)).len()
        else
            self._len;
    }
    pub fn format(self: TokenEntry, writer: *std.Io.Writer) !void {
        try writer.print("'{s}'", .{@tagName(self.kind)});
        switch (self.kind) {
            .identifier,
            .int_literal,
            => try writer.print(" : {s}", .{self.slice(current.*)}),
            else => {},
        }
    }
    pub const eof: TokenEntry = .{ .kind = .eof, .offset = 0, ._len = 0 };
};

pub inline fn binOpFromTokenKind(kind: TokenKind) ?BinaryOperator {
    const f = @intFromEnum(__bin_op_from);
    const t = @intFromEnum(__bin_op_to);
    return if (@intFromEnum(kind) >= f and @intFromEnum(kind) <= t) //
        @enumFromInt(@intFromEnum(kind) - f)
    else
        null;
}
pub const BinaryOperator = util.EnumSlice(TokenKind, u8, __bin_op_from, __bin_op_to);
const __bin_op_from: TokenKind = .add;
const __bin_op_to: TokenKind = .dotsat;

pub inline fn uOpFromTokenKind(kind: TokenKind) ?UnaryOperator {
    const f = @intFromEnum(__u_op_from);
    const t = @intFromEnum(__u_op_to);
    return if (@intFromEnum(kind) >= f and @intFromEnum(kind) <= t) //
        @enumFromInt(@intFromEnum(kind) - f)
    else
        null;
}
pub const UnaryOperator = util.EnumSlice(TokenKind, u8, __u_op_from, __u_op_to);
const __u_op_from: TokenKind = .neg;
const __u_op_to: TokenKind = .indexable_ptr;

pub const TokenKind = enum(u8) {
    eof,
    endl,

    //str literals
    string,
    identifier,
    builtin,
    int_literal,
    float_literal,
    type_literal,

    @"=>",
    @"=",
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
    @"var",
    in,
    out,
    push,
    shared,

    //shader stage qualifiers
    vertex,
    fragment,
    compute,

    @"if",
    @"else",
    @"switch",
    loop,

    @"return",
    @"break",
    @"continue",
    discard,

    @"fn",
    @"struct",
    @"enum",

    @"anytype",
    @"defer",

    true,
    false,

    //bin_op
    add,
    sub,
    mul,
    div,
    pow,
    mod,

    @"or",
    @"and",
    xor,
    shl,
    shr,

    eql,
    not_eql,
    greater,
    less,

    goq,
    loq,

    dist,
    cross,
    dot,
    dotsat,

    //u_op
    neg,
    pos,

    not,

    sat,
    abs,
    sqrt,
    invsqrt,
    norm,
    mag,
    sqrmag,

    ptr,
    indexable_ptr,
};

pub fn bindingPower(op: BinaryOperator) u8 {
    return switch (op) {
        .pow => 13,
        .cross => 12,
        .dist => 11,

        .dot, .dotsat => 10,

        .mul, .div, .mod => 8,

        .add, .sub => 7,
        .shl, .shr => 6,

        .greater, .less, .goq, .loq => 5,
        .eql, .not_eql => 4,

        .@"and" => 3,
        .xor => 2,
        .@"or" => 1,

        // else => 1,
    };
}

pub fn errorOut(self: *Tokenizer, error_info: ErrorInfo) Error {
    self.error_info = error_info;
    return error_message.errorOut(error_info);
}
const List = std.ArrayList;
const ErrorInfo = error_message.ErrorInfo;
const Error = hgsl.Error;
