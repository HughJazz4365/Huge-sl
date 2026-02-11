const std = @import("std");
const hgsl = @import("root.zig");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @This();
const error_message = @import("errorMessage.zig");

const use_multi_array_list = false;
const comment_symbol = "//";

list: List(TokenEntry) = .empty,

source: []const u8 = undefined,

path: []const u8,
full_source: []const u8,

error_info: ErrorInfo = undefined,

pub fn deinit(self: *Tokenizer, allocator: std.mem.Allocator) void {
    self.list.deinit(allocator);
}
pub fn tokenize(self: *Tokenizer, allocator: std.mem.Allocator) Error!void {
    self.source = self.full_source;
    if (self.source.len == 0) return;

    self.list = try .initCapacity(allocator, 32);

    while (try self.getNextFatTokenEntry()) |e| {
        try self.list.append(allocator, .{ .kind = e.kind, .offset = e.offset });
        if (e.kind != .endl) self.bump(e.len);
    }
}
pub fn parseTypeLiteral(self: Tokenizer, token: Token) Parser.TypeEntry {
    const bytes = self.slice(token);
    const scalar: Parser.Scalar = .{
        .layout = switch (bytes[0]) {
            'f' => .float,
            'u' => .uint,
            'i' => .int,
            else => unreachable,
        },
        .width = switch (bytes[1]) {
            '8' => ._8,
            '1' => ._16,
            '3' => ._32,
            '6' => ._64,
            else => unreachable,
        },
    };
    const off: usize = 2 + @as(usize, @intFromBool(scalar.width != ._8));
    if (bytes.len == off) return .{ .scalar = scalar };

    const vector_len: Parser.Vector.Len = switch (bytes[off + 1]) {
        '2' => ._2,
        '3' => ._3,
        '4' => ._4,
        else => unreachable,
    };
    if (bytes.len == off + 2) return .{ .vector = .{ .len = vector_len, .scalar = scalar } };
    const column_count: Parser.Vector.Len = switch (bytes[off + 3]) {
        '2' => ._2,
        '3' => ._3,
        '4' => ._4,
        else => unreachable,
    };
    return .{ .matrix = .{ .m = vector_len, .n = column_count, .scalar = scalar } };
}

fn createFatEntry(self: Tokenizer, token_kind: TokenKind, bytes: []const u8) FatTokenEntry {
    return .{
        .kind = token_kind,
        .offset = @truncate(bytes.ptr - self.full_source.ptr),
        .len = @truncate(bytes.len),
    };
}
fn getNextFatTokenEntry(self: *Tokenizer) Error!?FatTokenEntry {
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
        self.createFatEntry(.endl, self.source[0..0])
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
            return self.createFatEntry(@enumFromInt(i), bytes[0..tn.len]);
    }

    switch (bytes[0]) {
        '0'...'9' => return self.createFatEntry(.int_literal, bytes[0..1]),
        else => {},
    }
    // if (try self.getNumberLiteralFat(bytes)) |l| return l;

    if (self.matchOperator(
        bytes,
        if (self.tokenCount() == 0) false else switch (self.kind(self.tokenCount() - 1)) {
            .identifier, .int_literal, .float_literal, .true, .false, .@")", .@"]", .@"}" => true,
            //type literal
            else => false,
        },
    )) |t| return t;

    const tl_len = getNumericTypeLiteral(bytes);
    if (tl_len > 0) {
        if (bytes[0] == 'f' and bytes[1] == '8')
            return self.errorOut(.{
                .source_offset = bytes.ptr - self.full_source.ptr,
                .kind = .float8,
            });
        return .{
            .offset = @truncate(bytes.ptr - self.full_source.ptr),
            .len = @truncate(tl_len),
            .kind = .type_literal,
        };
    }

    const bi: usize = @intFromBool(bytes[0] == '@');
    const valid_identifier = try self.stripValidIdentifier(bytes[bi..]);
    return .{
        .kind = if (bytes[0] == '@') .builtin else .identifier,
        .offset = @truncate(bytes.ptr - self.full_source.ptr),
        .len = @truncate(valid_identifier.len + bi),
    };
}
fn getNumericTypeLiteral(bytes: []const u8) usize {
    if (bytes.len < 2) return 0;
    const scalar: Parser.Scalar = .{
        .layout = switch (bytes[0]) {
            'i' => .int,
            'u' => .uint,
            'f' => .float,
            else => return 0,
        },
        .width = inline for (@typeInfo(Parser.Scalar.Width).@"enum".fields) |ef| {
            if (util.strStarts(bytes[1..], ef.name[1..]))
                break @enumFromInt(ef.value);
        } else return 0,
    };

    const scalar_off: usize = if (scalar.width == ._8) 2 else 3;
    if (bytes.len <= scalar_off) return scalar_off;

    var lengthes: [2]usize = .{ 0, 0 };
    var dim: usize = 0;
    for (0..2) |i| {
        const off = scalar_off + i * 2 + 1;
        if (bytes.len <= off) break;
        if (bytes[off - 1] == 'x') {
            if (bytes.len <= off) return 0;
            lengthes[i] = switch (bytes[off]) {
                '2'...'4' => @intCast(bytes[off] - '1'),
                else => return 0,
            };
            dim += 1;
        }
    }
    const type_len = scalar_off + dim * 2;
    if (bytes.len > type_len and isIdentifierChar(bytes[type_len])) return 0;

    return type_len;
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
    if (!letter)
        return self.errorOut(.{
            .source_offset = bytes.ptr - self.full_source.ptr,
            .kind = .invalid_character,
        });

    return bytes[0..end];
}
fn matchOperator(self: *Tokenizer, bytes: []const u8, bin_priority: bool) ?FatTokenEntry {
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
        if (util.strStarts(bytes, pair[1])) break self.createFatEntry(pair[0], bytes[0..pair[1].len]);
    } else self.createFatEntry(switch (bytes[0]) {
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

pub const TokenEntry = packed struct {
    kind: TokenKind,
    offset: u32,
    pub const eof: TokenEntry = .{ .kind = .eof, .offset = 0 };
};
pub const FatTokenEntry = packed struct(u64) {
    kind: TokenKind,
    len: u24,
    offset: u32,
};
pub fn slice(self: Tokenizer, token: Token) []const u8 {
    const entry = self.getEntry(token);
    const entry_len: usize = switch (entry.kind) {
        .identifier, .type_literal, .builtin => return self.rawSlice(token),
        .int_literal, .float_literal => 1,
        else => 1,
    };
    return self.full_source[entry.offset .. entry.offset + entry_len];
}
pub fn rawSlice(self: Tokenizer, token: Token) []const u8 {
    const off = self.offset(token);
    const bytes = self.full_source[off..];
    const count = for (bytes[1..], 1..) |char, i| {
        if (!isIdentifierChar(char)) break i;
    } else bytes.len;
    return self.full_source[off .. off + count];
}

pub fn offset(self: Tokenizer, token: Token) u32 {
    return if (use_multi_array_list)
        self.list.items(.offset)[token]
    else
        self.list.items[token].offset;
}
pub fn kind(self: Tokenizer, token: Token) TokenKind {
    return if (token < self.tokenCount())
        if (use_multi_array_list)
            self.list.items(.kind)[token]
        else
            self.list.items[token].kind
    else blk: {
        @branchHint(.unlikely);
        break :blk .eof;
    };
}
pub inline fn tokenCount(self: Tokenizer) Token {
    return @truncate(if (use_multi_array_list) self.list.len else self.list.items.len);
}
inline fn getEntry(self: Tokenizer, token: Token) TokenEntry {
    return if (token < self.tokenCount())
        if (use_multi_array_list)
            self.list.get(token)
        else
            self.list.items[token]
    else blk: {
        @branchHint(.unlikely);
        break :blk .eof;
    };
}

pub inline fn binOpFromTokenKind(token_kind: TokenKind) ?BinaryOperator {
    const f = @intFromEnum(__bin_op_from);
    const t = @intFromEnum(__bin_op_to);
    return if (@intFromEnum(token_kind) >= f and @intFromEnum(token_kind) <= t) //
        @enumFromInt(@intFromEnum(token_kind) - f)
    else
        null;
}
pub const BinaryOperator = util.EnumSlice(TokenKind, u8, __bin_op_from, __bin_op_to);
const __bin_op_from: TokenKind = .add;
const __bin_op_to: TokenKind = .dotsat;

pub inline fn uOpFromTokenKind(token_kind: TokenKind) ?UnaryOperator {
    const f = @intFromEnum(__u_op_from);
    const t = @intFromEnum(__u_op_to);
    return if (@intFromEnum(token_kind) >= f and @intFromEnum(token_kind) <= t) //
        @enumFromInt(@intFromEnum(token_kind) - f)
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
    return Error.SyntaxError;
}
const List = if (use_multi_array_list) std.MultiArrayList else std.ArrayList;
const ErrorInfo = error_message.TokenizerErrorInfo;
const Error = hgsl.Error;
