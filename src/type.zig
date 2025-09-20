const std = @import("std");
const Parser = @import("Parser.zig");

pub const Type = union(enum) {
    void,
    type,
    bool,

    //intermediate compile time types
    unknown,
    enum_literal,
    compint,
    compfloat,

    number: Number,
    vector: Vector,
    array: Array,
    matrix: Matrix,

    @"struct",
    buffer,
    image,

    @"enum",

    function: FunctionType,
    entrypoint: ShaderStage,

    pub const format = @import("debug.zig").formatType;
    pub fn isComplete(self: Type) bool {
        return switch (self) {
            .unknown, .enum_literal => false,
            else => true,
        };
    }
};

pub const FunctionType = struct {
    rtype: *Type,
    args_types: []Type,
};

pub const ShaderStage = enum { vertex, fragment, compute };

pub const Matrix = struct {
    rows: VectorLen,
    column: Vector,
};
pub const Array = struct {
    child: *Type,
    len: u32,
};

pub const Vector = struct {
    child: Number,
    len: VectorLen,
    pub fn literalComp(comptime vec: Vector) []const u8 {
        if (vec.child.width != .word) @compileError(std.fmt.comptimePrint(
            "no builtin literal for vector with child bitwidth of {d}",
            .{vec.child.width},
        ));

        const prefix = switch (vec.child.type) {
            .float => "",
            .int => "i",
            .uint => "u",
        };
        return prefix ++ "vec" ++ .{"__234"[@intFromEnum(vec.len)]};
    }
    pub fn allVectors() []const Vector {
        comptime var slice: []const Vector = &.{};
        inline for (@typeInfo(NumberType).@"enum".fields) |nef| {
            inline for (@typeInfo(VectorLen).@"enum".fields) |lef| {
                const v: Vector = .{
                    .child = .{ .type = @enumFromInt(nef.value), .width = .word },
                    .len = @enumFromInt(lef.value),
                };
                slice = slice ++ &[1]Vector{v};
            }
        }
        return slice;
    }
};
const VectorLen = enum(u8) { _2 = 2, _3 = 3, _4 = 4 };

pub const Number = struct {
    type: NumberType,
    width: BitWidth,

    pub fn ToZig(comptime num: Number) type {
        const width = @intFromEnum(num.width);
        return @Type(
            if (num.type == .float)
                .{ .float = .{ .bits = width } }
            else
                .{ .int = .{ .bits = width, .signedness = if (num.type == .int) .signed else .unsigned } },
        );
    }
    pub fn allNumbers() []const Number {
        comptime var slice: []const Number = &.{};
        inline for (@typeInfo(NumberType).@"enum".fields) |nef| {
            inline for (@typeInfo(BitWidth).@"enum".fields) |wef| {
                const n: Number = .{ .type = @enumFromInt(nef.value), .width = @enumFromInt(wef.value) };
                slice = slice ++ &[1]Number{n};
            }
        }
        return slice;
    }
    pub fn literalComp(comptime num: Number) []const u8 {
        comptime var literal: []const u8 = &.{};
        literal = literal ++ comptime num.type.prefix() ++ switch (num.width) {
            .short => "16",
            .word => "32",
            .long => "64",
        };
        return literal;
    }
};
pub const NumberType = enum {
    float,
    int,
    uint,
    pub fn prefix(num_type: NumberType) []const u8 {
        return switch (num_type) {
            .float => "f",
            .int => "i",
            .uint => "u",
        };
    }
};
pub const BitWidth = enum(u8) { short = 16, word = 32, long = 64 };

const Expression = Parser.Expression;
