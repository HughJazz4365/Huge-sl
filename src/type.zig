const std = @import("std");

pub const Type = union(enum) {
    void,
    type,
    bool,

    int,
    float,

    number: Number,
    vector: Vector,
    array: Array,
    matrix: Matrix,

    @"struct",
    buffer,
    image,

    @"enum",
    enum_literal,

    func: void,
    entrypoint: void,
};
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
};
pub const NumberType = enum { float, int, uint };
pub const BitWidth = enum(u8) { byte = 8, short = 16, word = 32, long = 64 };
