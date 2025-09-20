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

    pub fn ToZig(comptime @"type": Type) type {
        return switch (@"type") {
            inline else => |value| if (@hasDecl(@TypeOf(value), "ToZig")) value.ToZig() else @compileError("cant convert type to zig"),
        };
    }
    pub fn isComplete(self: Type) bool {
        return switch (self) {
            .unknown, .enum_literal => false,
            else => true,
        };
    }
    pub fn size(self: Type) usize {
        return switch (self) {
            .number => |number| @intFromEnum(number.width) >> 3,
            .vector => |vector| (Type.size(.{ .number = vector.child }) * @intFromEnum(vector.len) + 15) / 16 * 16,
            .array => |array| array.child.size() * array.len,
            .matrix => |matrix| Type.size(.{ .vector = matrix.column_type }) * @intFromEnum(matrix.len),
            .bool => 1,
            else => 0,
        };
    }
    pub fn constructorStructure(self: Type) ConstructorStructure {
        return switch (self) {
            .vector => |vector| .{ .component = .{ .number = vector.child }, .len = @intFromEnum(vector.len) },
            .array => |array| .{ .component = array.child.*, .len = array.len },
            .matrix => |matrix| .{ .component = .{ .vector = matrix.column_type }, .len = @intFromEnum(matrix.len) },
            else => .{ .component = self },
        };
    }
};
pub const ConstructorStructure = struct {
    component: Type,
    len: u32 = 1,
};

pub const FunctionType = struct {
    rtype: *Type,
    args_types: []Type,
};

pub const ShaderStage = enum { vertex, fragment, compute };

pub const Matrix = struct {
    len: VectorLen,
    column_type: Vector,
};
pub const Array = struct {
    child: *Type,
    len: u32,
    pub fn ToZig(comptime arr: Array) type {
        return [arr.len](arr.child.*).ToZig();
    }
};

pub const Vector = struct {
    child: Number,
    len: VectorLen,
    pub fn ToZig(comptime vec: Vector) type {
        return @Vector(@intCast(@intFromEnum(vec.len)), vec.child.ToZig());
    }
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
