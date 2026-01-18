const std = @import("std");
const util = @import("util.zig");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");

pub const Type = union(enum) {
    //no physical layout
    unknown: *const Expression,
    type_of: *const Expression,

    void,
    type,
    bool,
    compint,
    compfloat,

    //scalar & composite
    scalar: Scalar,

    vector: Vector,
    matrix: Matrix,
    array: Array,

    //buffer address
    pointer: Pointer,
    //*f32 - buffer ptr to f32
    //some_ptr.* - ptr dereference

    //decl scopes
    // @"enum": Enum,
    @"struct": Struct,

    //block scopes
    // function: FunctionType,
    entry_point: Parser.StageInfo,
    pub const format = @import("debug.zig").formatType;

    pub fn toExpr(self: Type) Expression {
        return .{ .value = .create(.type, self) };
    }
    pub const Tag = std.meta.Tag(@This());
};
pub const Struct = struct {
    id: StructID,

    //parameters that are independent of 'struct{}' declaration
    alignment: Alignment = .scalar,
    layout: Layout = .unordered,
    pub const Layout = enum { ordered, unordered };
    pub const Alignment = enum { scalar, base, extended };
};
pub const Pointer = struct {
    child: *const Type,
    alignment: usize,
};
pub const Array = struct {
    elem: *const Type,
    len: u32,
};
pub const Matrix = struct {
    component: Scalar,
    m: Vector.Len,
    n: Vector.Len,
};
pub const Vector = struct {
    component: Scalar,
    len: Len,
    pub const Len = enum(u32) { _2 = 2, _3 = 3, _4 = 4 };
};
pub const Scalar = struct {
    width: Width,
    layout: Layout,
    pub const Width = enum(u32) { _8 = 8, _16 = 16, _32 = 32, _64 = 64 };
    pub const Layout = enum(u32) { float, uint, int };
    pub const all: [4 * 3 - 1]Scalar = blk: {
        const len = 4 * 3 - 1;
        var result: [len]Scalar = undefined;
        var count: usize = 0;
        for (@typeInfo(Width).@"enum".fields) |wef| {
            for (@typeInfo(Layout).@"enum".fields) |lef| {
                const s: Scalar = .{
                    .width = @as(Width, @enumFromInt(wef.value)),
                    .layout = @as(Layout, @enumFromInt(lef.value)),
                };
                if (s.width == ._8 and s.layout == .float) continue;
                result[count] = s;
                count += 1;
            }
        }
        break :blk result;
    };
    pub fn ToZig(comptime scalar: Scalar) type {
        return if (scalar.layout == .float)
            std.meta.Float(@max(16, @intFromEnum(scalar.width)))
        else
            @Int(if (scalar.layout == .int) .signed else .unsigned, @intFromEnum(scalar.width));
    }
};

const Expression = Parser.Expression;
const Error = hgsl.Error;
const StructID = Parser.StructID;
