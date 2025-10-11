const std = @import("std");
const util = @import("util.zig");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");

pub const InterpolationQualifier = enum { smooth, flat, noperspective };
pub const UniformAccessQualifier = enum { private, public };

pub fn getBuiltin(name: []const u8) Parser.Error!Builtin {
    return name_map.get(name) orelse Parser.Error.InvalidBuiltin;
}
const name_map: std.StaticStringMap(Builtin) = .initComptime(.{
    .{ "colHex", colHex },
    .{ "reflect", reflect },
});
const Builtin = struct {
    type: Type,
};

const reflect: Builtin = .{
    .type = .{ .function = .{
        .rtype = &vec3_type,
        .arg_types = &.{ vec3_type, vec3_type },
    } },
};
const colHex: Builtin = .{
    .type = .{ .function = .{
        .rtype = &vec3_type,
        .arg_types = &.{u32_type},
    } },
};
const u32_type: Type = .{ .number = .{ .type = .uint, .width = .word } };
const vec3_type: Type = .{ .vector = .{ .len = ._3, .component = .{ .type = .float, .width = .word } } };

const Type = tp.Type;
const Expression = Parser.Expression;
const Statement = Parser.Statement;
const FunctionType = tp.FunctionType;
