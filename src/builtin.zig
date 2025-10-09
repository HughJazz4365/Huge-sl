const std = @import("std");
const util = @import("util.zig");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");

pub const InterpolationQualifier = enum { smooth, flat, noperspective };
pub const UniformAccessQualifier = enum { private, public };

const col_hex_function: BuiltinFunction = .{
    .body = &.{},
    .type = intToVec3FT,
};

pub const BuiltinFunction = struct {
    body: []const Statement,
    type: tp.FunctionType,
};
const intToVec3FT: tp.FunctionType = .{
    .rtype = vec3_type,
    .arg_types = &.{i32_type},
};
const i32_type: Type = .{ .number = .{ .type = .int, .width = .word } };
const vec3_type: Type = .{ .vector = .{ .len = ._3, .component = .{ .type = .float, .width = .word } } };

const Type = tp.Type;
const Expression = Parser.Expression;
const Statement = Parser.Statement;
