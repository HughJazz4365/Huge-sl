const std = @import("std");
const util = @import("util.zig");
const tp = @import("type.zig");
const ct = @import("comptime.zig");
const Parser = @import("Parser.zig");

pub const InterpolationQualifier = enum { smooth, flat, noperspective };
pub const UniformAccessQualifier = enum { private, public };

pub fn refineBuiltinCall(self: *Parser, bf: BuiltinFunction, args: []Expression, callee_ptr: *Expression) Error!Expression {
    if (args.len != bf.argumentCount()) return self.errorOut(Error.NonMatchingArgumentCount);
    const initial: Expression = .{ .call = .{ .callee = callee_ptr, .args = args } };
    return switch (bf) {
        .col_hex => blk: {
            const components = try self.arena.allocator().alloc(Expression, 3);
            for (0..3) |i| {
                components[i] = .{ .bin_op = .{
                    .left = try self.createVal(Expression{ .cast = .{
                        .type = f32_type,
                        .target = try self.createVal(Expression{ .bin_op = .{
                            .left = try self.createVal(Expression{ .bin_op = .{
                                .left = &args[0],
                                .op = .@">>",
                                .right = try self.createVal(Expression{ .value = .{
                                    .type = u32_type,
                                    .payload = .{ .wide = util.fit(u128, @as(u32, i * 8)) },
                                } }),
                            } }),
                            .op = .@"|",
                            .right = oxAA,
                        } }),
                    } }),
                    .op = .@"*",
                    .right = inv_oxAA,
                } };
            }

            break :blk try self.implicitCast(ct.refineDescend(.{ .constructor = .{
                .type = vec3_type,
                .components = components,
            } }), vec3_type);
        },
        else => initial,
    };
}
pub fn typeOfBuiltInCall(self: *Parser, bf: BuiltinFunction, args: []Expression) Error!Type {
    if (args.len != bf.argumentCount()) return self.errorOut(Error.NonMatchingArgumentCount);
    const @"type": Type = switch (bf) {
        .col_hex => vec3_type,
        .reflect => try self.typeOf(args[0]),
        // else => .{ .unknown = try self.createVal(Expression{ .builtin = .{ .function = bf } }) },
    };
    std.debug.print("@{s} => {f}\n", .{ @tagName(bf), @"type" });

    return @"type";
}
pub fn typeOfBuiltin(self: *Parser, builtin: Builtin) Error!Type {
    return .{ .unknown = try self.createVal(Expression{ .builtin = builtin }) };
}
pub fn getBuiltin(name: []const u8) Error!Builtin {
    return name_map.get(name) orelse Error.InvalidBuiltin;
}
const name_map: std.StaticStringMap(Builtin) = .initComptime(.{
    .{ "reflect", Builtin{ .function = .reflect } },
    .{ "colHex", Builtin{ .function = .col_hex } },
});

pub const Builtin = union(enum) {
    function: BuiltinFunction,
    variable: enum { vav },
};
const BuiltinFunction = enum {
    col_hex,
    reflect,
    pub fn argumentCount(self: BuiltinFunction) usize {
        return switch (self) {
            .col_hex => 1,
            .reflect => 2,
            // else => 0,
        };
    }
};
const oxAA = Expression{ .value = .{
    .type = .compint,
    .payload = .{ .wide = 0xAA },
} };
const inv_oxAA = Expression{ .value = .{
    .type = .compfloat,
    .payload = .{ .wide = util.fit(u128, @as(f128, 1.0 / 0xAA)) },
} };
const u32_type: Type = .{ .number = .{ .type = .uint, .width = .word } };
const f32_type: Type = .{ .number = .{ .type = .float, .width = .word } };
const vec3_type: Type = .{ .vector = .{ .len = ._3, .component = .{ .type = .float, .width = .word } } };

const Type = tp.Type;
const Expression = Parser.Expression;
const Value = Parser.Value;
const Statement = Parser.Statement;
const FunctionType = tp.FunctionType;
const Error = Parser.Error;
