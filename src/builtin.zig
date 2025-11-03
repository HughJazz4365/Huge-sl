const std = @import("std");
const util = @import("util.zig");
const tp = @import("type.zig");
const ct = @import("comptime.zig");
const Parser = @import("Parser.zig");

pub const Interpolation = enum(u64) { smooth = 0, flat = 1, noperspective = 2 };
pub fn getBuiltin(name: []const u8) Error!Builtin {
    return name_map.get(name) orelse Error.InvalidBuiltin;
}
const name_map: std.StaticStringMap(Builtin) = .initComptime(.{
    //single instruction
    .{ "reflect", Builtin{ .function = .reflect } },
    .{ "transpose", Builtin{ .function = .transpose } },
    .{ "inverse", Builtin{ .function = .inverse } },
    .{ "colHex", Builtin{ .function = .col_hex } },

    //comptime
    .{ "Buffer", Builtin{ .function = .buffer } },

    //variables
    .{ "position", Builtin{ .variable = .position } },
    .{ "vertex_id", Builtin{ .variable = .vertex_id } },
});

pub const Builtin = union(enum) {
    function: BuiltinFunction,
    variable: BuiltinVariable,
};
const BuiltinFunction = enum {
    col_hex,
    reflect,
    array_type,
    transpose,
    inverse,
    buffer,

    pub fn argumentCount(self: BuiltinFunction) usize {
        return switch (self) {
            .reflect, .array_type => 2,
            else => 1,
        };
    }
    pub fn typeOf(self: BuiltinFunction) Type {
        return switch (self) {
            .buffer => .{ .function = .{
                .rtype = &Type{ .type = {} },
                .arg_types = &.{Type{ .type = {} }},
            } },
            else => .{ .unknown = @constCast(&Expression{ .builtin = .{ .function = self } }) },
        };
    }
};
pub const BuiltinVariable = enum {
    position,
    point_size,
    cull_distance,
    vertex_id,
    // invocation_id,

    pub fn ioDirection(bv: BuiltinVariable) enum { in, out } {
        return switch (bv) {
            .position, .point_size, .cull_distance => .out,
            else => .in,
        };
    }
    pub fn isMutable(bv: BuiltinVariable) bool {
        return switch (bv) {
            .position, .point_size, .cull_distance => true,
            else => false,
        };
    }
    pub fn typeOf(bv: BuiltinVariable) Type {
        return switch (bv) {
            .position => tp.vec4_type,
            .point_size => tp.f32_type,
            .cull_distance => Type{ .array = .{ .len = 1, .component = &tp.f32_type } },
            .vertex_id => tp.u32_type,
        };
    }
};

pub fn refineBuiltinCall(self: *Parser, bf: BuiltinFunction, args: []Expression, callee_ptr: *Expression) Error!Expression {
    if (args.len != bf.argumentCount()) return self.errorOut(Error.NonMatchingArgumentCount);
    const initial: Expression = .{ .call = .{ .callee = callee_ptr, .args = args } };
    return switch (bf) {
        //remove in favour of unpackUnorm4x8 instruction
        .col_hex => blk: {
            //not create val if we dont have to
            const hex = try self.createVal(try self.turnIntoIntermediateVariableIfNeeded(args[0]));

            const components = try self.arena.allocator().alloc(Expression, 3);
            for (0..3) |i| {
                components[2 - i] = .{ .cast = .{
                    .type = tp.f32_type,
                    .expr = try self.createVal(Expression{ .bin_op = .{
                        .left = try self.createVal(Expression{ .bin_op = .{
                            .left = hex,
                            .op = .@">>",
                            .right = try self.createVal(Expression{ .value = .{
                                .type = tp.u32_type,
                                .payload = .{ .wide = util.fit(Parser.WIDE, @as(u32, @intCast(i * 8))) },
                            } }),
                        } }),
                        .op = .@"&",
                        .right = @constCast(&oxFF),
                    } }),
                } };
            }

            break :blk try self.implicitCast(try ct.refineDescend(self, .{ .bin_op = .{
                .left = try self.createVal(Expression{ .constructor = .{
                    .type = tp.vec3_type,
                    .components = components,
                } }),
                .op = .@"*",
                .right = @constCast(&inv_oxFF),
            } }), tp.vec3_type);
        },
        .buffer => blk: {
            if (args[0] != .value) break :blk initial;
            const @"type" = args[0].value.payload.type;
            if (@"type" != .@"struct") return self.errorOut(Error.InvalidCall);
            break :blk .{ .value = .{ .type = .type, .payload = .{
                .type = .{ .buffer = @"type".@"struct" },
            } } };
        },

        else => initial,
    };
}
pub fn typeOfBuiltInCall(self: *Parser, bf: BuiltinFunction, args: []Expression) Error!Type {
    if (args.len != bf.argumentCount()) return self.errorOut(Error.NonMatchingArgumentCount);
    const @"type": Type = switch (bf) {
        .col_hex => tp.vec3_type,
        .array_type, .buffer => Type{ .type = {} },
        else => try self.typeOf(args[0]),
    };
    // std.debug.print("@{s} => {f}\n", .{ @tagName(bf), @"type" });

    return @"type";
}
pub fn typeOfBuiltin(self: *Parser, builtin: Builtin) Error!Type {
    _ = self;
    return if (builtin == .variable)
        builtin.variable.typeOf()
    else
        builtin.function.typeOf();
}
const oxFF = Expression{ .value = .{
    .type = .compint,
    .payload = .{ .wide = 0xFF },
} };
const inv_oxFF = Expression{ .value = .{
    .type = .compfloat,
    .payload = .{ .wide = util.fit(Parser.WIDE, @as(Parser.CF, 1.0 / 255.0)) },
} };
pub const stage_type: Type = .{ .@"enum" = .fromZig(Parser.ShaderStage) };
pub const interpolation_type: Type = .{ .@"enum" = .fromZig(Interpolation) };

const Type = tp.Type;
const Expression = Parser.Expression;
const Value = Parser.Value;
const Statement = Parser.Statement;
const FunctionType = tp.FunctionType;
const Error = Parser.Error;
