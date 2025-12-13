const std = @import("std");
const hgsl = @import("root.zig");
const util = @import("util.zig");
const tp = @import("type.zig");
const ct = @import("comptime.zig");
const Parser = @import("Parser.zig");

// pub const BufferType = enum(u64)

pub fn getBuiltin(name: []const u8) Error!Builtin {
    return name_map.get(name) orelse Error.InvalidBuiltin;
}
const name_map: std.StaticStringMap(Builtin) = .initComptime(.{
    //single instruction
    .{ "reflect", Builtin{ .function = .reflect } },
    .{ "transpose", Builtin{ .function = .transpose } },
    .{ "inverse", Builtin{ .function = .inverse } },

    .{ "ceil", Builtin{ .function = .ceil } },
    .{ "floor", Builtin{ .function = .floor } },

    .{ "colHex", Builtin{ .function = .col_hex } },

    //comptime
    .{ "Buffer", Builtin{ .function = .buffer } },
    .{ "Texture", Builtin{ .function = .texture } },
    .{ "TypeOf", Builtin{ .function = .type_of } },

    //variables
    .{ "position", Builtin{ .variable = .position } },
    .{ "frag_coord", Builtin{ .variable = .frag_coord } },
    .{ "vertex_id", Builtin{ .variable = .vertex_id } },
    .{ "Interpolation", Builtin{ .variable = .interpolation } },
});

pub const Builtin = union(enum) {
    function: BuiltinFunction,
    variable: BuiltinVariable,
};
pub const BuiltinFunction = enum {
    reflect,
    transpose,
    inverse,

    ceil,
    floor,

    col_hex,

    //comptime
    buffer,
    texture,
    type_of,

    //internal
    array_type,
    sample,

    pub fn argumentCount(self: BuiltinFunction) usize {
        return self.typeOf().function.arg_types.len;
    }
    pub fn typeOf(self: BuiltinFunction) Type {
        return switch (self) {
            .buffer => .{ .function = .{
                .rtype = &.type,
                .arg_types = &.{ .type, buffer_type_type },
            } },
            .texture => .{ .function = .{
                .rtype = &.type,
                .arg_types = &.{ .type, texture_type_type, .bool },
            } },
            .ceil,
            .floor,
            .transpose,
            .inverse,
            => .{ .function = .{
                .rtype = &.unknownempty,
                .arg_types = &.{.unknownempty},
            } },
            .reflect => .{ .function = .{
                .rtype = &.unknownempty,
                .arg_types = &.{ .unknownempty, .unknownempty },
            } },
            .type_of => .{ .function = .{
                .rtype = &.type,
                .arg_types = &.{.unknownempty},
            } },

            else => @panic("unknown builtin function type"),
        };
    }
};
pub const BuiltinVariable = enum {
    position,
    frag_coord,

    point_size,
    cull_distance,
    vertex_id,
    interpolation,
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
            .frag_coord => tp.vec3_type,
            .point_size => tp.f32_type,
            .cull_distance => .{ .array = .{ .len = 1, .component = &tp.f32_type } },
            .vertex_id => tp.u32_type,
            .interpolation => .type,
        };
    }
};

pub fn refineBuiltinCall(self: *Parser, bf: BuiltinFunction, args: []Expression, callee_ptr: *Expression) Error!Expression {
    if (args.len != bf.argumentCount()) return self.errorOut(Error.NonMatchingArgumentCount);
    const initial: Expression = .{ .call = .{ .callee = callee_ptr, .args = args } };
    return switch (bf) {
        .type_of => blk: {
            const type_of = self.typeOf(args[0]);
            break :blk if (type_of.isEmpty())
                initial
            else
                type_of.asExpr();
        },
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
            if (args[0] != .value or args[1] != .value) break :blk initial;
            const @"type" = args[0].value.payload.type;
            if (@"type" != .@"struct") return self.errorOut(Error.InvalidCall);

            break :blk .{ .value = .{ .type = .type, .payload = .{
                .type = .{ .buffer = .{
                    .struct_id = @"type".@"struct",
                    .type = @enumFromInt(util.extract(u64, args[1].value.payload.wide)),
                } },
            } } };
        },
        .texture => blk: {
            if (args[0] != .value or args[1] != .value or args[2] != .value) break :blk initial;
            const @"type" = args[0].value.payload.type;
            if (@"type" != .scalar) return self.errorOut(Error.InvalidCall);
            const texture_type: tp.TextureType = @enumFromInt(util.extract(u64, args[1].value.payload.wide));

            const is_sampled = util.extract(bool, args[2].value.payload.wide);

            break :blk .{ .value = .{ .type = .type, .payload = .{
                .type = .{ .texture = .{
                    .texel_primitive = @"type".scalar,
                    .type = texture_type,
                    .sampled = is_sampled,
                } },
            } } };
        },

        else => initial,
    };
}
pub fn typeOfBuiltInCall(self: *Parser, bf: BuiltinFunction, args: []Expression) Type {
    if (args.len != bf.argumentCount()) return .unknownempty;
    const @"type": Type = switch (bf) {
        .col_hex => tp.vec3_type,
        .array_type, .buffer, .texture => .type,
        // else => bf.typeOf().function.rtype.*,
        else => self.typeOf(args[0]),
    };
    // std.debug.print("@{s} => {f}\n", .{ @tagName(bf), @"type" });

    return @"type";
}
pub fn typeOfBuiltin(builtin: Builtin) Type {
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
pub const stage_type: Type = .{ .@"enum" = .fromZig(Parser.Stage) };

pub const texture_type_type: Type = .{ .@"enum" = .fromZig(tp.TextureType) };
pub const interpolation_type: Type = .{ .@"enum" = .fromZig(hgsl.Interpolation) };
pub const buffer_type_type: Type = .{ .@"enum" = .fromZig(tp.BufferType) };

const Type = tp.Type;
const Expression = Parser.Expression;
const Value = Parser.Value;
const Statement = Parser.Statement;
const FunctionType = tp.FunctionType;
const Error = Parser.Error;
