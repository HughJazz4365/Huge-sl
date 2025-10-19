const std = @import("std");
const tp = @import("type.zig");
const util = @import("util.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub fn formatStatement(statement: Statement, writer: *std.Io.Writer) !void {
    switch (statement) {
        .var_decl => |var_decl| {
            try writer.print("{s} {s}[refs: {d}] : {f} = {f}", .{ @tagName(var_decl.qualifier), var_decl.name, var_decl.reference_count, var_decl.type, var_decl.initializer });
        },
        .assignment => |ass| try writer.print("{f} = {f}", .{
            ass.target,
            ass.value,
        }),

        .@"return" => |returned| {
            try writer.print("return", .{});
            if (!returned.isEmpty()) try writer.print(" {f}", .{returned});
        },
        else => try writer.print("[{s}]", .{@tagName(statement)}),
    }
}

pub fn formatType(t: tp.Type, writer: *std.Io.Writer) !void {
    switch (t) {
        .entrypoint => |ep| try writer.print("entrypoint(.{s})", .{@tagName(ep)}),
        .scalar => |n| try writer.print("{s}{d}", .{ n.type.prefix(), @intFromEnum(n.width) }),
        .vector => |v| try writer.print("{s}vec{d}", .{
            if (v.component.type == .float) "" else v.component.type.prefix(),
            @intFromEnum(v.len),
        }),
        .unknown => |unknown| if (unknown.isEmpty()) {
            try writer.print("@TypeOf({f})", .{unknown.*});
        } else try writer.print("@UnknownType", .{}),
        .function => |f| {
            try writer.print("fn(", .{});
            for (f.arg_types, 0..) |at, i| try writer.print("{f}{s}", .{ at, if (i + 1 < f.arg_types.len) ", " else "" });
            try writer.print("){f}", .{f.rtype.*});
        },
        else => try writer.print("{s}", .{@tagName(t)}),
    }
}

pub fn formatExpression(expr: Expression, writer: *std.Io.Writer) !void {
    switch (expr) {
        .identifier => |id| try writer.print("{s}", .{id}),
        .builtin => |builtin| switch (builtin) {
            inline else => |val| try writer.print("@{s}", .{@tagName(val)}),
        },
        .value => |v| try writer.print("{f}", .{v}),
        .bin_op => |bin_op| try writer.print("({f} {s} {f})", .{ bin_op.left, @tagName(bin_op.op), bin_op.right }),
        .u_op => |u_op| try writer.print("{s}{f}", .{ @tagName(u_op.op), u_op.target }),
        .constructor => |constructor| {
            try writer.print("{f}{{ ", .{constructor.type});
            for (constructor.components, 0..) |component, i| {
                try writer.print("{f}{s}", .{
                    component,
                    if (i + 1 >= constructor.components.len) "" else ", ",
                });
            }
            try writer.print(" }}", .{});
        },
        .cast => |cast| try writer.print("{f}{{ {f} }}", .{ cast.type, cast.expr.* }),
        .indexing => |indexing| try writer.print("{f}[{f}]", .{ indexing.target, indexing.index }),
        .call => |call| {
            if (call.callee.* == .value and call.callee.value.type == .function) {
                try writer.print("[FUNCTIONVALUE](", .{});
            } else try writer.print("{f}(", .{call.callee.*});
            for (call.args, 0..) |arg, i| try writer.print("{f}{s}", .{ arg, if (i + 1 < call.args.len) ", " else ")" });
        },
        inline else => |_, tag| try writer.print("{s}", .{@tagName(tag)}),
    }
}
pub fn formatValue(value: Parser.Value, writer: *std.Io.Writer) !void {
    switch (value.type) {
        .compint => try writer.print("{d}", .{util.extract(Parser.CI, value.payload.wide)}),
        .compfloat => try writer.print("{d}f", .{util.extract(Parser.CF, value.payload.wide)}),
        .entrypoint => {
            const entry_point: *const Parser.EntryPoint = @ptrCast(@alignCast(value.payload.ptr));
            try writer.print("{f}{{\n[global interfaces: {d}, local interfaces: {d}]\n", .{
                value.type,
                entry_point.global_interface_count,
                entry_point.interfaces.len - entry_point.global_interface_count,
            });
            for (entry_point.body.items) |statement| try writer.print("{f}\n", .{statement});
            try writer.print("}}\n", .{});
        },
        .scalar => |scalar| switch (scalar.type) {
            inline else => |st| switch (scalar.width) {
                inline else => |width| try writer.print("[{f}]|{d}|", .{
                    value.type,
                    util.extract((tp.Scalar{ .type = st, .width = width }).ToZig(), value.payload.wide),
                }),
            },
        },
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.component.width) {
                inline else => |width| switch (vector.component.type) {
                    inline else => |num_type| try writer.print("|{f}|{d}", .{
                        value.type,
                        @as(*const (tp.Vector{ .len = len, .component = .{ .width = width, .type = num_type } }).ToZig(), @ptrCast(@alignCast(value.payload.ptr))).*,
                    }),
                },
            },
        },
        .type => if (value.payload.type == .unknown)
            try writer.print("[EMPTY]", .{})
        else
            try writer.print("{f}", .{value.payload.type}),
        .function => {
            try writer.print("{f}{{\n", .{value.type});

            const func: *const Parser.Function = @ptrCast(@alignCast(value.payload.ptr));
            for (func.body.items, 0..) |statement, i|
                if (i < 2) (try writer.print("{f}\n", .{statement})) else {
                    try writer.print("...\n", .{});
                    break;
                };
            try writer.print("}}\n", .{});
        },
        .unknown, .void => try writer.print("[EMPTY]", .{}),
        else => try writer.print("[{s}]", .{@tagName(value.type)}),
    }
}
pub fn formatToken(token: Token, writer: *std.Io.Writer) !void {
    switch (token) {
        .identifier => |id| try writer.print("[id]: {s}", .{id}),
        .type_literal => |tl| try writer.print("[type_literal]: {f}", .{tl}),
        inline else => |value, tag| try writer.print("[{s}]: {any}", .{ @tagName(tag), value }),
    }
}
const Expression = Parser.Expression;
const Statement = Parser.Statement;
const Token = Parser.Token;
