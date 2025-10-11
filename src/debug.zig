const std = @import("std");
const tp = @import("type.zig");
const util = @import("util.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub fn formatStatement(statement: Statement, writer: *std.Io.Writer) !void {
    switch (statement) {
        .var_decl => |var_decl| {
            try writer.print("{s} {s} : {f} = {f}", .{ @tagName(var_decl.qualifier), var_decl.name, var_decl.type, var_decl.initializer });
        },
        .assignment => |ass| try writer.print("{f} = {f}", .{
            ass.target,
            ass.value,
        }),

        .@"return" => |@"return"| {
            try writer.print("return", .{});
            if (@"return") |r| try writer.print(" {f}", .{r});
        },
        else => try writer.print("[{s}]", .{@tagName(statement)}),
    }
}

pub fn formatType(t: tp.Type, writer: *std.Io.Writer) !void {
    switch (t) {
        .entrypoint => |ep| try writer.print("entrypoint(.{s})", .{@tagName(ep)}),
        .number => |n| try writer.print("{s}{d}", .{ n.type.prefix(), @intFromEnum(n.width) }),
        .vector => |v| try writer.print("{s}vec{d}", .{
            if (v.component.type == .float) "" else v.component.type.prefix(),
            @intFromEnum(v.len),
        }),
        .unknown => try writer.print("[UNKNOWNTYPE]", .{}),
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
        .builtin => |b| try writer.print("@{s}", .{b}),
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
            try writer.print("{f}(", .{call.callee.*});
            for (call.args, 0..) |arg, i| try writer.print("{f}{s}", .{ arg, if (i + 1 < call.args.len) ", " else ")" });
        },
        inline else => |_, tag| try writer.print("{s}", .{@tagName(tag)}),
    }
}
pub fn formatValue(value: Parser.Value, writer: *std.Io.Writer) !void {
    switch (value.type) {
        .compint => try writer.print("{d}", .{@as(i128, @bitCast(value.payload.wide))}),
        .compfloat => try writer.print("{d}", .{@as(f128, @bitCast(value.payload.wide))}),
        // .number => |num| try writer.print("{d}"
        .entrypoint => {
            try writer.print("{f}{{\n", .{value.type});

            const entry_point: *const Parser.EntryPoint = @ptrCast(@alignCast(value.payload.ptr));
            for (entry_point.body.items) |statement| try writer.print("{f}\n", .{statement});
            try writer.print("}}\n", .{});
        },
        .number => |number| switch (number.type) {
            inline else => |@"type"| switch (number.width) {
                inline else => |width| try writer.print("[{f}]|{d}|", .{
                    value.type,
                    util.extract((tp.Number{ .type = @"type", .width = width }).ToZig(), value.payload.wide),
                }),
                // inline else => |width| try writer.print("{d}", .{
                //     ct.wideAs((tp.Number{ .type = @"type", .width = width }).ToZig(), value.payload.wide),
                // }),
            },
        },
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.component.width) {
                inline else => |width| switch (vector.component.type) {
                    inline else => |num_type| std.debug.print("|{f}|{d}", .{
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
        .unknown => try writer.print("[?Value]", .{}),
        .function => {
            try writer.print("{f}{{\n", .{value.type});

            const func: *const Parser.Function = @ptrCast(@alignCast(value.payload.ptr));
            for (func.body.items) |statement| try writer.print("{f}\n", .{statement});
            try writer.print("}}\n", .{});
        },
        else => try writer.print("[{s}]", .{@tagName(value.type)}),
    }
}
pub fn formatToken(token: Token, writer: *std.Io.Writer) !void {
    switch (token) {
        .identifier => |id| try writer.print("[id]: {s}\n", .{id}),
        .type_literal => |tl| try writer.print("[type_literal]: {s}\n", .{@tagName(tl)}),
        inline else => |value, tag| try writer.print("[{s}]: {any}\n", .{ @tagName(tag), value }),
    }
}
const p = std.debug.print;
const Expression = Parser.Expression;
const Statement = Parser.Statement;
const Token = Parser.Token;
