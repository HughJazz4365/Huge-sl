const std = @import("std");
const tp = @import("type.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub fn formatStatement(statement: Statement, writer: *std.Io.Writer) !void {
    switch (statement) {
        .var_decl => |var_decl| {
            try writer.print("{s} {s} : {f}", .{ @tagName(var_decl.variable.qualifier), var_decl.variable.name, var_decl.variable.type });
            if (var_decl.value) |value| try writer.print(" = {f}", .{value});
        },
        .assignment => |ass| try writer.print("{f} {s}= {f}", .{
            ass.target,
            if (ass.modifier) |m| @tagName(m) else "",
            ass.value,
        }),

        else => try writer.print("[{s}]", .{@tagName(statement)}),
    }
}

pub fn formatType(t: tp.Type, writer: *std.Io.Writer) !void {
    switch (t) {
        .entrypoint => |ep| try writer.print("entrypoint(.{s})", .{@tagName(ep)}),
        .number => |n| try writer.print("{s}{d}", .{ n.type.prefix(), @intFromEnum(n.width) }),
        .vector => |v| try writer.print("{s}vec{d}", .{
            if (v.child.type == .float) "" else v.child.type.prefix(),
            @intFromEnum(v.len),
        }),
        else => try writer.print("{s}", .{@tagName(t)}),
    }
}

pub fn formatExpression(expr: Expression, writer: *std.Io.Writer) !void {
    switch (expr) {
        .identifier => |id| try writer.print("{s}", .{id}),
        .value => |v| try writer.print("{f}", .{v}),
        .bin_op => |bin_op| try writer.print("({f} {s} {f})", .{ bin_op.left, @tagName(bin_op.op), bin_op.right }),
        .u_op => |u_op| try writer.print("{s}{f}", .{ @tagName(u_op.op), u_op.target }),
        .tuple => |tuple| {
            try writer.print(".{{ ", .{});
            for (tuple, 0..) |elem, i| {
                try writer.print("{f}{s}", .{
                    elem,
                    if (i + 1 >= tuple.len) "" else ", ",
                });
            }
            try writer.print(" }}", .{});
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
