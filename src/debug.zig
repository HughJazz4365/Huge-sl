const std = @import("std");
const tp = @import("type.zig");
const ct = @import("comptime.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub fn formatStatement(statement: Statement, writer: *std.Io.Writer) !void {
    switch (statement) {
        .var_decl => |var_decl| {
            try writer.print("{s} {s} : {f} = {f}", .{ @tagName(var_decl.qualifier), var_decl.name, var_decl.type, var_decl.value });
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
        .unknown => try writer.print("[?]", .{}),
        else => try writer.print("{s}", .{@tagName(t)}),
    }
}

pub fn formatExpression(expr: Expression, writer: *std.Io.Writer) !void {
    switch (expr) {
        .identifier => |id| try writer.print("{s}", .{id}),
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
                    ct.wideAs((tp.Number{ .type = @"type", .width = width }).ToZig(), value.payload.wide),
                }),
                // inline else => |width| try writer.print("{d}", .{
                //     ct.wideAs((tp.Number{ .type = @"type", .width = width }).ToZig(), value.payload.wide),
                // }),
            },
        },
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.child.width) {
                inline else => |width| switch (vector.child.type) {
                    inline else => |num_type| std.debug.print("|{f}|{d}", .{
                        value.type,
                        @as(*const (tp.Vector{ .len = len, .child = .{ .width = width, .type = num_type } }).ToZig(), @ptrCast(@alignCast(value.payload.ptr))).*,
                    }),
                },
            },
        },
        .unknown => try writer.print("[?]", .{}),
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
