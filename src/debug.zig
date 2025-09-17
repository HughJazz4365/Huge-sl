const std = @import("std");
const tp = @import("type.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub fn formatStatement(statement: Statement, writer: *std.Io.Writer) !void {
    switch (statement) {
        .var_decl => |var_decl| {
            try writer.print("{s} {s} : {f}", .{ @tagName(var_decl.variable.qualifier), var_decl.variable.name, var_decl.variable.type });
            if (var_decl.value) |val| try writer.print(" = {f}", .{val});
        },
        else => try writer.print("[STATEMENT]", .{}),
    }
}

pub fn formatType(t: tp.Type, writer: *std.Io.Writer) !void {
    switch (t) {
        .entrypoint => |ep| try writer.print("entrypoint(.{s})", .{@tagName(ep)}),
        else => try writer.print("{s}", .{@tagName(t)}),
    }
}

pub fn formatExpression(expr: Expression, writer: *std.Io.Writer) !void {
    switch (expr) {
        .val => |v| try writer.print("{f}", .{v}),
        .bin_op => |bin_op| try writer.print("({f} {s} {f})", .{ bin_op.left, @tagName(bin_op.op), bin_op.right }),
        .u_op => |u_op| try writer.print("{s}{f}", .{ @tagName(u_op.op), u_op.target }),
        inline else => |_, tag| try writer.print("{s}", .{@tagName(tag)}),
    }
}
pub fn formatValue(value: Parser.Value, writer: *std.Io.Writer) !void {
    switch (value.type) {
        .compint => try writer.print("{d}", .{@as(i128, @bitCast(value.payload.wide))}),
        .compfloat => try writer.print("{d}", .{@as(f128, @bitCast(value.payload.wide))}),
        // .number => |num| try writer.print("{d}"
        else => try writer.print("[VAL]", .{}),
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
