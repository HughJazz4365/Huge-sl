const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub fn formatStatement(statement: Statement, writer: *std.Io.Writer) !void {
    _ = .{ statement, writer };
}
pub fn formatExpression(expr: Expression, writer: *std.Io.Writer) !void {
    switch (expr) {
        .bin_op => |bin_op| try writer.print("({f} {s} {f})", .{ bin_op.left, @tagName(bin_op.op), bin_op.right }),
        .u_op => |u_op| try writer.print("{s}{f}", .{ @tagName(u_op.op), u_op.target }),
        inline else => |_, tag| try writer.print("{s}", .{@tagName(tag)}),
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
