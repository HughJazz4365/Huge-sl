const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub fn print(value: anytype) void {
    switch (@TypeOf(value)) {
        Expression => printExpression(value),
    }
}
fn printExpression(expr: Expression) void {
    switch (expr) {
        //writer
        inline else => |_, tag| p("[{s}]\n", .{@tagName(tag)}),
    }
}
const p = std.debug.print;
const Expression = Parser.Expression;
