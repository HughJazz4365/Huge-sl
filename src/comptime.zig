const Parser = @import("Parser.zig");

pub fn simplify(self: *Parser, expr: Expression) Error!Expression {
    _ = self;
    return switch (expr) {
        else => expr,
    };
}
pub fn simplifyTarget(self: *Parser, expr: Expression) Error!Expression {
    _ = self;
    return expr;
}

const Error = Parser.Error;
const Expression = Parser.Expression;
