const std = @import("std");
const root = @import("root.zig");
const Tokenizer = @import("Tokenizer.zig");
pub const ErrorInfo = union(enum) {
    //syntax error
    unexpected_token: FatToken,
    float8,

    //
    undeclared_identifier: []const u8,
    dependency_loop: []const u8,
    unclosed_scope,
    cant_implicitly_cast,

    unexpected_return,
    unreachable_statement,
    return_type_mismatch,
    non_void_ignore,

    unknown,
};

pub fn printErrorMessage(writer: *std.Io.Writer, error_info: ErrorInfo) Error!void {
    switch (error_info) {
        .unexpected_token => |token| //
        try writer.print("unexpected token: {f}\n", .{token.token}),
        .unknown => try writer.print("unknown error\n", .{}),
        .dependency_loop => |dependency_loop| try writer.print("'{s}' depends on itself\n", .{dependency_loop}),
        .undeclared_identifier => |undeclared_identifier| try writer.print("undeclared identifier '{s}'\n", .{undeclared_identifier}),
        .float8 => try writer.print("floats cant have bit width of 8\n", .{}),
        else => try writer.print("error: {s}\n", .{@tagName(error_info)}),
    }
    try writer.flush();
}
pub fn errorOut(error_info: ErrorInfo) Error {
    return switch (error_info) {
        else => Error.SyntaxError,
    };
}
const Error = root.Error;
const Token = Tokenizer.Token;
const FatToken = Tokenizer.FatToken;
