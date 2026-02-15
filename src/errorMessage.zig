//TODO SEPARATION BETWEEN TOKENIZER ERROR INFO AND PARSING ERROR INFO
//TOKENIZER - {source_offset, kind}
//PARSER    - {node/token, extra}
const std = @import("std");
const root = @import("root.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
pub const TokenizerErrorInfo = struct {
    source_offset: usize,
    kind: Kind,
    const Kind = enum { invalid_character, float8 };
};
pub const ParserErrorInfo = struct {
    pub const unknown: ParserErrorInfo = .{ .payload = .unknown };
    id: u32 = undefined,
    payload: Payload,
    pub const Payload = union(enum) {
        unexpected_token,
        entry_point_decl_arg_count,
        entry_point_type_decl_arg_count,
        undeclared_identifier,
        redeclaration,
        dependency_loop,

        not_a_type,
        missing_initializer,
        missing_function_body,
        qualifier_cant_have_initializer,
        unclosed_scope,
        cant_implicitly_cast,
        invalid_type,

        qualifier_incompatible_with_type: QualifierIncompatibleWithType,

        unexpected_return,
        unreachable_statement,
        return_type_mismatch,
        non_void_ignore,

        unknown,
    };
};

const QualifierIncompatibleWithType = struct {
    var_decl: Node,
    type: Parser.Type,
};

pub fn errorOutParser(parser: *Parser, writer: *std.Io.Writer) Error {
    const error_info = parser.error_info;
    switch (error_info.payload) {
        .unexpected_token => //
        try writer.print("unexpected token: {f}\n", .{Parser.FatToken{
            .self = parser.tokenizer,
            .token = error_info.id,
        }}),
        .undeclared_identifier => //
        try writer.print("undeclared identifier: {s}\n", .{parser.tokenizer.slice(error_info.id)}),
        // .redeclaration => |rd| //
        // try writer.print("redeclaration: {s}\n", .{tokenizer.slice(rd)}),
        // // .unknown => try writer.print("unknown error\n", .{}),
        // // .dependency_loop => |dependency_loop| try writer.print("'{s}' depends on itself\n", .{dependency_loop}),
        // // .undeclared_identifier => |undeclared_identifier| try writer.print("undeclared identifier '{s}'\n", .{undeclared_identifier}),
        // // .float8 => try writer.print("floats cant have bit width of 8\n", .{}),
        else => |payload| try writer.print("error: {s}\n", .{@tagName(payload)}),
    }
    try writer.flush();
    return Error.SyntaxError;
}
pub fn errorOutTokenizer(tokenizer: Tokenizer, writer: *std.Io.Writer) Error {
    const error_info = tokenizer.error_info;
    switch (error_info.kind) {
        .invalid_character => try writer.print(
            "invalid source character: {c}({d})\n",
            .{
                tokenizer.full_source[error_info.source_offset],
                tokenizer.full_source[error_info.source_offset],
            },
        ),
        .float8 => try writer.print("floats cant have bit width of 8\n", .{}),
    }
    try writer.flush();
    return Error.SyntaxError;
}
const Error = root.Error;
const Token = Tokenizer.Token;
const FatToken = Tokenizer.FatToken;
const Node = Parser.Node;
