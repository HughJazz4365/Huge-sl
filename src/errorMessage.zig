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
    pub const unknown: ParserErrorInfo = .{ .payload = .unknown, .token = 0 };
    token: Token,
    payload: Payload,
    //plan the error message and information dependecies for
    //each error kind
    pub const Payload = union(enum) {
        unexpected_token,
        undeclared_identifier, //UseToken
        //undeclared identifer {id}:
        // <line thing>
        redeclaration: Token, //UseToken, DeclToken
        //redeclaration of identifier {token}
        // <usage line>
        // declared at line {line}:
        // <declaration line>
        dependency_loop, //DeclToken
        //dependency loop:
        //<line>

        return_type_mismatch,
        cant_implicitly_cast,
        not_a_type: Parser.Type, //AType, BType, Token
        //expected {a}, found {b}:
        //<line>
        //===OR===
        //cannot implicitly cast {a} to {b}

        missing_initializer: Parser.Qualifier, //Qualifier, DeclToken
        //{qualifier} variables must have intializer
        //<line>

        missing_function_body, //FNTOKEN
        //missing function body:
        //<line>
        missing_return_type, //FNTOKEN
        //missing function return type:
        //<line>

        missing_return_value: Token, //RETURNTOKEN, FNDECL
        //missing return value of type {TYPE}
        //<return line>
        //function declared at:
        //<decl line>

        qualifier_cant_have_initializer: Parser.Qualifier, //QUALIFIER, VARDECL
        //{qualifier} variables can`t have intializers
        //<line>

        unclosed_scope,
        unable_to_resolve_comptime_value, //TOKEN
        //unable_to_resolve_comptime_value:
        //<line>

        invalid_function_declaration, //TOKEN
        //unable_to_resolve_comptime_value:
        //<line>

        invalid_assignment_target, //TOKEN
        //invalid_assignment_target:
        //<line>

        type_cant_have_constructor: Parser.Type, //TOKEN, TYPE
        //value of type '{type}' cant be constructed
        //<line>

        qualifier_incompatible_with_type: QualifierIncompatibleWithType,
        //QUALIFIER, TYPE, TOKEN
        //{qualifier} variables cannot have type of {type}:
        //<line>

        return_outside_function, //TOKEN
        //return statement outside of function scope:
        //<line>

        unreachable_statement, //TOKEN
        //unreachable code: <line>

        non_void_ignore, //TOKEN
        //value of type '{type}' ignored:
        //<line>

        unknown,
    };
};
const QualifierIncompatibleWithType = struct {
    qualifier: Parser.Qualifier,
    type: Parser.Type,
};

pub fn errorOutParser(parser: *Parser, writer: *std.Io.Writer) Error {
    const error_info = parser.error_info;
    const loc = getSourceLocation(parser.tokenizer, parser.tokenizer.offset(error_info.token));
    switch (error_info.payload) {
        .unclosed_scope => {
            try writer.print("unclosed scope, declared at line {d}:\n", .{loc.line_number + 1});
            try loc.printLineToken(.pointer, parser.tokenizer, error_info.token, writer);
        },
        .unexpected_token => {
            try writer.print("unexpected token '{s}':\n", .{@tagName(parser.tokenizer.kind(error_info.token))});
            try loc.printLineToken(.pointer_underline, parser.tokenizer, error_info.token, writer);
        },

        else => |payload| try writer.print("error: {s}\n", .{@tagName(payload)}),
    }
    try writer.flush();
    return Error.CompilationError;
}
const Color = enum {
    green,
    red,
    default,
    pub fn escapeCode(self: Color) []const u8 {
        return switch (self) {
            .green => "\x1b[32m",
            .red => "\x1b[f31m",
            .default => "\x1b[39m",
        };
    }
};

const HighlightType = enum {
    none,
    pointer,
    pointer_underline,
    squiggly_undeline,
};

const SourceLocation = struct {
    line_number: usize = 0,
    line_start: usize = 0,
    line_end: usize = 0,
    line_offset: usize = 0,
    pub inline fn printLineToken(
        self: SourceLocation,
        highlight_type: HighlightType,
        tokenizer: Tokenizer,
        token: Token,
        writer: *std.Io.Writer,
    ) !void {
        try self.printHighlightedLine(
            highlight_type,
            tokenizer.full_source,
            tokenizer.slice(token).len,
            writer,
        );
    }
    pub fn printHighlightedLine(
        self: SourceLocation,
        highlight_type: HighlightType,
        full_source: []const u8,
        token_length: usize,
        writer: *std.Io.Writer,
    ) !void {
        var skip: usize = 0;
        while (self.line_start + skip < full_source.len and
            Tokenizer.isWhitespace(full_source[self.line_start + skip]))
            skip += 1;

        const gap = 2;
        const max_len = 100 - gap;
        const s = full_source[self.line_start + skip .. self.line_end];
        const clamped = s[0..@min(max_len, s.len)];

        inline for (0..gap) |_| try writer.writeByte(' ');
        try writer.writeByte('|');
        try writer.writeAll(clamped);
        try writer.writeByte('|');
        try writer.writeByte('\n');

        const highlight_offset = self.line_offset - skip + gap;
        if (highlight_offset >= clamped.len + gap) return;

        if (highlight_type != .none) {
            var splats: [1][]const u8 = .{" "};
            try writer.writeSplatAll(&splats, highlight_offset);
            // try writer.writeAll(Color.green.escapeCode());
            switch (highlight_type) {
                .pointer => try writer.writeByte('^'),
                .pointer_underline => {
                    var sp: [1][]const u8 = .{"^"};
                    try writer.writeSplatAll(&sp, token_length);
                },
                .squiggly_undeline => {
                    var sp: [1][]const u8 = .{"~"};
                    try writer.writeSplatAll(&sp, token_length);
                },
                else => unreachable,
            }
            // try writer.writeAll(Color.default.escapeCode());
            try writer.writeByte('\n');
        }
    }
};
pub fn getSourceLocation(tokenizer: Tokenizer, offset: usize) SourceLocation {
    var loc: SourceLocation = .{};
    var count: usize = 0;
    const source = tokenizer.full_source;
    while (count < offset)
        if (Tokenizer.startingEndlLength(source[count..])) |endl_length| {
            count += endl_length;
            loc.line_number += 1;
            loc.line_offset = 0;
            loc.line_start = count;
        } else {
            count += 1;
            loc.line_offset += 1;
        };
    var line_end = loc.line_start;
    while (true) {
        if (source.len <= line_end) break;
        if (Tokenizer.startingEndlLength(source[line_end..])) |_| {
            break;
        } else line_end += 1;
    }
    loc.line_end = line_end;

    return loc;
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
