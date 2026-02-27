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
    const Kind = enum { unexpected_character, float8 };
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
        redeclaration: Redeclaration,
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
const Redeclaration = struct {
    statement: Token,
    name: Token,
};
const QualifierIncompatibleWithType = struct {
    qualifier: Parser.Qualifier,
    type: Parser.Type,
};

pub fn errorOutParser(parser: *Parser, writer: *std.Io.Writer) Error {
    const error_info = parser.error_info;
    const loc: TokenSourceLocation = .get(parser.tokenizer, error_info.token);
    try loc.printWithPath(parser.tokenizer, writer);
    try writer.writeAll(comptime Color.red.ec() ++ " error: " ++ Color.default.ec());

    switch (error_info.payload) {
        .unclosed_scope => {
            try writer.print("unclosed scope:\n", .{});
            try loc.printLineToken(.pointer, parser.tokenizer, writer);
        },
        .unexpected_token => {
            try writer.print("unexpected token '{s}':\n", .{@tagName(parser.tokenizer.kind(error_info.token))});
            try loc.printLineToken(.pointer_underline, parser.tokenizer, writer);
        },
        .redeclaration => |redeclaration| {
            try writer.print("redeclaration of '{s}':\n", .{parser.tokenizer.slice(redeclaration.name)});
            try loc.printLineToken(.pointer_underline, parser.tokenizer, writer);

            const other_loc: TokenSourceLocation = .get(parser.tokenizer, redeclaration.statement);
            try other_loc.printWithPath(parser.tokenizer, writer);

            try writer.print(" originally declared:\n", .{});
            try other_loc.printLineToken(.pointer_underline, parser.tokenizer, writer);
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
    pub fn ec(self: Color) []const u8 {
        return switch (self) {
            .green => "\x1b[32m",
            .red => "\x1b[31m",
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

const TokenSourceLocation = struct {
    line_number: usize = 0,
    line_start: usize = 0,
    line_end: usize = 0,
    line_offset: usize = 0,
    token_length: usize = 0,

    pub fn printWithPath(self: TokenSourceLocation, tokenizer: Tokenizer, writer: *std.Io.Writer) !void {
        try writer.print("[{s}:{d}:{d}]", .{
            tokenizer.path,
            self.line_number + 1,
            self.line_offset + 1,
        });
    }
    pub inline fn printLineToken(
        self: TokenSourceLocation,
        highlight_type: HighlightType,
        tokenizer: Tokenizer,
        writer: *std.Io.Writer,
    ) !void {
        var skip: usize = 0;
        while (self.line_start + skip < tokenizer.full_source.len and
            Tokenizer.isWhitespace(tokenizer.full_source[self.line_start + skip]))
            skip += 1;

        const gap = 2;
        const max_len = 85 - gap;
        const s = tokenizer.full_source[self.line_start + skip .. self.line_end];
        const clamped = s[0..@min(max_len, s.len)];

        inline for (0..gap) |_| try writer.writeByte(' ');
        try writer.writeAll(clamped);
        try writer.writeByte('\n');

        const highlight_offset = self.line_offset - skip + gap;
        if (highlight_offset >= clamped.len + gap) return;

        if (highlight_type != .none) {
            var splats: [1][]const u8 = .{" "};
            try writer.writeSplatAll(&splats, highlight_offset);
            try writer.writeAll(Color.green.ec());
            switch (highlight_type) {
                .pointer => try writer.writeByte('^'),
                .pointer_underline => {
                    var sp: [1][]const u8 = .{"^"};
                    try writer.writeSplatAll(&sp, self.token_length);
                },
                .squiggly_undeline => {
                    var sp: [1][]const u8 = .{"~"};
                    try writer.writeSplatAll(&sp, self.token_length);
                },
                else => unreachable,
            }
            try writer.writeAll(Color.default.ec());
            try writer.writeByte('\n');
        }
    }
    pub fn get(tokenizer: Tokenizer, token: Token) TokenSourceLocation {
        const entry = tokenizer.getEntry(token);
        return getRaw(tokenizer.full_source, entry.offset, entry.len);
    }
    pub fn getRaw(source: []const u8, offset: usize, len: usize) TokenSourceLocation {
        var loc: TokenSourceLocation = .{ .token_length = len };
        var count: usize = 0;

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
};
pub fn errorOutTokenizer(tokenizer: Tokenizer, writer: *std.Io.Writer) Error {
    const error_info = tokenizer.error_info;
    const loc: TokenSourceLocation = .getRaw(
        tokenizer.full_source,
        tokenizer.error_info.source_offset,
        if (tokenizer.error_info.kind == .float8) 2 else 1,
    );
    try loc.printWithPath(tokenizer, writer);
    try writer.writeAll(comptime Color.red.ec() ++ " error: " ++ Color.default.ec());

    switch (error_info.kind) {
        .unexpected_character => try writer.print(
            "unexpected source character: {c}({d})\n",
            .{
                tokenizer.full_source[error_info.source_offset],
                tokenizer.full_source[error_info.source_offset],
            },
        ),
        .float8 => try writer.print("floats cant have bit width of 8:\n", .{}),
    }
    try loc.printLineToken(.pointer_underline, tokenizer, writer);
    try writer.flush();
    return Error.SyntaxError;
}
const Error = root.Error;
const Token = Tokenizer.Token;
const FatToken = Tokenizer.FatToken;
