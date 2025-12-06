const std = @import("std");
const hgsl = @import("root.zig");
const util = @import("util.zig");
const Parser = @import("Parser.zig");

const Error = hgsl.Error;
const ErrMsg = @This();

statement_offset: usize = 0,
token_offset: usize = 0,

writer: *std.Io.Writer,
source: []const u8 = "",

path: []const u8 = "",
pub fn offFromPtr(self: ErrMsg, ptr: [*]const u8) usize {
    return @intFromPtr(ptr) - @intFromPtr(self.source.ptr);
}
pub fn new(writer: *std.Io.Writer, source: []const u8, path: []const u8) ErrMsg {
    return .{
        .writer = writer,
        .path = path,
        .source = source,
    };
}
pub fn errorInvalidNumericLiteral(self: ErrMsg, offset: usize, is_float: bool) Error {
    try self.printPath();
    try self.writer.print("invalid {s} literal:\n", .{if (is_float) "float" else "integer"});

    try self.printLineAtOffset(offset);
    try self.writer.flush();
    return Error.SyntaxError;
}

pub fn errorInvalidCharacter(self: ErrMsg, offset: usize, char: u8) Error {
    try self.printPath();
    try self.writer.print("invalid character '{c}'\n", .{char});

    try self.printLineAtOffset(offset);
    try self.writer.flush();
    return Error.SyntaxError;
}

pub fn errorCode(self: ErrMsg, offset: usize, comptime err_code: ErrorCode) Error {
    try self.printPath();

    switch (err_code) {
        .unknown => try self.writer.write("compilation error:\n"),
    }
    try self.printLineAtOffset(offset);
    try self.writer.flush();
    return Error.CodeGenError;
}
const ErrorCode = enum {
    unknown,
};
fn printPath(self: ErrMsg) Error!void {
    if (self.path.len != 0)
        try self.writer.print("[{s}] ", .{self.path});
}
fn printLineAtOffset(self: ErrMsg, offset: usize) Error!void {
    const offset_info = self.getOffsetInfo(offset);
    if (offset_info.line_len == 0) return;

    const max_line_len = 64;
    const buffer_offset = self.writer.buffered().len;
    try self.writer.print("[{d}:{d}]: ", .{
        offset_info.line_number,
        offset_info.column_number,
    });

    const local = offset - offset_info.line_start + self.writer.buffered().len - buffer_offset;
    // std.debug.print("LOCAL: {d}, po: {d}\n", .{ local, buffer_offset });

    _ = try self.writer.write(self.source[offset_info.line_start..@min(offset_info.line_len, max_line_len)]);
    try self.writer.writeByte('\n');

    if (local < max_line_len) {
        for (0..local) |_|
            try self.writer.writeByte(' ');
        try self.writer.writeByte('^');
        try self.writer.writeByte('\n');
    }
}

fn getOffsetInfo(self: ErrMsg, offset: usize) OffsetInfo {
    var line_num: usize = 1;
    var line_start: usize = 0;

    var skip_next = false;
    const line_end: usize = for (0..self.source.len) |i| {
        if (skip_next == true) continue;
        if (util.startingEndlLength(self.source[i..])) |l| {
            if (l > 1) skip_next = true;
            if (i > offset) break i;
            line_start = i + l;

            line_num += 1;
        }
    } else self.source.len;

    return .{
        .line_start = line_start,
        .line_len = line_end - line_start,
        .line_number = line_num,
        .column_number = offset + 1 - line_start,
    };
}
const OffsetInfo = struct {
    line_number: usize,
    column_number: usize,

    line_start: usize,
    line_len: usize,
};
// var statement_offset: usize = 0;
// var token_offset: usize = 0;

//===| PROPOGATE |===
//     OutOfMemory,

//===| UNEXPECTED TOKEN(token: Token, expected: []Token)
//     UnexpectedToken,

//===| PLAIN MESSAGE(offset: usize, msg: []const u8)
//or
//===| MESSAGE(offset: usize, comptime err: ErrCode) ErrCode = enum{}
//     RecursionNotSupported,
//     UnclosedScope,
//     MissingInitializer,
//     RepeatingArgumentNames(function argument redeclaration)
//     NoTypeVariable,
//     MutatingImmutableVariable(MutatingImmutExpression),

//===| VARIABLE REDECLARATION(decl_offset: usize, redecl_offset: usize, name: []const u8)
//     VariableRedeclaration,

//===|VariableTypeAndQualifierDontMatch|
// case dependant
//     VariableTypeAndQualifierDontMatch,

//===|UNEXPECTED INITIALIZER(offset: usize, qualifier: Q)
// variables with 'qualifier' qualifier cant have initializers
//     UnexpectedInitializer,

//===|UnexpectedStatement|===
// cant have 'statement type' outside of function scope
//     UnexpectedStatement,

//===|UNDECLARED IDENTIFIER(offset: usize, name: []const u8)|===
//use of undeclared identifier
//     UndeclaredVariable,

//===|Type cant have members(offset: usize, type: Type)|===
//bad errors
//     IncompleteStatement,
//     IncompleteStructConstructor,
//     MissingFunctionBody,
//====================

//     NoMemberWithName,
//     InvalidCall,
//     InvalidBuiltin,
//     NonMatchingArgumentCount,
//     NonMatchingVariableInitializerCount,

//     CannotImplicitlyCast,
//     CannotExplicitlyCast,
//     CannotInferType,

//     InvalidOperands,
//     InvalidUnaryOperationTarget,
//     InvalidConstructor,
//     InvalidIndex,
//     InvalidIndexingTarget,
//     InvalidAssignmentTarget,
//     InvalidMemberAccess,
//     InvalidSwizzle,
//     NonVoidValueIgnored,

//     NumericError,
//     OutOfBoundsAccess,
//     NegativePower,
//     InvalidBitshift,
//     IntegerUnderFlow,
//     IntegerOverFlow,
//     SampleOnStorageTexture,
// } || Tokenizer.Error;
