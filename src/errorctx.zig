const std = @import("std");
const util = @import("util.zig");
const ErrorCtx = @This();
const Tokenizer = @import("Tokenizer.zig");

const error_msg_buf_len = 256;
const max_line_len = 128;

const green = "\x1b[32m";
const red = "\x1b[31m";
const gray = "\x1b[37m";

const reset = "\x1b[0m";

source: []const u8 = "",
offset: usize = 0,
file_path: []const u8 = "",

// error_code: [:0]const u8 = undefined,
error_code: anyerror = error.UnhandledError,

error_msg_buf: [error_msg_buf_len]u8 = @splat(0),
error_msg_len: usize = 0,
error_msg_writer: std.Io.Writer = undefined,
out_writer: ?*std.Io.Writer = undefined,

pub fn reinit(self: *ErrorCtx, source: []const u8, file_path: []const u8) void {
    self.source = source;
    self.file_path = file_path;
    self.error_msg_writer = std.Io.Writer.fixed(&self.error_msg_buf);
}
pub fn init(self: *ErrorCtx, source: []const u8, file_path: []const u8, out_writer: ?*std.Io.Writer) void {
    self.source = source;
    self.file_path = file_path;
    self.error_msg_writer = std.Io.Writer.fixed(&self.error_msg_buf);
    self.out_writer = out_writer;
}
pub fn printError(self: *ErrorCtx, offset: usize, err: anyerror, comptime fmt: []const u8, args: anytype) void {
    self.error_code = err;
    self.offset = offset;

    self.error_msg_writer.end = 0;
    self.error_msg_writer.print(fmt, args) catch return;
    self.error_msg_len = self.error_msg_writer.end;
}

pub fn output(self: *ErrorCtx) !void {
    const out_writer = self.out_writer orelse return;
    const prefix = "(Huge-sl compilation error)";
    if (self.error_code == error.OutOfMemory) { //special case
        try out_writer.print(prefix ++ red ++ "Process ran out of memory\n" ++ reset, .{});
        try out_writer.flush();
        return;
    }

    const source_ctx = self.getSourceContext();
    const err_msg = self.error_msg_buf[0..self.error_msg_len];

    try out_writer.print(
        prefix ++ gray ++ "[{s}:{d}:{d}]" ++ gray ++ " - " ++ red ++ "{s}:{s}" ++ reset ++ "{s}\n\t{s}\n\t{s}" ++ green ++ "^\n" ++ reset,
        .{
            self.file_path,
            source_ctx.line_num,
            source_ctx.char_num,
            @errorName(self.error_code),
            if (err_msg.len != 0) "\nerror message: " else "",
            err_msg,
            source_ctx.line,
            //arrow pointing at symbol
            @as([max_line_len]u8, @splat(' '))[0..@min(source_ctx.char_num -| 1, max_line_len)],
        },
    );
    try out_writer.flush();
}

pub fn outputUpdateIfEmpty(self: *ErrorCtx, err: anyerror) anyerror {
    if (self.error_code == error.UnhandledError) self.printError(self.offset, err, "", .{});
    try self.output();
    return self.error_code;
}
fn getSourceContext(self: *ErrorCtx) SourceContext {
    var line_num: usize = 1;

    var line_start: usize = 0;

    var skip_next = false;
    const line_end: usize = for (0..self.source.len) |i| {
        if (skip_next == true) continue;
        if (util.startingEndlLength(self.source[i..])) |l| {
            if (l > 1) skip_next = true;
            if (i > self.offset) break i;
            line_start = i + l;

            line_num += 1;
        }
    } else self.source.len;

    return .{
        .line = self.source[line_start..line_end],
        .line_num = line_num,
        .char_num = self.offset + 1 - line_start,
    };
}
const SourceContext = struct {
    line: []const u8 = "",
    line_num: usize = 0,
    char_num: usize = 0,
};
