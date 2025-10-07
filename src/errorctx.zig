const std = @import("std");
const util = @import("util.zig");
const ErrorCtx = @This();
const Tokenizer = @import("Tokenizer.zig");

const error_msg_buf_len = 256;
const max_line_len = 128;

source: []const u8 = undefined,
offset: usize = 0,
file_path: []const u8 = undefined,

// error_code: [:0]const u8 = undefined,
error_code: anyerror = undefined,

error_msg_buf: [error_msg_buf_len]u8 = @splat(0),
error_msg_len: usize = 0,
writer: std.Io.Writer = undefined,

pub fn init(self: *ErrorCtx, source: []const u8, file_path: []const u8) void {
    self.source = source;
    self.file_path = file_path;
    self.writer = std.Io.Writer.fixed(&self.error_msg_buf);
}
pub fn printError(self: *ErrorCtx, offset: usize, err: anyerror, comptime fmt: []const u8, args: anytype) void {
    self.error_code = err;
    self.offset = offset;

    self.writer.end = 0;
    self.writer.print(fmt, args) catch return;
    self.error_msg_len = self.writer.end;
}

pub fn output(self: *ErrorCtx) !void {
    const source_ctx = self.getSourceContext();
    const err_msg = self.error_msg_buf[0..self.error_msg_len];
    //let em choose writer
    //, writer: *std.Io.Writer
    const green = "\x1b[32m";
    const red = "\x1b[31m";
    const gray = "\x1b[37m";

    const reset = "\x1b[0m";
    std.debug.print(
        "(ShaderCompilationError)" ++ gray ++ "[{s}:{d}:{d}]" ++ gray ++ " - " ++ red ++ "{s}" ++ reset ++ "{s}" ++ red ++ "{s}" ++ reset ++ "\n\t{s}\n\t{s}" ++ green ++ "^\n" ++ reset,
        .{
            self.file_path,
            source_ctx.line_num,
            source_ctx.char_num,
            @errorName(self.error_code),
            if (err_msg.len != 0) ", error message:\n" else "",
            err_msg,
            source_ctx.line,
            //arrow pointing at symbol
            @as([max_line_len]u8, @splat(' '))[0..@min(source_ctx.char_num -| 1, max_line_len)],
        },
    );
}
pub fn outputReturnErr(self: *ErrorCtx) anyerror {
    try self.output();
    return self.error_code;
}
fn getSourceContext(self: *ErrorCtx) SourceContext {
    var line_num: usize = 1;

    var line_start: usize = 0;
    var line_end: usize = 0;

    var skip_next = false;
    for (0..self.source.len) |i| {
        if (skip_next == true) continue;
        if (util.startingEndlLength(self.source[i..])) |l| {
            if (l > 1) skip_next = true;
            line_end = i;
            if (line_end > self.offset) break;
            line_start = i + l;

            line_num += 1;
        }
    } else line_end = self.source.len;

    return .{
        .line = self.source[line_start..line_end],
        .line_num = line_num,
        .char_num = self.offset + 1 - line_start,
    };
    // _ = self;
    // return .{};
}
const SourceContext = struct {
    line: []const u8 = "",
    line_num: usize = 0,
    char_num: usize = 0,
};
