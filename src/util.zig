const std = @import("std");

pub fn extract(T: type, from: anytype) T {
    const F = @TypeOf(from);
    if (@typeInfo(F) != .int or @typeInfo(F).int.signedness == .signed) @compileError("invalid from type - " ++ @typeName(F));
    const U = @Type(.{ .int = .{ .bits = @sizeOf(T) * 8, .signedness = .unsigned } });
    return @bitCast(uintCast(U, from));
}
pub fn fit(T: type, value: anytype) T {
    if (@typeInfo(T) != .int or @typeInfo(T).int.signedness == .signed) @compileError("invalid fit type - " ++ @typeName(T));
    const U = @Type(.{ .int = .{ .bits = @sizeOf(@TypeOf(value)) * 8, .signedness = .unsigned } });
    return uintCast(T, @as(U, @bitCast(value)));
}
inline fn uintCast(T: type, from: anytype) T {
    const F = @TypeOf(from);
    return if (@sizeOf(F) > @sizeOf(T)) @truncate(from) else @as(T, from);
}

pub fn strEql(a: []const u8, b: []const u8) bool {
    return if (a.len != b.len) false else std.mem.eql(u8, a, b);
}
pub fn startingEndlLength(slice: []const u8) ?usize {
    if (strStartsComp(slice, "\r\n")) return 2;
    return if (slice[0] == '\n') 1 else null;
    // if (char == '\n' or util.strStartsComp(self.state.source[count..], "\r\n")) {
}
pub inline fn strStarts(haystack: []const u8, needle: []const u8) bool {
    return startsWith(u8, haystack, needle);
}
pub const startsWith = std.mem.startsWith;
pub fn strStartsComp(haystack: []const u8, comptime needle: []const u8) bool {
    return if (haystack.len < needle.len) false else inline for (needle, haystack[0..needle.len]) |n, h| (if (n != h) break false) else true;
}
pub fn SortEnumDecending(Enum: type) type {
    return Enum;
}
pub fn matchToEnum(Enum: type, str: []const u8) ?Enum {
    return inline for (@typeInfo(Enum).@"enum".fields) |ef| {
        if (strStarts(str, ef.name)) break @enumFromInt(ef.value);
    } else null;
}
