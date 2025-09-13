const std = @import("std");

pub fn strEql(a: []const u8, b: []const u8) bool {
    return if (a.len != b.len) false else std.mem.eql(u8, a, b);
}
pub fn strExtract(haystack: []const u8, needle: []const u8) bool {
    if (!strStarts(haystack, needle)) return false;
    if (haystack.len == needle.len) return true;
    return !isIdentifierChar(haystack[needle.len]);
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
pub fn isIdentifierChar(char: u8) bool {
    return switch (char) {
        'a'...'z',
        'A'...'Z',
        '_',
        '@',
        => true,
        else => false,
    };
}
pub fn matchToEnum(Enum: type, str: []const u8) ?Enum {
    return inline for (@typeInfo(Enum).@"enum".fields) |ef| {
        if (strStarts(str, ef.name)) break @enumFromInt(ef.value);
    } else null;
}
