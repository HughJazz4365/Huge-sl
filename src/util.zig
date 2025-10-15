const std = @import("std");

pub fn numericCast(T: type, from: anytype) T {
    const F = @TypeOf(from);
    if (T == bool) return numericCast(u1, from) > 0;
    if (F == bool) return numericCast(T, @intFromBool(from));

    const from_tinfo = @typeInfo(F);
    const to_tinfo = @typeInfo(T);
    return //
    if (to_tinfo == .int) (if (from_tinfo == .int) @intCast(from) else if (from_tinfo == .float) @intFromFloat(from) else @compileError("Invalid types for numeric cast")) else //
    if (to_tinfo == .float) (if (from_tinfo == .float) @floatCast(from) else if (from_tinfo == .int) @floatFromInt(from) else @compileError("Invalid types for numeric cast")) else //
    @compileError("Invalid types for numeric cast");
}
pub fn extract(T: type, from: anytype) T {
    if (T == bool) return extract(u8, from) > 0;
    const F = @TypeOf(from);
    if (@typeInfo(F) != .int or @typeInfo(F).int.signedness == .signed) @compileError("invalid from type - " ++ @typeName(F));
    const U = @Type(.{ .int = .{ .bits = @sizeOf(T) * 8, .signedness = .unsigned } });
    return @bitCast(uintCast(U, from));
}
pub fn fit(T: type, value: anytype) T {
    if (@TypeOf(value) == bool) return fit(T, @as(u8, @intFromBool(value)));
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
    const Type = std.builtin.Type;
    const tinfo = @typeInfo(Enum).@"enum";
    var enum_fields: [tinfo.fields.len]Type.EnumField = undefined;
    @memcpy(&enum_fields, tinfo.fields);
    std.sort.insertion(Type.EnumField, &enum_fields, {}, struct {
        pub fn f(_: anytype, a: Type.EnumField, b: Type.EnumField) bool {
            return a.name.len > b.name.len;
        }
    }.f);
    return @Type(.{ .@"enum" = .{ .decls = &.{}, .fields = &enum_fields, .tag_type = tinfo.tag_type, .is_exhaustive = tinfo.is_exhaustive } });
}
pub fn matchToEnum(Enum: type, str: []const u8) ?Enum {
    return inline for (@typeInfo(Enum).@"enum".fields) |ef| {
        if (strStarts(str, ef.name)) break @enumFromInt(ef.value);
    } else null;
}
