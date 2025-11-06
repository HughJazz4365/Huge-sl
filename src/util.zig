const std = @import("std");
const Type = std.builtin.Type;
const Allocator = std.mem.Allocator;
pub fn wrut(a: u32, b: u32) u32 {
    return rut(u32, a, b);
}
pub fn rut(T: type, a: T, b: T) T {
    return (a + b - 1) / b * b;
}

pub fn removeAt(T: type, slice_ptr: *[]T, index: usize) void {
    @memmove(slice_ptr.*[index .. slice_ptr.len - 1], slice_ptr.*[index + 1 ..]);
    slice_ptr.len -= 1;
}
pub fn reallocPrependSlice(allocator: Allocator, T: type, slice: []T, elems: []const T) Allocator.Error![]T {
    const l, const el = .{ slice.len, elems.len };
    const new_slice = try allocator.alloc(T, l + el);
    if (l > 0) {
        @memcpy(new_slice[el..], slice);
        allocator.free(slice);
    }
    @memcpy(new_slice[0..el], elems);
    return new_slice;
}
pub fn reallocAdd(allocator: Allocator, T: type, slice: []T, elem: T) Allocator.Error![]T {
    const l = slice.len;
    const new_slice = try allocator.alloc(T, l + 1);
    if (l > 0) {
        @memcpy(new_slice[0..l], slice);
        allocator.free(slice);
    }
    new_slice[l] = elem;
    return new_slice;
}
pub fn pow(T: type, left: T, right: T) T {
    return if (T == f16)
        @floatCast(std.math.pow(f32, @floatCast(left), @floatCast(right)))
    else
        std.math.pow(T, left, right);
}
pub fn StructFromEnum(Enum: type, T: type, default_value: ?T) type {
    const em = @typeInfo(Enum).@"enum";
    var struct_fields: [em.fields.len]std.builtin.Type.StructField = undefined;
    inline for (em.fields, &struct_fields) |ef, *sf| {
        sf.* = .{
            .default_value_ptr = if (default_value) |d| &d else null,
            .alignment = @alignOf(T),
            .is_comptime = false,
            .name = ef.name,
            .type = T,
        };
    }
    return @Type(.{ .@"struct" = .{
        .decls = &.{},
        .fields = &struct_fields,
        .is_tuple = false,
        .layout = .auto,
    } });
}
pub fn FlagStructFromEnum(Enum: type, default_value: bool) type {
    const em = @typeInfo(Enum).@"enum";
    var struct_fields: [em.fields.len]std.builtin.Type.StructField = undefined;
    inline for (em.fields, &struct_fields) |ef, *sf| {
        sf.* = .{
            .default_value_ptr = &default_value,
            .alignment = 0,
            .is_comptime = false,
            .name = ef.name,
            .type = bool,
        };
    }
    return @Type(.{ .@"struct" = .{
        .decls = &.{},
        .fields = &struct_fields,
        .is_tuple = false,
        .layout = .@"packed",
    } });
}
pub fn FlagStructFromUnion(Union: type, comptime dv: bool) type {
    const union_fields = @typeInfo(Union).@"union".fields;
    var struct_fields: [union_fields.len]Type.StructField = undefined;
    for (&struct_fields, union_fields) |*sf, uf|
        sf.* = .{ .alignment = 0, .type = bool, .default_value_ptr = @ptrCast(&dv), .is_comptime = false, .name = uf.name };
    return @Type(.{ .@"struct" = .{ .decls = &.{}, .fields = &struct_fields, .layout = .@"packed", .is_tuple = false } });
}
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
