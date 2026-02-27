const std = @import("std");
const Type = std.builtin.Type;
const Allocator = std.mem.Allocator;
pub fn wrut(a: u32, b: u32) u32 {
    return rut(u32, a, b);
}
pub fn rut(T: type, a: T, b: T) T {
    return (a + b - 1) / b * b;
}

pub fn last(T: type, slice: []const T) T {
    return slice[slice.len -| 1];
}

pub fn bufAppend(
    T: type,
    list: *std.ArrayList(T),
    allocator: Allocator,
    buf: []T,
    item: T,
) !void {
    if (list.items.len == buf.len) {
        const capacity = buf.len + buf.len / 2;
        list.* = try .initCapacity(allocator, capacity);
        list.items.len = buf.len;
        @memcpy(list.items, buf);
    }
    try list.append(allocator, item);
}
pub fn removeAt(T: type, slice_ptr: *[]T, index: usize) void {
    @memmove(slice_ptr.*[index .. slice_ptr.len - 1], slice_ptr.*[index + 1 ..]);
    slice_ptr.len -= 1;
}
pub fn enumInRange(Enum: type, a: Enum, from: Enum, to: Enum) bool {
    return @intFromEnum(from) <= @intFromEnum(a) and
        @intFromEnum(a) <= @intFromEnum(to);
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
pub fn EnumSlice(
    Enum: type,
    I: type,
    comptime from: Enum,
    comptime to: Enum,
) type {
    const f = @intFromEnum(from);
    const t = @intFromEnum(to) + 1;
    const len = t - f;
    var enum_field_names: [len][]const u8 = undefined;
    var enum_field_values: [len]I = undefined;

    for (@typeInfo(Enum).@"enum".fields[f..t], 0..t - f) |ef, i| {
        enum_field_names[i] = ef.name;
        enum_field_values[i] = @intCast(i);
    }

    return @Enum(
        I,
        .exhaustive,
        &enum_field_names,
        &enum_field_values,
    );
}
pub fn StructFromEnum(Enum: type, T: type, default_value: ?T, layout: std.builtin.Type.ContainerLayout) type {
    const em = @typeInfo(Enum).@"enum";
    var struct_field_names: [em.fields.len][]const u8 = undefined;
    var struct_field_types: [em.fields.len]type = undefined;
    const sturct_field_attributes: [em.fields.len]std.builtin.Type.StructField.Attributes =
        @splat(.{
            .@"comptime" = false,
            .@"align" = if (layout == .@"packed") null else @alignOf(T),
            .default_value_ptr = if (default_value) |dv| &dv else null,
        });
    inline for (em.fields, 0..) |ef, i| {
        struct_field_names[i] = ef.name;
        struct_field_types[i] = T;
    }
    return @Struct(
        layout,
        null,
        &struct_field_names,
        &struct_field_types,
        &sturct_field_attributes,
    );
}
pub fn FlagStructFromEnum(Enum: type, default_value: bool) type {
    return StructFromEnum(Enum, bool, default_value, .@"packed");
}
pub fn FlagStructFromUnion(Union: type, comptime dv: bool) type {
    const union_fields = @typeInfo(Union).@"union".fields;

    var struct_field_names: [union_fields.len][]const u8 = undefined;
    var struct_field_types: [union_fields.len]type = undefined;
    var sturct_field_attributes: [union_fields.len]std.builtin.Type.StructField.Attributes =
        @splat(.{
            .@"comptime" = false,
            .default_value_ptr = @ptrCast(&dv),
        });
    inline for (union_fields, 0..) |uf, i| {
        struct_field_names[i] = uf.name;
        struct_field_types[i] = bool;
    }
    return @Struct(
        .@"packed",
        null,
        &struct_field_names,
        &struct_field_types,
        &sturct_field_attributes,
    );
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
    const U = @Int(.unsigned, @sizeOf(T) * 8);
    return @bitCast(uintCast(U, from));
}
pub fn fit(T: type, value: anytype) T {
    if (@TypeOf(value) == bool) return fit(T, @as(u8, @intFromBool(value)));
    if (@typeInfo(T) != .int or @typeInfo(T).int.signedness == .signed) @compileError("invalid fit type - " ++ @typeName(T));
    const U = @Int(.unsigned, @sizeOf(@TypeOf(value)) * 8);
    return uintCast(T, @as(U, @bitCast(value)));
}
inline fn uintCast(T: type, from: anytype) T {
    const F = @TypeOf(from);
    return if (@sizeOf(F) > @sizeOf(T)) @truncate(from) else @as(T, from);
}

pub fn strEql(a: []const u8, b: []const u8) bool {
    return if (a.len != b.len) false else std.mem.eql(u8, a, b);
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
    const I = std.math.IntFittingRange(0, enum_fields.len -| 1);
    var enum_field_names: [enum_fields.len][]const u8 = undefined;
    var enum_field_values: [enum_fields.len]I = undefined;
    for (&enum_field_names, &enum_field_values, &enum_fields) |*en, *ev, ef|
        en.*, ev.* = .{ ef.name, ef.value };

    return @Enum(
        I,
        .exhaustive,
        &enum_field_names,
        &enum_field_values,
    );
}
pub fn matchToEnum(Enum: type, str: []const u8) ?Enum {
    return inline for (@typeInfo(Enum).@"enum".fields) |ef| {
        if (strStarts(str, ef.name)) break @enumFromInt(ef.value);
    } else null;
}
