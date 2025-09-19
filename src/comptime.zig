const std = @import("std");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");

const minusonecompint: Value = .{ .type = .compint, .payload = .{ .wide = @bitCast(@as(i128, -1)) } };
pub fn simplify(self: *Parser, expr: Expression) Error!Expression {
    return switch (expr) {
        .bin_op => |bin_op| try doBinOp(self, bin_op),
        else => expr,
    };
}

fn doBinOp(self: *Parser, bin_op: Parser.BinOp) Error!Expression {
    _ = self;
    const initial: Expression = .{ .bin_op = bin_op };

    const left = if (bin_op.left.* != .value) return initial else bin_op.left.value;
    const right = if (bin_op.right.* != .value) return initial else bin_op.right.value;
    return switch (bin_op.op) {
        .@"+" => .{ .value = try addValues(left, right) },
        .@"-" => .{ .value = try addValues(left, try mulValues(right, minusonecompint)) },
        else => initial,
    };
}

fn mulValues(left: Value, right: Value) Error!Value {
    const t, const a, const b = try implicitCastEqualizeValues(left, right, true);
    const payload: Parser.ValuePayload = switch (t) {
        .compint => .{ .wide = @bitCast(@as(i128, @bitCast(a.wide)) * @as(i128, @bitCast(b.wide))) },
        .compfloat => .{ .wide = @bitCast(@as(f128, @bitCast(a.wide)) * @as(f128, @bitCast(b.wide))) },
        //number , vector
        else => return Error.InvalidOperands,
    };
    return .{ .type = t, .payload = payload };
}
fn addValues(left: Value, right: Value) Error!Value {
    const t, const a, const b = try implicitCastEqualizeValues(left, right, false);

    const payload: Parser.ValuePayload = switch (t) {
        .compint => .{ .wide = @bitCast(@as(i128, @bitCast(a.wide)) + @as(i128, @bitCast(b.wide))) },
        .compfloat => .{ .wide = @bitCast(@as(f128, @bitCast(a.wide)) + @as(f128, @bitCast(b.wide))) },
        //number , vector
        else => return Error.InvalidOperands,
    };
    return .{ .type = t, .payload = payload };
}

fn implicitCastEqualizeValues(a: Value, b: Value, allow_splat: bool) Error!EqualizeResult {
    _ = allow_splat;
    if (std.meta.eql(a.type, b.type)) return .{ a.type, a.payload, b.payload };

    //'first' comes first in Type union
    var first, var second = blk: {
        const a_tag_value = @intFromEnum(std.meta.activeTag(a.type));
        const b_tag_value = @intFromEnum(std.meta.activeTag(b.type));
        break :blk if (a_tag_value >= b_tag_value) [2]Value{ b, a } else [2]Value{ a, b };
    };

    switch (second.type) {
        .compfloat, .number => first = try implicitCastValue(first, second.type, false),
        else => return Error.InvalidOperands,
    }
    return .{ second.type, first.payload, second.payload };
}

pub fn implicitCastValue(value: Value, target: Type, allow_splat: bool) Error!Value {
    _ = allow_splat;
    if (std.meta.eql(value.type, target)) return value;

    return switch (target) {
        .compfloat => if (value.type == .compint)
            .{ .type = .compfloat, .wide = @bitCast(@as(f128, @floatFromInt(@as(i128, @bitCast(value.payload.wide))))) }
        else
            Error.CannotImplicitlyCast,
        .number => |number| switch (number.type) {
            inline else => |t| switch (number.width) {
                inline else => |w| blk: {
                    const T = comptime (tp.Number{ .type = t, .width = w }).ToZig();
                    switch (value.type) {
                        .compint => break :blk numberCast(T, wideAs(i128, value.payload.wide)),
                        .compfloat => {},
                        else => {},
                    }
                    return Error.CannotImplicitlyCast;
                },
            },
        },
        else => Error.CannotImplicitlyCast,
    };
}

fn numberCast(T: type, value: anytype) T {
    const err = "invalid number cast";
    const F = @TypeOf(value);

    const from_tinfo = @typeInfo(F);
    const to_tinfo = @typeInfo(T);

    return switch (to_tinfo) {
        .float => if (from_tinfo == .float) @floatCast(value) else @floatFromInt(value),
        .int => if (from_tinfo == .int) @intCast(value) else @intFromFloat(value),
        else => @compileError(err),
    };
}
fn wideAs(T: type, wide: u128) T {
    const s = @sizeOf(u128);
    if (@sizeOf(T) == s) return @bitCast(wide);
    @compileError("SIZE WIDE AS");
    // {
    //     first.type = .compfloat;
    //     first.payload = .{
    //         .wide = @bitCast(@as(f128, @floatFromInt(@as(i128, @bitCast(first.payload))))),
    //     };
}
fn asWide(value: anytype) u128 {
    const T = @TypeOf(value);
}

const EqualizeResult = std.meta.Tuple(&.{ Type, Parser.ValuePayload, Parser.ValuePayload });

const Error = Parser.Error;
const Expression = Parser.Expression;
const Value = Parser.Value;
const Type = tp.Type;
