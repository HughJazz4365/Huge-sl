const std = @import("std");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");

const minusonecompint: Value = .{ .type = .compint, .payload = .{ .wide = @bitCast(@as(i128, -1)) } };
pub fn refine(self: *Parser, expr: Expression) Error!Expression {
    return switch (expr) {
        .bin_op => |bin_op| try doBinOp(self, bin_op),
        .u_op => |u_op| try doUOp(self, u_op),
        .constructor => |constructor| try refineConstructor(self, constructor),
        else => expr,
    };
}
pub fn refineConstructor(self: *Parser, constructor: Parser.Constructor) Error!Expression {
    const initial: Expression = .{ .constructor = constructor };
    if (constructor.type == .unknown) return initial;

    const target_structure = constructor.type.constructorStructure();
    if (target_structure.len <= 1) return Error.InvalidConstructor;

    const slice = if (constructor.components.len == target_structure.len)
        constructor.components
    else
        try self.arena.allocator().alloc(Expression, target_structure.len);

    var filled_count: usize = 0;
    for (constructor.components) |source_component| {
        if (filled_count >= slice.len) return Error.InvalidConstructor;
        slice[filled_count] = self.implicitCast(source_component, target_structure.component) catch {
            // if (component_structure.len <= 1) return Error.CannotImplicitlyCast;
            var elem_iter = try ElementIterator.new(self, source_component);
            while (try elem_iter.next(self)) |elem| {
                if (filled_count >= slice.len) return Error.InvalidConstructor;
                slice[filled_count] = try self.implicitCast(elem, target_structure.component);
                filled_count += 1;
            }
            continue;
        };

        filled_count += 1;
        continue;
    }
    return try constructValue(self, .{ .type = constructor.type, .components = slice });
}

fn constructValue(self: *Parser, constructor: Parser.Constructor) Error!Expression {
    const initial: Expression = .{ .constructor = constructor };
    // if (constructor.value != .vector) return Error.InvalidConstructor;
    return switch (constructor.type) {
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.child.width) {
                inline else => |width| switch (vector.child.type) {
                    inline else => |num_type| blk: {
                        const @"type": Type = .{ .vector = .{ .len = len, .child = .{ .width = width, .type = num_type } } };
                        const T = @"type".ToZig();
                        const C = @typeInfo(T).vector.child;
                        var vector_value: T = undefined;
                        for (0..@intFromEnum(len)) |i| {
                            const component = constructor.components[i];
                            if (component != .value) return initial;
                            const elem = wideAs(C, component.value.payload.wide);
                            vector_value[i] = elem;
                        }
                        const ptr = try self.createVal(vector_value);
                        std.debug.print("{d}\n", .{vector_value});
                        break :blk .{ .value = .{ .type = @"type", .payload = .{ .ptr = @ptrCast(@alignCast(ptr)) } } };
                    },
                },
            },
        },

        else => initial,
    };
}
const ElementIterator = struct {
    expr: *Expression,
    index: usize = 0,

    pub fn next(self: *ElementIterator, parser: *Parser) Error!?Expression {
        const @"type" = parser.typeOf(self.expr.*);

        if (self.expr.* == .constructor and self.expr.constructor.type == .unknown) {
            if (self.index >= self.expr.constructor.components.len) return null;
            defer self.index += 1;
            return self.expr.constructor.components[self.index];
        } else {
            const structure = @"type".constructorStructure();
            if (self.index >= structure.len) return null;
            const ptr = try parser.createVal(Expression{ .value = .{
                .type = .compint,
                .payload = .{ .wide = asWide(self.index) },
            } });
            self.index += 1;
            return try refine(parser, .{ .indexing = .{ .target = self.expr, .index = ptr } });
        }
    }
    pub inline fn new(parser: *Parser, expr: Expression) Error!ElementIterator {
        const @"type" = parser.typeOf(expr);
        if (!(@"type" == .array or @"type" == .vector or (@"type" == .unknown and expr == .constructor))) return Error.CannotImplicitlyCast;
        return .{ .expr = try parser.createVal(expr) };
    }
};

fn doUOp(self: *Parser, u_op: Parser.UOp) Error!Expression {
    _ = self;
    if (u_op.op == .@"+") return u_op.target.*;
    const initial: Expression = .{ .u_op = u_op };

    const target = if (u_op.target.* != .value) return initial else u_op.target.value;
    return switch (u_op.op) {
        .@"-" => .{ .value = try mulValues(target, minusonecompint) },
        .@"+" => initial,
        // .@"-" => .{ .value = try addValues(left, try mulValues(right, minusonecompint)) },
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
        .@"*" => .{ .value = try mulValues(left, right) },
        .@"^" => .{ .value = try powValues(left, right) },
        // else => initial,
    };
}

fn powValues(left: Value, right: Value) Error!Value {
    const t, const a, const b = try implicitCastEqualizeValues(left, right, true);
    const payload: Parser.ValuePayload = switch (t) {
        .compint => .{ .wide = asWide(std.math.powi(i128, wideAs(i128, a.wide), wideAs(i128, b.wide)) catch return Error.NumericError) },
        .compfloat => .{ .wide = asWide(@as(f128, @floatCast(std.math.pow(
            f64,
            @floatCast(wideAs(f128, a.wide)),
            @floatCast(wideAs(f128, b.wide)),
        )))) },
        //number , vector
        else => return Error.InvalidOperands,
    };
    return .{ .type = t, .payload = payload };
}

fn mulValues(left: Value, right: Value) Error!Value {
    const t, const a, const b = try implicitCastEqualizeValues(left, right, true);
    const payload: Parser.ValuePayload = switch (t) {
        .compint => .{ .wide = asWide(wideAs(i128, a.wide) * wideAs(i128, b.wide)) },
        .compfloat => .{ .wide = asWide(wideAs(f128, a.wide) * wideAs(f128, b.wide)) },
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

pub fn implicitCast(self: *Parser, expr: Expression, @"type": Type) Error!Expression {
    const type_of = self.typeOf(expr);
    if (std.meta.eql(type_of, @"type")) return expr;
    return switch (expr) {
        .value => |value| .{ .value = try implicitCastValue(value, @"type", true) },
        .constructor => |constructor| try refineConstructor(self, if (constructor.type == .unknown)
            .{ .components = constructor.components, .type = @"type" }
        else
            return Error.CannotImplicitlyCast),
        // .constructor => |constructor| try self.implicitCastConstructor(constructor, @"type"),
        else => expr,
    };
}
fn implicitCastEqualizeValues(a: Value, b: Value, allow_splat: bool) Error!EqualizeResult {
    _ = allow_splat;
    if (std.meta.eql(a.type, b.type)) return .{ a.type, a.payload, b.payload };

    //'first' comes first in Type union
    var reordered = false;
    var first, const second = blk: {
        const a_tag_value = @intFromEnum(std.meta.activeTag(a.type));
        const b_tag_value = @intFromEnum(std.meta.activeTag(b.type));
        if (a_tag_value >= b_tag_value) reordered = true;
        break :blk if (a_tag_value >= b_tag_value) [2]Value{ b, a } else [2]Value{ a, b };
    };

    switch (second.type) {
        .compfloat, .number => first = try implicitCastValue(first, second.type, false),
        else => return Error.InvalidOperands,
    }

    return if (reordered)
        .{ second.type, second.payload, first.payload }
    else
        .{ second.type, first.payload, second.payload };
}

pub fn implicitCastValue(value: Value, target: Type, allow_splat: bool) Error!Value {
    _ = allow_splat;
    if (std.meta.eql(value.type, target)) return value;

    return switch (target) {
        .compfloat => if (value.type == .compint)
            .{ .type = .compfloat, .payload = .{ .wide = @bitCast(@as(f128, @floatFromInt(@as(i128, @bitCast(value.payload.wide))))) } }
        else
            Error.CannotImplicitlyCast,
        //@PREVENT ROUNDING COMPTIME FLOATS WHEN CASING TO INTEGER
        .number => |number| .{ .type = target, .payload = .{ .wide = try switch (number.type) {
            inline else => |t| switch (number.width) {
                inline else => |w| switch (value.type) {
                    .compint => asWide(numberCast(
                        comptime (tp.Number{ .type = t, .width = w }).ToZig(),
                        wideAs(i128, value.payload.wide),
                    )),
                    .compfloat => asWide(numberCast(
                        comptime (tp.Number{ .type = t, .width = w }).ToZig(),
                        wideAs(f128, value.payload.wide),
                    )),
                    else => Error.CannotImplicitlyCast,
                },
            },
        } } },
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
pub inline fn wideAs(T: type, wide: u128) T {
    const s = @sizeOf(u128);
    if (@sizeOf(T) == s) return @bitCast(wide);
    const ptr: *const T = @ptrCast(@alignCast(&wide));
    return ptr.*;
}
pub fn asWide(value: anytype) u128 {
    var wide: u128 = 0;
    const T = @TypeOf(value);
    @memcpy(
        @as([*]u8, @ptrCast(@alignCast(&wide))),
        @as([*]const u8, @ptrCast(@alignCast(&value)))[0..@sizeOf(T)],
    );
    return wide;
}

const EqualizeResult = std.meta.Tuple(&.{ Type, Parser.ValuePayload, Parser.ValuePayload });

const Error = Parser.Error;
const Expression = Parser.Expression;
const Value = Parser.Value;
const Type = tp.Type;
