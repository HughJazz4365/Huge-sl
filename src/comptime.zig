const std = @import("std");
const tp = @import("type.zig");
const Parser = @import("Parser.zig");

const minusonecompint: Value = .{ .type = .compint, .payload = .{ .wide = @bitCast(@as(i128, -1)) } };
pub fn refine(self: *Parser, expr: Expression) Error!Expression {
    return switch (expr) {
        .bin_op => |bin_op| try refineBinOp(self, bin_op),
        .u_op => |u_op| try refineUOp(self, u_op),
        .constructor => |constructor| try refineConstructor(self, constructor),
        .cast => |cast| try refineCast(self, cast),
        .indexing => |indexing| try refineIndexing(self, indexing),
        .identifier => |identifier| try refineIdentifier(self, identifier),
        else => expr,
    };
}
inline fn refineIdentifier(self: *Parser, identifier: []const u8) Error!Expression {
    const var_ref = try self.current_scope.getVariableReferece(identifier);
    return if (var_ref.isComptime()) var_ref.value.* else .{ .identifier = identifier };
}

fn refineCast(self: *Parser, cast: Parser.Cast) Error!Expression {
    const initial: Expression = .{ .cast = cast };
    if (cast.type == .unknown) return initial;

    return (self.implicitCast(cast.expr.*, cast.type)) catch {
        const type_of = try self.typeOf(cast.expr.*);
        if (!isExplicitlyCastable(type_of, cast.type)) return Error.CannotExplicitlyCast;

        if (cast.type == .vector and type_of.depth() == 0)
            return try splatCast(self, cast.expr, cast.type);
        return initial;
    };
}
fn isExplicitlyCastable(from: Type, to: Type) bool {
    return switch (to) {
        .number, .compint, .compfloat, .bool, .@"enum" => to == .number or to == .compint or to == .compfloat or to == .bool or to == .@"enum",
        .vector => |vector| switch (from) {
            .vector => |fromvec| fromvec.len == vector.len,
            .array => |array| array.len == @intFromEnum(vector.len),
            .number, .compint, .compfloat, .bool, .@"enum" => true,
            else => false,
        },
        .array => |array| from == .vector and array.len == @intFromEnum(from.vector.len),
        else => false,
    };
}
fn splatCast(self: *Parser, expr_ptr: *Expression, @"type": Type) Error!Expression {
    const structure = @"type".constructorStructure();
    const len = structure.len;
    const component = try refine(self, .{ .cast = .{
        .type = structure.component,
        .expr = expr_ptr,
    } });
    const slice = try self.arena.allocator().alloc(Expression, len);
    for (slice) |*c| c.* = component;
    return try refine(self, .{ .constructor = .{ .type = @"type", .components = slice } });
}
fn refineIndexing(self: *Parser, indexing: Parser.Indexing) Error!Expression {
    const initial: Expression = .{ .indexing = indexing };
    _ = self;
    // if ((indexing.index.* == .value and indexing.target.* == .bin_op) and
    //     (indexing.target.bin_op.left.* == .value or indexing.target.bin_op.right.* == .value))
    // {
    //     //if index is comptime and one of operads is comptime
    //     //(a * b)[i] => (a[i] * b[i])
    //     const create_left = try self.createVal(try refine(self, Expression{ .indexing = .{
    //         .index = indexing.index,
    //         .target = indexing.target.bin_op.left,
    //     } }));
    //     const create_right = try self.createVal(try refine(self, Expression{ .indexing = .{
    //         .index = indexing.index,
    //         .target = indexing.target.bin_op.right,
    //     } }));
    //     return try refine(self, .{ .bin_op = .{
    //         .left = create_left,
    //         .right = create_right,
    //         .op = indexing.target.bin_op.op,
    //     } });
    // }
    if (!(indexing.index.* == .value and indexing.target.* == .value)) return initial;

    const index_value = indexing.index.value;
    const target_value = indexing.target.value;
    const index: usize = switch (index_value.type) {
        .compint => @intCast(wideAs(i128, index_value.payload.wide)),
        .compfloat => castFloatNoRound(usize, wideAs(f128, index_value.payload.wide)) catch return Error.InvalidIndex,
        .number => |number| switch (number.width) {
            inline else => |width| @intCast(wideAs(
                (Type{ .number = .{ .width = width, .type = .uint } }).ToZig(),
                index_value.payload.wide,
            )),
        },
        else => return Error.InvalidIndex,
    };

    const structure = target_value.type.constructorStructure();
    if (structure.len <= index) return Error.OutOfBoundsAccess;

    const component_size = structure.component.size();
    var ptr: [*]const u8 = @ptrCast(target_value.payload.ptr);
    ptr += component_size * index;
    return .{ .value = .{
        .type = structure.component,
        .payload = switch (structure.component) {
            .number, .compint, .compfloat => blk: {
                var wide: u128 = 0;
                @memcpy(@as([*]u8, @ptrCast(&wide)), ptr[0..component_size]);
                break :blk .{ .wide = wide };
            },
            else => .{ .ptr = ptr },
        },
    } };
}

fn castFloatNoRound(T: type, float: anytype) error{NotAWholeNumber}!T {
    if (@floor(float) != float) return error.NotAWholeNumber;
    return @intFromFloat(float);
}
fn refineConstructor(self: *Parser, constructor: Parser.Constructor) Error!Expression {
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
            const splitted_component: Expression = if (source_component == .value) source_component else blk: {
                const name = try self.createIntermediateValueName();
                try self.addStatement(.{ .var_decl = .{
                    .qualifier = .@"const",
                    .name = name,
                    .type = try self.typeOf(source_component),
                    .value = source_component,
                } });
                break :blk .{ .identifier = name };
            };
            var elem_iter = try ElementIterator.new(self, splitted_component);
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
    if (filled_count != target_structure.len) return Error.InvalidConstructor;
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
        const @"type" = try parser.typeOf(self.expr.*);

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
        const @"type" = try parser.typeOf(expr);
        if (!(@"type" == .array or @"type" == .vector or (@"type" == .unknown and expr == .constructor))) return Error.CannotImplicitlyCast;
        return .{ .expr = try parser.createVal(expr) };
    }
};

fn refineUOp(self: *Parser, u_op: Parser.UOp) Error!Expression {
    if (u_op.op == .@"+") return u_op.target.*;
    const initial: Expression = .{ .u_op = u_op };

    const target = if (u_op.target.* != .value) return initial else u_op.target.value;
    return switch (u_op.op) {
        .@"-" => .{ .value = try mulValues(self, target, minusonecompint) },
        .@"+" => initial,
        // .@"-" => .{ .value = try addValues(left, try mulValues(right, minusonecompint)) },
    };
}

fn refineBinOp(self: *Parser, bin_op: Parser.BinOp) Error!Expression {
    const initial: Expression = .{ .bin_op = bin_op };

    const left_type = try self.typeOf(bin_op.left.*);
    const right_type = try self.typeOf(bin_op.right.*);

    if (left_type == .unknown and right_type != .unknown) {
        bin_op.left.* = self.implicitCast(bin_op.left.*, right_type) catch return initial;
    } else if (right_type == .unknown and left_type != .unknown)
        bin_op.right.* = self.implicitCast(bin_op.right.*, left_type) catch return initial;

    blk: {
        return doBinOpSplat(self, bin_op) catch break :blk;
    }

    const left = if (bin_op.left.* != .value) return initial else bin_op.left.value;
    const right = if (bin_op.right.* != .value) return initial else bin_op.right.value;
    return switch (bin_op.op) {
        // return switch (bin_op.op) {
        .@"+" => .{ .value = try addValues(left, right) },
        .@"-" => .{ .value = try addValues(left, try mulValues(self, right, minusonecompint)) },
        .@"*" => .{ .value = try mulValues(self, left, right) },
        .@"^" => .{ .value = try powValues(left, right) },
        // else => initial,
    };
}
fn doBinOpSplat(self: *Parser, bin_op: Parser.BinOp) Error!Expression {
    const initial: Expression = .{ .bin_op = bin_op };

    const left_type = try self.typeOf(bin_op.left.*);
    const right_type = try self.typeOf(bin_op.right.*);

    if (left_type == .vector and right_type.depth() == 0) {
        const left_child_type: Type = .{ .number = left_type.vector.child };
        const casted = self.implicitCast(bin_op.right.*, left_child_type) catch return initial;
        const copy_right = try self.createVal(casted);
        bin_op.right.* = try refine(self, .{ .cast = .{
            .type = left_type,
            .expr = copy_right,
        } });
        return try refine(self, initial);
    } else return Error.CannotImplicitlyCast;
}

fn powValues(left: Value, right: Value) Error!Value {
    const t, const a, const b = try implicitCastEqualizeValues(left, right);
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

fn mulValues(self: *Parser, left: Value, right: Value) Error!Value {
    const t, const a, const b = try implicitCastEqualizeValues(left, right);
    const payload: Parser.ValuePayload = switch (t) {
        .compint => .{ .wide = asWide(wideAs(i128, a.wide) * wideAs(i128, b.wide)) },
        .compfloat => .{ .wide = asWide(wideAs(f128, a.wide) * wideAs(f128, b.wide)) },
        //number , vector
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.child.type) {
                inline else => |num_type| switch (vector.child.width) {
                    inline else => |width| blk: {
                        const V = (tp.Vector{ .len = len, .child = .{ .type = num_type, .width = width } }).ToZig();
                        const a_ptr: *V = @ptrCast(@alignCast(@constCast(a.ptr)));
                        const b_ptr: *const V = @ptrCast(@alignCast(b.ptr));

                        break :blk .{ .ptr = @ptrCast(@alignCast(try self.createVal(a_ptr.* * b_ptr.*))) };
                    },
                },
            },
        },
        else => return Error.InvalidOperands,
    };
    return .{ .type = t, .payload = payload };
}
fn addValues(left: Value, right: Value) Error!Value {
    const t, const a, const b = try implicitCastEqualizeValues(left, right);

    const payload: Parser.ValuePayload = switch (t) {
        .compint => .{ .wide = @bitCast(@as(i128, @bitCast(a.wide)) + @as(i128, @bitCast(b.wide))) },
        .compfloat => .{ .wide = @bitCast(@as(f128, @bitCast(a.wide)) + @as(f128, @bitCast(b.wide))) },
        //number , vector
        else => return Error.InvalidOperands,
    };
    return .{ .type = t, .payload = payload };
}

pub fn implicitCast(self: *Parser, expr: Expression, @"type": Type) Error!Expression {
    const type_of = try self.typeOf(expr);
    if (std.meta.eql(type_of, @"type")) return expr;
    const result: Expression = switch (expr) {
        .value => |value| .{ .value = try implicitCastValue(value, @"type") },
        .constructor => |constructor| try refineConstructor(self, if (constructor.type == .unknown)
            .{ .components = constructor.components, .type = @"type" }
        else
            return Error.CannotImplicitlyCast),
        else => expr,
        .cast => |cast| try implicitCastCast(self, cast, @"type"),
    };
    return if (std.meta.eql(try self.typeOf(result), @"type")) result else Error.CannotImplicitlyCast;
}
fn implicitCastCast(self: *Parser, cast: Parser.Cast, @"type": Type) Error!Expression {
    const initial: Expression = .{ .cast = cast };
    if (cast.type != .unknown)
        return if (std.meta.eql(cast.type, @"type")) initial else Error.CannotImplicitlyCast;
    return try refine(self, Expression{ .cast = .{ .type = @"type", .expr = cast.expr } });
}

fn implicitCastEqualizeValues(a: Value, b: Value) Error!EqualizeResult {
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
        .compfloat, .number => first = try implicitCastValue(first, second.type),
        else => return Error.InvalidOperands,
    }

    return if (reordered)
        .{ second.type, second.payload, first.payload }
    else
        .{ second.type, first.payload, second.payload };
}

pub fn implicitCastValue(value: Value, target: Type) Error!Value {
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
