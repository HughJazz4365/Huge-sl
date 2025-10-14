const std = @import("std");
const tp = @import("type.zig");
const bi = @import("builtin.zig");
const util = @import("util.zig");
const Parser = @import("Parser.zig");

const minusonecompint: Value = .{ .type = .compint, .payload = .{ .wide = @bitCast(@as(CI, -1)) } };

fn errorImplicitCast(self: *Parser, expr: Expression, @"type": Type) Error {
    return self.errorOutFmt(
        Error.CannotImplicitlyCast,
        "Cannot implicitly cast \'{f}\' to \'{f}\'",
        .{ expr, @"type" },
    );
}
pub fn refineTopLevel(self: *Parser, expr: Expression) Error!Expression {
    const result = try refine(self, expr);
    //can be triggered multiple times from one expr = bad
    if (expr == .identifier and result == .identifier and util.strEql(expr.identifier, result.identifier)) {
        // std.debug.print("exprref: {f}\n", .{result});
        try self.current_scope.trackReference(result.identifier, .track);
    }
    return result;
}
//remove that
pub fn refineDescend(self: *Parser, expr: Expression) Error!Expression {
    return switch (expr) {
        .bin_op => |bin_op| blk: {
            bin_op.left.* = try refineDescend(self, bin_op.left.*);
            bin_op.right.* = try refineDescend(self, bin_op.right.*);
            break :blk try refine(self, expr);
        },
        .call => |call| blk: {
            call.callee.* = try refineDescend(self, call.callee.*);
            break :blk try refine(self, expr);
        },
        .u_op => |u_op| blk: {
            u_op.target.* = try refineDescend(self, u_op.target.*);
            break :blk try refine(self, expr);
        },
        .member_access => |member_access| blk: {
            member_access.target.* = try refineDescend(self, member_access.target.*);
            break :blk try refine(self, expr);
        },
        .indexing => |indexing| blk: {
            indexing.target.* = try refineDescend(self, indexing.target.*);
            break :blk try refine(self, expr);
        },
        .cast => |cast| blk: {
            cast.expr.* = try refineDescend(self, cast.expr.*);
            break :blk try refine(self, expr);
        },
        .constructor => |constructor| blk: {
            for (constructor.components) |*c| c.* = try refineDescend(self, c.*);
            break :blk try refine(self, expr);
        },
        else => refine(self, expr),
    };
}
pub fn refine(self: *Parser, expr: Expression) Error!Expression {
    // std.debug.print("R: {f}\n", .{expr});
    return switch (expr) {
        .bin_op => |bin_op| try refineBinOp(self, bin_op),
        .u_op => |u_op| try refineUOp(self, u_op),
        .constructor => |constructor| try refineConstructor(self, constructor),
        .cast => |cast| try refineCast(self, cast),
        .indexing => |indexing| try refineIndexing(self, indexing),
        .identifier => |identifier| try refineIdentifier(self, identifier),
        .call => |call| try refineCall(self, call),
        else => expr,
    };
}
fn refineCall(self: *Parser, call: Parser.Call) Error!Expression {
    const initial: Expression = .{ .call = call };
    if (call.callee.* == .builtin and call.callee.builtin == .function) return try bi.refineBuiltinCall(self, call.callee.builtin.function, call.args, call.callee);

    return initial;
}

fn refineIdentifier(self: *Parser, identifier: []const u8) Error!Expression {
    const var_ref = try self.current_scope.getVariableReference(self, identifier);
    return var_ref.value;
}

fn refineCast(self: *Parser, cast: Parser.Cast) Error!Expression {
    const initial: Expression = .{ .cast = cast };
    if (cast.type == .unknown) return initial;

    return (self.implicitCast(cast.expr.*, cast.type)) catch {
        const type_of = try self.typeOf(cast.expr.*);
        if (!isExplicitlyCastable(type_of, cast.type)) return Error.CannotExplicitlyCast;

        switch (cast.type) {
            .vector, .array, .matrix => {
                if (isExplicitlyCastable(type_of.constructorStructure().component, cast.type.constructorStructure().component))
                    return try splatCast(self, cast.expr, cast.type);
            },
            else => {},
        }
        return if (cast.type != .unknown and cast.expr.* == .value) .{ .value = try castValue(cast.expr.value, cast.type) } else initial;
    };
}
fn castValue(value: Value, to: Type) Error!Value {
    return switch (to) {
        .number => |number| switch (number.type) {
            inline else => |nt| switch (number.width) {
                inline else => |width| blk: {
                    const totype: Type = .{ .number = .{ .type = nt, .width = width } };
                    const T = totype.ToZig();
                    break :blk switch (value.type) {
                        .bool => .{ .type = totype, .payload = .{ .wide = util.fit(WIDE, util.numericCast(T, util.extract(bool, value.payload.wide))) } },
                        .number => |from_number| switch (from_number.type) {
                            inline else => |from_nt| switch (from_number.width) {
                                inline else => |from_width| .{ .type = totype, .payload = .{ .wide = util.fit(WIDE, util.numericCast(T, util.extract(
                                    (Type{ .number = .{ .type = from_nt, .width = from_width } }).ToZig(),
                                    value.payload.wide,
                                ))) } },
                            },
                        },
                        else => return Error.CannotExplicitlyCast,
                    };
                },
            },
        },
        else => return Error.CannotExplicitlyCast,
    };
}
fn isExplicitlyCastable(from: Type, to: Type) bool {
    return switch (to) {
        .number, .compint, .compfloat, .bool, .@"enum" => to.isScalar() or to == .bool or to == .@"enum",
        .vector => |vector| switch (from) {
            .vector => |fromvec| fromvec.len == vector.len,
            .array => |array| array.len == @intFromEnum(vector.len),
            .number, .compint, .compfloat, .bool, .@"enum" => true,
            else => false,
        },
        .array => |array| from.constructorStructure().len == @intFromEnum(from.vector.len) and
            isExplicitlyCastable(from.constructorStructure().component, array.component.*),
        else => false,
    };
}
fn splatCast(self: *Parser, expr_ptr: *Expression, @"type": Type) Error!Expression {
    const structure = @"type".constructorStructure();
    const len = structure.len;
    const component = try self.turnIntoIntermediateVariableIfNeeded(try refine(self, .{ .cast = .{
        .type = structure.component,
        .expr = expr_ptr,
    } }));

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
        .compint => @intCast(util.extract(CI, index_value.payload.wide)),
        .compfloat => @intCast(util.extract(CI, (try implicitCastValue(indexing.index.value, .compint)).payload.wide)),
        .number => |number| switch (number.width) {
            inline else => |width| @intCast(util.extract(
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
                var wide: WIDE = 0;
                @memcpy(@as([*]u8, @ptrCast(&wide)), ptr[0..component_size]);
                break :blk .{ .wide = wide };
            },
            else => .{ .ptr = ptr },
        },
    } };
}

fn refineConstructor(self: *Parser, constructor: Parser.Constructor) Error!Expression {
    const initial: Expression = .{ .constructor = constructor };
    const @"type": Type = constructor.type;
    if (@"type" == .unknown) return initial;

    const target_structure = @"type".constructorStructure();
    if (target_structure.len <= 1) return self.errorOut(Error.InvalidConstructor);

    const slice = if (constructor.components.len == target_structure.len)
        constructor.components
    else
        try self.arena.allocator().alloc(Expression, target_structure.len);

    var filled_count: usize = 0;
    for (constructor.components) |source_component| {
        if (filled_count >= slice.len) {
            filled_count += 1;
            continue;
        }
        slice[filled_count] = self.implicitCast(source_component, target_structure.component) catch {
            // if (component_structure.len <= 1) return Error.CannotImplicitlyCast;
            const splitted_component = try self.turnIntoIntermediateVariableIfNeeded(source_component);
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
    if (filled_count != target_structure.len) return self.errorOutFmt(
        Error.InvalidConstructor,
        "Constructor component count doest match that of a target type({f}). given: {d}, expected: {d}",
        .{ @"type", filled_count, target_structure.len },
    );
    return try constructValue(self, .{ .type = constructor.type, .components = slice });
}

fn constructValue(self: *Parser, constructor: Parser.Constructor) Error!Expression {
    const initial: Expression = .{ .constructor = constructor };
    // if (constructor.value != .vector) return Error.InvalidConstructor;
    if (constructor.type == .unknown) return initial;
    return switch (constructor.type) {
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.component.width) {
                inline else => |width| switch (vector.component.type) {
                    inline else => |num_type| blk: {
                        const comptype: Type = .{ .vector = .{ .len = len, .component = .{ .width = width, .type = num_type } } };
                        const T = comptype.ToZig();
                        const C = @typeInfo(T).vector.child;
                        var vector_value: T = undefined;
                        for (0..@intFromEnum(len)) |i| {
                            const component = constructor.components[i];
                            if (component != .value) return initial;
                            const elem = util.extract(C, component.value.payload.wide);
                            vector_value[i] = elem;
                        }
                        const ptr = try self.createVal(vector_value);
                        break :blk .{ .value = .{ .type = comptype, .payload = .{ .ptr = @ptrCast(@alignCast(ptr)) } } };
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
                .payload = .{ .wide = util.fit(WIDE, self.index) },
            } });
            self.index += 1;
            return try refine(parser, .{ .indexing = .{ .target = self.expr, .index = ptr } });
        }
    }
    pub fn new(parser: *Parser, expr: Expression) Error!ElementIterator {
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
        .@"-" => .{ .value = try mulVecOrScalarValues(self, target, minusonecompint) },
        .@"+" => u_op.target.*,
        .@"|" => blk: {
            const @"type" = try self.typeOf(u_op.target.*);
            if (@"type" != .vector or @"type".vector.component.type != .float)
                return self.errorOutFmt(Error.InvalidUnaryOperationTarget, "Only floating point vectors can be normalized", .{});
            if (u_op.target.* != .value) break :blk initial;
            normalizeValue(u_op.target.value);
            break :blk u_op.target.*;
        },
        // else => initial,
        // .@"-" => .{ .value = try addValues(left, try mulValues(right, minusonecompint)) },
    };
}
fn normalizeValue(value: Parser.Value) void {
    switch (value.type.vector.len) {
        inline else => |len| switch (value.type.vector.component.width) {
            inline else => |width| {
                const comptype: Type = .{ .vector = .{ .len = len, .component = .{ .type = .float, .width = width } } };
                const T = comptype.ToZig();
                const vec_ptr: *T = @ptrCast(@alignCast(@constCast(value.payload.ptr)));
                const val = vec_ptr.*;
                vec_ptr.* *= @as(T, @splat(1 / @sqrt(@reduce(.Add, val * val))));
            },
        },
    }
}

fn refineBinOp(self: *Parser, bin_op: Parser.BinOp) Error!Expression {
    return switch (bin_op.op) {
        .@"*" => try refineMul(self, bin_op.left, bin_op.right),
        .@"+" => try refineAdd(self, bin_op.left, bin_op.right),
        .@">>" => try refineRightShift(self, bin_op.left, bin_op.right),
        .@"&" => try refineAnd(self, bin_op.left, bin_op.right),
        else => .{ .bin_op = bin_op },
        // .@"-" => .{ .value = try addValues(left, try mulValues(self, right, minusonecompint)) },
        // .@"^" => .{ .value = try powValues(left, right) },
        // .@"'", .@"\"" => .{ .value = try dotValues(left, right) },
        // else => initial,
    };
}
//expand into or/and/xor
fn refineAnd(self: *Parser, left: *Expression, right: *Expression) Error!Expression {
    const initial: Expression = .{ .bin_op = .{ .left = left, .right = right, .op = .@"&" } };
    var left_expr, var left_type, var right_expr, var right_type = .{ left.*, try self.typeOf(left.*), right.*, try self.typeOf(right.*) };

    if (!equalizeExprTypesIfUnknown(self, &left_expr, &left_type, &right_expr, &right_type)) return initial;
    if (right_type == .vector or left_type == .vector) {
        if (!left_type.eql(right_type) or left_type.vector.component.type == .float) return self.errorOut(Error.InvalidOperands);
    } else if (left_type.isScalar() and right_type.isScalar()) {
        try equalizeScalarTypes(self, &left_expr, &left_type, &right_expr, &right_type);
        if (left_type == .number and left_type.number.type == .float) return self.errorOut(Error.InvalidOperands);
        if (left_type == .compfloat) {
            left_expr = try self.implicitCast(left_expr, .compint);
            right_expr = try self.implicitCast(right_expr, .compint);
        }
    } else if (left_type != .bool and right_type != .bool) return self.errorOut(Error.InvalidOperands);
    if (left_expr == .value and right_expr == .value) return .{ .value = try andValues(left_expr.value, right_expr.value) };
    left.*, right.* = .{ left_expr, right_expr };
    return initial;
}
fn andValues(left: Value, right: Value) Error!Value {
    return switch (left.type) {
        .bool => .{ .type = .bool, .payload = .{
            .wide = util.fit(WIDE, util.extract(bool, left.payload.wide) and util.extract(bool, right.payload.wide)),
        } },
        .compint => .{ .type = .compint, .payload = .{
            .wide = util.fit(WIDE, util.extract(CI, left.payload.wide) & util.extract(CI, right.payload.wide)),
        } },
        .number, .vector => switch (if (left.type == .vector) left.type.vector.component.type else left.type.number.type) {
            inline else => |nt| switch (if (left.type == .vector) left.type.vector.component.width else left.type.number.width) {
                inline else => |width| blk: {
                    if (nt == .float) unreachable;
                    if (left.type == .number) {
                        const comptype = Type{ .number = .{ .width = width, .type = nt } };
                        const T = comptype.ToZig();
                        break :blk .{ .type = comptype, .payload = .{
                            .wide = util.fit(WIDE, util.extract(T, left.payload.wide) & util.extract(T, right.payload.wide)),
                        } };
                    }
                    inline for (@typeInfo(tp.VectorLen).@"enum".fields) |ef| {
                        if (@as(tp.VectorLen, @enumFromInt(ef.value)) == left.type.vector.len) {
                            const comptype = Type{ .vector = .{ .len = @enumFromInt(ef.value), .component = .{ .width = width, .type = nt } } };
                            const T = comptype.ToZig();
                            const left_ptr: *T = @ptrCast(@alignCast(@constCast(left.payload.ptr)));
                            const right_ptr: *T = @ptrCast(@alignCast(@constCast(right.payload.ptr)));
                            left_ptr.* = left_ptr.* & right_ptr.*;
                            break :blk left;
                        }
                    }
                    unreachable;
                },
            },
        },
        else => unreachable,
    };
}
//add left shift// allow left argument to be signed
fn refineRightShift(self: *Parser, left: *Expression, right: *Expression) Error!Expression {
    const initial: Expression = .{ .bin_op = .{ .left = left, .right = right, .op = .@">>" } };
    var left_expr, var left_type, var right_expr, var right_type = .{ left.*, try self.typeOf(left.*), right.*, try self.typeOf(right.*) };

    if (!equalizeExprTypesIfUnknown(self, &left_expr, &left_type, &right_expr, &right_type)) return initial;

    if (left_type == .vector or right_type == .vector) {
        if (!left_type.eql(right_type) or left_type.vector.component.type != .uint) return self.errorOut(Error.InvalidOperands);
    } else {
        //rewrite that scope
        const u32_type: Type = .{ .number = .{ .type = .uint, .width = .word } };
        if (left_type == .compfloat or left_type == .compint) {
            left_expr = .{ .value = try implicitCastValue(left_expr.value, u32_type) };
            left_type = u32_type;
        } else if (left_type != .number or left_type.number.type == .uint)
            return self.errorOut(Error.InvalidOperands);

        if (right_type == .compfloat or right_type == .compint) {
            right_expr = .{ .value = try implicitCastValue(right_expr.value, u32_type) };
            right_type = u32_type;
        } else if (right_type != .number or right_type.number.type != .uint)
            return self.errorOut(Error.InvalidOperands);
    }
    if (left_expr == .value and right_expr == .value)
        return .{ .value = try rightShiftValues(self, left_expr.value, right_expr.value) };
    left.*, right.* = .{ left_expr, right_expr };
    return initial;
}
fn rightShiftValues(self: *Parser, left: Value, right: Value) Error!Value {
    return switch (left.type) {
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.component.width) {
                inline else => |width| blk: {
                    const comptype: Type = .{ .vector = .{ .len = len, .component = .{ .type = .uint, .width = width } } };
                    const T = comptype.ToZig();
                    const left_ptr: *T = @ptrCast(@alignCast(@constCast(left.payload.ptr)));
                    const right_ptr: *T = @ptrCast(@alignCast(@constCast(right.payload.ptr)));
                    if (@reduce(.Or, right_ptr.* > @as(T, @splat(@intFromEnum(width))))) return self.errorOut(Error.InvalidBitshift);
                    left_ptr.* = left_ptr.* >> @truncate(right_ptr.*);
                    break :blk left;
                },
            },
        },
        .number => |number| switch (number.width) {
            inline else => |width| blk: {
                const comptype: Type = .{ .number = .{ .type = .uint, .width = width } };
                const T = comptype.ToZig();
                const left_value: T = util.extract(T, left.payload.wide);
                const right_value: T = util.extract(T, right.payload.wide);
                if (right_value > @intFromEnum(width)) return self.errorOut(Error.InvalidBitshift);
                break :blk .{ .type = left.type, .payload = .{ .wide = util.fit(WIDE, left_value >> @truncate(right_value)) } };
            },
        },
        else => unreachable,
    };
}
fn refineAdd(self: *Parser, left: *Expression, right: *Expression) Error!Expression {
    const initial: Expression = .{ .bin_op = .{ .left = left, .right = right, .op = .@"+" } };
    var left_expr, var left_type, var right_expr, var right_type = .{ left.*, try self.typeOf(left.*), right.*, try self.typeOf(right.*) };

    if (!equalizeExprTypesIfUnknown(self, &left_expr, &left_type, &right_expr, &right_type)) return initial;
    if (left_type.isScalar() or right_type.isScalar()) try equalizeScalarTypes(self, &left_expr, &left_type, &right_expr, &right_type);
    if (!left_type.eql(right_type)) return self.errorInvalidOperandTypes(left_type, .@"+", right_type);
    //add values
    if (left_expr == .value and right_expr == .value) return .{ .value = try addValues(left_expr.value.type, left_expr.value.payload, right_expr.value.payload) };
    left.*, right.* = .{ left_expr, right_expr };
    return initial;
}
fn refineMul(self: *Parser, left: *Expression, right: *Expression) Error!Expression {
    const initial: Expression = .{ .bin_op = .{ .left = left, .right = right, .op = .@"*" } };

    var left_expr, var left_type, var right_expr, var right_type = .{ left.*, try self.typeOf(left.*), right.*, try self.typeOf(right.*) };

    if (!equalizeExprTypesIfUnknown(self, &left_expr, &left_type, &right_expr, &right_type)) return initial;

    //TODO: matrix bs
    if (left_type == .matrix) {
        //mat x mat
        // mat x vec
        // mar x scalar
        return Error.InvalidInput;
    } else if (right_type == .matrix) {
        //vec x mat
        //scalar x mat
        return Error.InvalidInput;
    } else if (left_type == .vector or right_type == .vector) {
        if (right_type == .vector) {
            std.mem.swap(Type, &left_type, &right_type);
            std.mem.swap(Expression, &left_expr, &right_expr);
        }
        right_expr = self.implicitCast(right_expr, .{ .number = left_type.vector.component }) catch
            try self.implicitCast(right_expr, left_type);

        if (left_expr == .value and right_expr == .value)
            return .{ .value = try mulVecOrScalarValues(self, left_expr.value, right_expr.value) };
        left.*, right.* = .{ left_expr, right_expr };
        return initial;
    } else if (left_type.isScalar()) {
        try equalizeScalarTypes(self, &left_expr, &left_type, &right_expr, &right_type);
        if (left_expr == .value and right_expr == .value)
            return .{ .value = try mulVecOrScalarValues(self, left_expr.value, right_expr.value) };

        left.*, right.* = .{ left_expr, right_expr };
        return initial;
    } else return self.errorOutFmt(
        Error.InvalidOperands,
        "Invalid multiplication operands: {f} * {f}",
        .{ left_type, right_type },
    );
}
fn equalizeExprTypesIfUnknown(self: *Parser, left_expr: *Expression, left_type: *Type, right_expr: *Expression, right_type: *Type) bool {
    if (left_type.* == .unknown and right_type.* == .unknown) return false;
    if (left_type.* == .unknown) {
        left_expr.* = self.implicitCast(left_expr.*, right_type.*) catch return false;
        left_type.* = right_type.*;
    } else if (right_type.* == .unknown) {
        right_expr.* = self.implicitCast(right_expr.*, left_type.*) catch return false;
        right_type.* = left_type.*;
    }
    return true;
}
fn equalizeScalarTypes(self: *Parser, left_expr: *Expression, left_type: *Type, right_expr: *Expression, right_type: *Type) Error!void {
    const result_type: Type = if (left_type.scalarRestrictiveness() > right_type.scalarRestrictiveness()) left_type.* else right_type.*;
    left_expr.* = try self.implicitCast(left_expr.*, result_type);
    right_expr.* = try self.implicitCast(right_expr.*, result_type);
    left_type.* = result_type;
    right_type.* = result_type;
}
// fn dotValues(left: Value, right: Value) Error!Value {
//     const t, const a, const b = try implicitCastEqualizeValues(left, right);
//     if (t != .vector or t.vector.component.type != .float) return Error.InvalidOperands;
//     // return self.errorOutFmt(Error.InvalidUnaryOperationTarget, "Only floating point vectors can be normalized", .{});
//     switch (t.vector.len) {
//         inline else => |len| switch (t.vector.component.width) {
//             inline else => |width| {
//                 const comptype: Type = .{ .vector = .{ .len = len, .component = .{ .type = .float, .width = width } } };
//                 const T = comptype.ToZig();
//                 // const Elem = (Type{ .number = comptype.component }).ToZig();

//                 const left_ptr: *T = @ptrCast(@alignCast(@constCast(a.ptr)));
//                 const right_ptr: *T = @ptrCast(@alignCast(@constCast(b.ptr)));

//                 left_ptr[0] = @reduce(.Add, left_ptr.* * right_ptr.*);
//                 return .{ .type = .{ .number = t.vector.component }, .payload = .{ .wide = util.fit(WIDE, left_ptr[0]) } };
//             },
//         },
//     }
//     unreachable;
// }
// fn powValues(left: Value, right: Value) Error!Value {
//     const t, const a, const b = try implicitCastEqualizeValues(left, right);
//     const payload: Parser.ValuePayload = switch (t) {
//         .compint => .{ .wide = util.fit(WIDE, std.math.powi(CI, util.extract(CI, a.wide), util.extract(CI, b.wide)) catch return Error.NumericError) },
//         .compfloat => .{ .wide = util.fit(WIDE, @as(CF, @floatCast(std.math.pow(
//             f64,
//             @floatCast(util.extract(CF, a.wide)),
//             @floatCast(util.extract(CF, b.wide)),
//         )))) },
//         //number , vector
//         else => return Error.InvalidOperands,
//     };
//     return .{ .type = t, .payload = payload };
// }

fn mulVecOrScalarValues(self: *Parser, left: Value, right: Value) Error!Value {
    var left_value = left;
    var right_value = right;
    if (right.type == .vector) std.mem.swap(Value, &right_value, &left_value);
    right_value = if (left_value.type == .vector and right_value.type.isScalar())
        try splatNumber(
            self,
            left_value.type.vector,
            (try implicitCastValue(right_value, .{ .number = left_value.type.vector.component })).payload.wide,
        )
    else
        try implicitCastValue(right_value, left_value.type);

    const t, const a, const b = .{ left_value.type, left_value.payload, right_value.payload };
    const payload: Parser.ValuePayload = switch (t) {
        .compint => .{ .wide = util.fit(WIDE, util.extract(CI, a.wide) * util.extract(CI, b.wide)) },
        .compfloat => .{ .wide = util.fit(WIDE, util.extract(CF, a.wide) * util.extract(CF, b.wide)) },
        //number , vector
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.component.type) {
                inline else => |num_type| switch (vector.component.width) {
                    inline else => |width| blk: {
                        const V = (tp.Vector{ .len = len, .component = .{ .type = num_type, .width = width } }).ToZig();
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
fn splatNumber(self: *Parser, vector_type: tp.Vector, wide: WIDE) Error!Value {
    return switch (vector_type.len) {
        inline else => |l| switch (vector_type.component.type) {
            inline else => |nt| switch (vector_type.component.width) {
                inline else => |width| blk: {
                    const comptype: Type = .{ .number = .{ .type = nt, .width = width } };
                    const T = comptype.ToZig();
                    const V = @Vector(@intCast(@intFromEnum(l)), T);

                    const num = util.extract(T, wide);
                    const ptr = try self.create(V);
                    inline for (0..@intFromEnum(l)) |i| ptr[i] = num;

                    break :blk .{
                        .type = .{ .vector = .{ .len = l, .component = comptype.number } },
                        .payload = .{ .ptr = @ptrCast(@alignCast(ptr)) },
                    };
                },
            },
        },
    };
}
fn addValues(t: Type, left: Parser.ValuePayload, right: Parser.ValuePayload) Error!Value {
    const payload: Parser.ValuePayload = switch (t) {
        .compint => .{ .wide = util.fit(WIDE, util.extract(CI, left.wide) + util.extract(CI, right.wide)) },
        .compfloat => .{ .wide = util.fit(WIDE, util.extract(CF, left.wide) + util.extract(CF, right.wide)) },
        //number , vector
        else => return Error.InvalidOperands,
    };
    return .{ .type = t, .payload = payload };
}
pub fn implicitCast(self: *Parser, expr: Expression, @"type": Type) Error!Expression {
    if (@"type" == .unknown) return expr;

    const type_of = try self.typeOf(expr);
    if (type_of.eql(@"type")) return expr;
    const result: Expression = switch (expr) {
        .value => |value| .{ .value = try implicitCastValue(value, @"type") },
        .constructor => |constructor| try refineConstructor(self, if (constructor.type == .unknown)
            .{ .components = constructor.components, .type = @"type" }
        else
            return Error.CannotImplicitlyCast),
        else => expr,
        .cast => |cast| try implicitCastCast(self, cast, @"type"),
    };
    return if (@"type".eql(try self.typeOf(result))) result else errorImplicitCast(self, result, @"type");
}
fn implicitCastCast(self: *Parser, cast: Parser.Cast, @"type": Type) Error!Expression {
    const initial: Expression = .{ .cast = cast };
    if (cast.type.isEmpty())
        return try refine(self, Expression{ .cast = .{ .type = @"type", .expr = cast.expr } });

    if (cast.type == .unknown) return initial;
    return if (cast.type.eql(@"type")) initial else self.errorOut(Error.CannotImplicitlyCast);
}
pub fn implicitCastValue(value: Value, target: Type) Error!Value {
    if (value.type.eql(target)) return value;

    return switch (target) {
        .compint => if (value.type == .compfloat)
            .{ .type = .compint, .payload = .{ .wide = util.fit(WIDE, @as(CI, @intFromFloat(blk: {
                const float = util.extract(CF, value.payload.wide);
                break :blk if (@floor(float) == float) float else return Error.CannotImplicitlyCast;
            }))) } }
        else
            Error.CannotImplicitlyCast,
        .compfloat => if (value.type == .compint)
            .{ .type = .compfloat, .payload = .{ .wide = util.fit(WIDE, @as(CF, @floatFromInt(util.extract(CI, value.payload.wide)))) } }
        else
            Error.CannotImplicitlyCast,
        //@PREVENT ROUNDING COMPTIME FLOATS WHEN CASING TO INTEGER
        .number => |number| .{ .type = target, .payload = .{ .wide = try switch (number.type) {
            inline else => |t| switch (number.width) {
                inline else => |w| switch (value.type) {
                    .compint => util.fit(WIDE, numberCast(
                        comptime (tp.Number{ .type = t, .width = w }).ToZig(),
                        util.extract(CI, value.payload.wide),
                    )),
                    .compfloat => util.fit(WIDE, numberCast(
                        comptime (tp.Number{ .type = t, .width = w }).ToZig(),
                        util.extract(CF, value.payload.wide),
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

const EqualizeResult = std.meta.Tuple(&.{ Type, Parser.ValuePayload, Parser.ValuePayload });

const CI = Parser.CI;
const CF = Parser.CF;
const WIDE = Parser.WIDE;
const Error = Parser.Error;
const Expression = Parser.Expression;
const Value = Parser.Value;
const Type = tp.Type;
