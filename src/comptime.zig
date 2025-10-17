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
    const callee_type = try self.typeOf(call.callee.*);
    if (callee_type == .unknown) return initial;
    if (callee_type != .function) return self.errorOut(Error.InvalidCall);

    return if (call.callee.* == .builtin and call.callee.builtin == .function)
        try bi.refineBuiltinCall(self, call.callee.builtin.function, call.args, call.callee) //
    else if (call.callee.* == .value) try callFunctionComptime(self, call) //
    else initial;
}

fn callFunctionComptime(self: *Parser, call: Parser.Call) Error!Expression {
    const initial: Expression = .{ .call = call };
    const function: Function = @as(*const Function, @ptrCast(@alignCast(call.callee.value.payload.ptr))).*;
    if (!function.is_pure) return initial;

    for (call.args, function.type.arg_types) |arg, arg_type| {
        if (arg != .value and !arg.isEmpty()) return initial;
        if (arg_type == .unknown) @panic("TODO: 'Generic' functions");
    }
    //at this point function is pure, non generic and all argument are of known type
    // for now assume that function can return early
    // recursion handled at definition time
    var dispatch = try FunctionDispatch.new(self, &function, call.args);
    defer dispatch.deinit(self);

    const last_scope = self.current_scope;
    self.current_scope = &dispatch.scope;
    defer self.current_scope = last_scope;

    for (function.body.items) |statement| {
        try self.addStatement(statement);
        if (!dispatch.returned_expr.isEmpty()) return dispatch.returned_expr.*;
    }

    return initial;
}
const FunctionDispatch = struct {
    var_decls: []DispatchVariable,
    var_decl_count: usize = 0,

    args: []Value,
    function: *const Function,

    returned_expr: *const Expression = &Expression.empty,
    scope: Scope,

    pub fn new(parser: *Parser, function: *const Function, args: []Expression) Error!FunctionDispatch {
        return .{
            .scope = .{
                .addStatementFn = &addStatementFn,
                .trackReferenceFn = &trackReferenceFn,
                .getVariableReferenceFn = &getVariableReferenceFn,
                .result_type = function.type.rtype.*,
            },
            .var_decls = try parser.arena.allocator().alloc(DispatchVariable, function.body.items.len),
            .function = function,
            .args = blk: {
                const slice = try parser.arena.allocator().alloc(Value, args.len);
                for (slice, args) |*v, arg| v.* = arg.value;
                break :blk slice;
            },
        };
    }
    pub fn deinit(self: *FunctionDispatch, parser: *Parser) void {
        parser.arena.allocator().free(self.args);
        parser.arena.allocator().free(self.var_decls);
    }
    fn addStatementFn(scope: *Scope, parser: *Parser, statement: Statement) Error!void {
        const dispatch: *FunctionDispatch = @fieldParentPtr("scope", scope);

        const refined_statement = try refineStatementDispatch(parser, statement);
        if (!(try parser.isStatementComplete(refined_statement))) return parser.errorOut(Error.IncompleteStatement);

        switch (refined_statement) {
            .var_decl => |var_decl| {
                dispatch.var_decls[dispatch.var_decl_count] = .{
                    .name = var_decl.name,
                    .value = var_decl.initializer.value,
                };
                dispatch.var_decl_count += 1;
            },
            // .assignment => |assignment| (for (dispatch.var_decls.items) |*vd| (break vd) else unreachable).value = expr.value.value,
            .@"return" => |expr| dispatch.returned_expr = try parser.createVal(expr),
            else => {},
        }
    }
    fn getVariableReferenceFn(scope: *Scope, _: *Parser, name: []const u8) Error!Parser.VariableReference {
        const dispatch: *FunctionDispatch = @fieldParentPtr("scope", scope);
        const val = for (dispatch.var_decls) |vd|
            (if (util.strEql(vd.name, name)) break vd.value)
        else for (dispatch.function.arg_names, dispatch.args) |arg_name, arg|
            (if (util.strEql(arg_name, name)) break arg)
        else
            unreachable;
        // else
        //     unreachable;
        return .{ .is_mutable = true, .type = val.type, .value = .{ .value = val } };
    }
    fn trackReferenceFn(_: *Scope, _: []const u8, _: Parser.Scope.DeclReferenceType) Error!void {}
    fn refineStatementDispatch(self: *Parser, statement: Statement) Error!Statement {
        return switch (statement) {
            .var_decl => |var_decl| blk: {
                var vd = var_decl;
                if (vd.type == .unknown) vd.type = try self.asTypeCreate(try refineDescend(self, vd.type.unknown.*));
                vd.initializer = try self.implicitCast(try refineDescend(self, vd.initializer), vd.type);

                break :blk .{ .var_decl = vd };
            },
            // .assignment => |assignment| blk: {
            // },
            .@"return" => |returned| if (!returned.isEmpty())
                .{ .@"return" = try self.implicitCast(try refineDescend(self, returned), self.current_scope.result_type) }
            else
                statement,
            else => statement,
        };
    }
};

const DispatchVariable = struct {
    name: []const u8,
    value: Value,
};
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
        .scalar => |scalar| switch (scalar.type) {
            inline else => |st| switch (scalar.width) {
                inline else => |width| blk: {
                    const totype: Type = .{ .scalar = .{ .type = st, .width = width } };
                    const T = totype.ToZig();
                    break :blk switch (value.type) {
                        .bool => .{ .type = totype, .payload = .{ .wide = util.fit(WIDE, util.numericCast(T, util.extract(bool, value.payload.wide))) } },
                        .scalar => |from_scalar| switch (from_scalar.type) {
                            inline else => |from_st| switch (from_scalar.width) {
                                inline else => |from_width| .{ .type = totype, .payload = .{ .wide = util.fit(WIDE, util.numericCast(T, util.extract(
                                    (Type{ .scalar = .{ .type = from_st, .width = from_width } }).ToZig(),
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
        .scalar, .compint, .compfloat, .bool, .@"enum" => to.isNumber() or to == .bool or to == .@"enum",
        .vector => |vector| switch (from) {
            .vector => |fromvec| fromvec.len == vector.len,
            .array => |array| array.len == @intFromEnum(vector.len),
            .scalar, .compint, .compfloat, .bool, .@"enum" => true,
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
        .scalar => |scalar| switch (scalar.width) {
            inline else => |width| @intCast(util.extract(
                (Type{ .scalar = .{ .width = width, .type = .uint } }).ToZig(),
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
            .scalar, .compint, .compfloat => blk: {
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
        .@";" => blk: {
            const @"type" = try self.typeOf(u_op.target.*);
            if (@"type" != .vector or @"type".vector.component.type != .float)
                return self.errorOutFmt(Error.InvalidUnaryOperationTarget, "Only floating point vectors can be normalized", .{});
            if (u_op.target.* != .value) break :blk initial;
            normalizeValue(u_op.target.value);
            break :blk u_op.target.*;
        },
        else => initial,
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
        .@"+", .@"-" => try refineAddOrSub(self, @enumFromInt(@intFromEnum(bin_op.op)), bin_op.left, bin_op.right),
        .@">>" => try refineRightShift(self, bin_op.left, bin_op.right),
        .@"&", .@"|", .@"^^" => try refineAndOrXor(self, @enumFromInt(@intFromEnum(bin_op.op)), bin_op.left, bin_op.right),
        .@"**", .@"***" => if (bin_op.left.* == .value and bin_op.right.* == .value) .{ .value = try dotValues(bin_op.op == .@"***", bin_op.left.value.type, bin_op.left.value.payload, bin_op.right.value.payload) } else .{ .bin_op = bin_op },
        .@"^" => try refinePow(self, bin_op.left, bin_op.right),
        else => .{ .bin_op = bin_op },
        // else => initial,
    };
}
//we might allow pow instruction for integers?? through casting to float and then back
fn refinePow(self: *Parser, left: *Expression, right: *Expression) Error!Expression {
    const initial: Expression = .{ .bin_op = .{ .left = left, .right = right, .op = .@"^" } };
    _ = self;
    return initial;
}
fn refineAndOrXor(self: *Parser, op: AndOrXorOp, left: *Expression, right: *Expression) Error!Expression {
    const initial: Expression = .{ .bin_op = .{ .left = left, .right = right, .op = @enumFromInt(@intFromEnum(op)) } };
    var left_expr, var left_type, var right_expr, var right_type = .{ left.*, try self.typeOf(left.*), right.*, try self.typeOf(right.*) };

    if (!equalizeExprTypesIfUnknown(self, &left_expr, &left_type, &right_expr, &right_type)) return initial;
    if (right_type == .vector or left_type == .vector) {
        if (!left_type.eql(right_type) or left_type.vector.component.type == .float) return self.errorOut(Error.InvalidOperands);
    } else if (left_type.isNumber() and right_type.isNumber()) {
        try equalizeScalarTypes(self, &left_expr, &left_type, &right_expr, &right_type);
        if (left_type == .scalar and left_type.scalar.type == .float) return self.errorOut(Error.InvalidOperands);
        if (left_type == .compfloat) {
            left_expr = try self.implicitCast(left_expr, .compint);
            right_expr = try self.implicitCast(right_expr, .compint);
        }
    } else if (left_type != .bool and right_type != .bool) return self.errorOut(Error.InvalidOperands);
    if (left_expr == .value and right_expr == .value) return .{ .value = try andValues(@enumFromInt(@intFromEnum(op)), left_expr.value.type, left_expr.value.payload, right_expr.value.payload) };
    left.*, right.* = .{ left_expr, right_expr };
    return initial;
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
        const u32_type: Type = .{ .scalar = .{ .type = .uint, .width = .word } };
        if (left_type == .compfloat or left_type == .compint) {
            left_expr = .{ .value = try implicitCastValue(left_expr.value, u32_type) };
            left_type = u32_type;
        } else if (left_type != .scalar or left_type.scalar.type == .uint)
            return self.errorOut(Error.InvalidOperands);

        if (right_type == .compfloat or right_type == .compint) {
            right_expr = .{ .value = try implicitCastValue(right_expr.value, u32_type) };
            right_type = u32_type;
        } else if (right_type != .scalar or right_type.scalar.type != .uint)
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
        .scalar => |scalar| switch (scalar.width) {
            inline else => |width| blk: {
                const comptype: Type = .{ .scalar = .{ .type = .uint, .width = width } };
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
fn refineAddOrSub(self: *Parser, op: AddOrSubOp, left: *Expression, right: *Expression) Error!Expression {
    const initial: Expression = .{ .bin_op = .{ .left = left, .right = right, .op = @enumFromInt(@intFromEnum(op)) } };
    var left_expr, var left_type, var right_expr, var right_type = .{ left.*, try self.typeOf(left.*), right.*, try self.typeOf(right.*) };

    if (!equalizeExprTypesIfUnknown(self, &left_expr, &left_type, &right_expr, &right_type)) return initial;
    if (left_type.isNumber() or right_type.isNumber()) try equalizeScalarTypes(self, &left_expr, &left_type, &right_expr, &right_type);
    if (!left_type.eql(right_type)) return self.errorInvalidOperandTypes(left_type, .@"+", right_type);
    //add values
    if (left_expr == .value and right_expr == .value) return .{ .value = try addOrSubValues(op, left_expr.value.type, left_expr.value.payload, right_expr.value.payload) };
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
        right_expr = self.implicitCast(right_expr, .{ .scalar = left_type.vector.component }) catch
            try self.implicitCast(right_expr, left_type);

        if (left_expr == .value and right_expr == .value)
            return .{ .value = try mulVecOrScalarValues(self, left_expr.value, right_expr.value) };
        left.*, right.* = .{ left_expr, right_expr };
        return initial;
    } else if (left_type.isNumber()) {
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
    const result_type: Type = if (left_type.numberRestrictiveness() > right_type.numberRestrictiveness()) left_type.* else right_type.*;
    left_expr.* = try self.implicitCast(left_expr.*, result_type);
    right_expr.* = try self.implicitCast(right_expr.*, result_type);
    left_type.* = result_type;
    right_type.* = result_type;
}

fn mulVecOrScalarValues(self: *Parser, left: Value, right: Value) Error!Value {
    var left_value = left;
    var right_value = right;
    if (right.type == .vector) std.mem.swap(Value, &right_value, &left_value);
    right_value = if (left_value.type == .vector and right_value.type.isNumber())
        try splatScalar(
            self,
            left_value.type.vector,
            (try implicitCastValue(right_value, .{ .scalar = left_value.type.vector.component })).payload.wide,
        )
    else
        try implicitCastValue(right_value, left_value.type);

    const t, const a, const b = .{ left_value.type, left_value.payload, right_value.payload };
    return try mulValuesSameType({}, t, a, b);
}

fn splatScalar(self: *Parser, vector_type: tp.Vector, wide: WIDE) Error!Value {
    return switch (vector_type.len) {
        inline else => |l| switch (vector_type.component.type) {
            inline else => |st| switch (vector_type.component.width) {
                inline else => |width| blk: {
                    const comptype: Type = .{ .scalar = .{ .type = st, .width = width } };
                    const T = comptype.ToZig();
                    const V = @Vector(@intCast(@intFromEnum(l)), T);

                    const num = util.extract(T, wide);
                    const ptr = try self.create(V);
                    inline for (0..@intFromEnum(l)) |i| ptr[i] = num;

                    break :blk .{
                        .type = .{ .vector = .{ .len = l, .component = comptype.scalar } },
                        .payload = .{ .ptr = @ptrCast(@alignCast(ptr)) },
                    };
                },
            },
        },
    };
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
        .scalar => |scalar| .{ .type = target, .payload = .{ .wide = try switch (scalar.type) {
            inline else => |t| switch (scalar.width) {
                inline else => |w| switch (value.type) {
                    .compint => util.fit(WIDE, util.numericCast(
                        comptime (tp.Scalar{ .type = t, .width = w }).ToZig(),
                        util.extract(CI, value.payload.wide),
                    )),
                    .compfloat => util.fit(WIDE, util.numericCast(
                        comptime (tp.Scalar{ .type = t, .width = w }).ToZig(),
                        util.extract(CF, value.payload.wide),
                    )),
                    else => Error.CannotImplicitlyCast,
                },
            },
        } } },
        else => Error.CannotImplicitlyCast,
    };
}
const powValues = createEvalNumericBinOpSameTypeFunction(void, struct {
    pub fn pow(_: void, @"type": Type, left: anytype, right: anytype, result: *Value) void {
        const T = @TypeOf(right);
        const tinfo = @typeInfo(T);
        const l = if (tinfo == .vector) left.* else left;
        const res: T = if (tinfo == .int or tinfo == .float or tinfo == .vector) std.math.pow(T, l, right) else unreachable;
        if (tinfo == .vector) {
            left.* = res;
            result.* = .{ .type = @"type", .payload = .{ .ptr = @ptrCast(@alignCast(@constCast(left))) } };
        } else {
            result.* = .{ .type = @"type", .payload = .{ .wide = util.fit(WIDE, res) } };
        }
    }
}.pow);

const mulValuesSameType = createEvalNumericBinOpSameTypeFunction(void, struct {
    pub fn mul(_: void, @"type": Type, left: anytype, right: anytype, result: *Value) void {
        const T = @TypeOf(right);
        const tinfo = @typeInfo(T);
        const l = if (tinfo == .vector) left.* else left;
        const res: T = if (tinfo == .int or tinfo == .float or tinfo == .vector)
            l * right
        else
            unreachable;
        if (tinfo == .vector) {
            left.* = res;
            result.* = .{ .type = @"type", .payload = .{ .ptr = @ptrCast(@alignCast(@constCast(left))) } };
        } else {
            result.* = .{ .type = @"type", .payload = .{ .wide = util.fit(WIDE, res) } };
        }
    }
}.mul);
const dotValues = createEvalNumericBinOpSameTypeFunction(bool, struct {
    pub fn dot(clamped: bool, @"type": Type, left: anytype, right: anytype, result: *Value) void {
        const T = @TypeOf(right);
        const tinfo = @typeInfo(T);

        const res = if (tinfo == .vector) blk: {
            const unclamped = @reduce(.Add, left.* * right);
            break :blk if (clamped) @max(unclamped, 0) else unclamped;
        } else unreachable;
        result.* = .{ .type = .{ .scalar = @"type".vector.component }, .payload = .{ .wide = util.fit(WIDE, res) } };
    }
}.dot);
const addOrSubValues = createEvalNumericBinOpSameTypeFunction(AddOrSubOp, struct {
    pub fn add(op: AddOrSubOp, @"type": Type, left: anytype, right: anytype, result: *Value) void {
        const T = @TypeOf(right);
        const tinfo = @typeInfo(T);
        const l = if (tinfo == .vector) left.* else left;
        const res: T = if (tinfo == .int or tinfo == .float or tinfo == .vector)
            (if (op == .@"+")
                l + right
            else
                l - right)
        else
            unreachable;
        if (tinfo == .vector) {
            left.* = res;
            result.* = .{ .type = @"type", .payload = .{ .ptr = @ptrCast(@alignCast(@constCast(left))) } };
        } else {
            result.* = .{ .type = @"type", .payload = .{ .wide = util.fit(WIDE, res) } };
        }
    }
}.add);

const AddOrSubOp = enum(u8) {
    @"+" = @intFromEnum(BinaryOperator.@"+"),
    @"-" = @intFromEnum(BinaryOperator.@"-"),
};
const andValues = createEvalNumericBinOpSameTypeFunction(AndOrXorOp, struct {
    pub fn _and(op: AndOrXorOp, @"type": Type, left: anytype, right: anytype, result: *Value) void {
        const T = @TypeOf(right);
        const tinfo = @typeInfo(T);
        const l = if (tinfo == .vector) left.* else left;

        const res: T = if (T == bool) switch (op) {
            .@"&" => l and right,
            .@"|" => l or right,
            .@"^^" => l != right,
        } else if (tinfo == .int or (tinfo == .vector and @typeInfo(tinfo.vector.child) == .int)) switch (op) {
            .@"&" => l & right,
            .@"|" => l | right,
            .@"^^" => l ^ right,
        } else unreachable;
        if (tinfo == .vector) {
            left.* = res;
            result.* = .{ .type = @"type", .payload = .{ .ptr = @ptrCast(@alignCast(@constCast(left))) } };
        } else {
            result.* = .{ .type = @"type", .payload = .{ .wide = util.fit(WIDE, res) } };
        }
    }
}._and);
const AndOrXorOp = enum(u8) {
    @"|" = @intFromEnum(BinaryOperator.@"|"),
    @"&" = @intFromEnum(BinaryOperator.@"&"),
    @"^^" = @intFromEnum(BinaryOperator.@"^^"),
};
fn createEvalNumericBinOpSameTypeFunction(Ctx: type, eval_fn: fn (Ctx, Type, anytype, anytype, *Value) void) fn (Ctx, Type, Payload, Payload) Error!Value {
    return struct {
        pub fn f(ctx: Ctx, t: Type, left: Payload, right: Payload) Error!Value {
            return switch (t) {
                .bool => evalWide(bool, ctx, t, left, right),
                .compfloat => evalWide(CF, ctx, t, left, right),
                .compint => evalWide(CI, ctx, t, left, right),
                .scalar => |scalar| switch (scalar.type) {
                    inline else => |st| switch (t.scalar.width) {
                        inline else => |width| evalWide((Type{ .scalar = .{ .type = st, .width = width } }).ToZig(), ctx, t, left, right),
                    },
                },
                .vector => |vector| switch (vector.len) {
                    inline else => |len| switch (vector.component.type) {
                        inline else => |st| switch (vector.component.width) {
                            inline else => |width| blk: {
                                const T = (Type{ .vector = .{ .len = len, .component = .{ .type = st, .width = width } } }).ToZig();
                                var res: Value = undefined;
                                const left_ptr: *T = @ptrCast(@alignCast(@constCast(left.ptr)));
                                const right_val: T = @as(*const T, @ptrCast(@alignCast(right.ptr))).*;
                                eval_fn(ctx, t, left_ptr, right_val, &res);
                                break :blk res;
                            },
                        },
                    },
                },

                else => {
                    std.debug.print("T: {f}\n", .{t});

                    return Error.InvalidOperands;
                },
            };
        }

        fn evalWide(T: type, ctx: Ctx, @"type": Type, left: Payload, right: Payload) Value {
            var res: Value = undefined;
            eval_fn(ctx, @"type", util.extract(T, left.wide), util.extract(T, right.wide), &res);
            return res;
        }
    }.f;
}

const EqualizeResult = std.meta.Tuple(&.{ Type, Parser.ValuePayload, Parser.ValuePayload });

const CI = Parser.CI;
const CF = Parser.CF;
const WIDE = Parser.WIDE;
const Error = Parser.Error;
const Expression = Parser.Expression;
const Value = Parser.Value;
const Payload = Parser.ValuePayload;
const Statement = Parser.Statement;
const Function = Parser.Function;
const Scope = Parser.Scope;
const BinaryOperator = @import("Tokenizer.zig").BinaryOperator;
const Type = tp.Type;
const List = std.ArrayList;
const VariableDeclaration = Parser.VariableDeclaration;
