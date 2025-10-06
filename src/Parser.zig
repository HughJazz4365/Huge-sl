const std = @import("std");
const debug = @import("debug.zig");
const bi = @import("builtin.zig");
const ct = @import("comptime.zig");
const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");

tokenizer: *Tokenizer = undefined,

allocator: Allocator = undefined,
arena: std.heap.ArenaAllocator = undefined,

global_scope: GlobalScope = undefined,
current_scope: *Scope = undefined,

intermediate_value_index: u32 = 0,

pub const Error = error{
    OutOfMemory,

    UnexpectedToken,

    UndeclaredVariable,
    NoValueConstant,
    MutatingImmutableVariable,
    StageInputCantHaveInitialValue,

    CannotImplicitlyCast,
    CannotExplicitlyCast,
    CannotInferType,

    InvalidOperands,
    InvalidConstructor,
    InvalidIndex,

    NumericError,
    OutOfBoundsAccess,
    RecursionNotSupported,
    NegativePower, //a ^ (- |r|)
} || Tokenizer.Error;

pub fn parse(allocator: Allocator, tokenizer: *Tokenizer) Error!Parser {
    var self: Parser = .{};
    self.allocator = allocator;
    self.tokenizer = tokenizer;
    self.arena = .init(allocator);
    self.global_scope = .new(self.arena.allocator());
    self.current_scope = &self.global_scope.scope;

    while (try self.parseStatement()) |statement| {
        try self.global_scope.addStatement(statement);
        std.debug.print("{f}\n", .{statement});
    }
    return self;
}
pub fn deinit(self: *Parser) void {
    self.arena.deinit();
}
pub fn turnIntoIntermediateVariableIfNeeded(self: *Parser, expr: Expression) Error!Expression {
    if (!expr.shouldTurnIntoIntermediate()) return expr;

    const name = try self.createIntermediateVariableName();
    try self.addStatement(.{ .var_decl = .{
        .qualifier = .@"const",
        .name = name,
        .type = try self.typeOf(expr),
        .value = expr,
    } });
    return .{ .identifier = name };
}
pub fn createIntermediateVariableName(self: *Parser) Error![]u8 {
    const prefix = "IV ";
    const slice = try self.arena.allocator().alloc(u8, prefix.len + 4);
    @memcpy(slice[0..prefix.len], prefix);
    const byteptr: [*]const u8 = @ptrCast(@alignCast(&self.intermediate_value_index));
    inline for (0..4) |i| slice[slice.len - (4 - i)] = byteptr[i];
    self.intermediate_value_index += 1;
    return slice;
}

pub fn createVal(self: *Parser, value: anytype) !*@TypeOf(value) {
    const ptr = try self.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}
pub fn create(self: *Parser, T: type) !*T {
    return try self.arena.allocator().create(T);
}

pub inline fn addStatement(self: *Parser, statement: Statement) Error!void {
    try self.current_scope.addStatement(statement);
}
pub fn parseStatement(self: *Parser) Error!?Statement {
    var token = try self.tokenizer.peek();
    while (token == .endl) {
        self.tokenizer.skip();
        token = try self.tokenizer.peek();
    }

    return switch (token) {
        .eof => null,
        .@"const", .@"var", .uniform, .property, .shared, .out, .in => blk: {
            self.tokenizer.skip();
            break :blk try self.parseVariableDecl(token);
        },

        .@"return" => blk: {
            self.tokenizer.skip();
            const peek = try self.tokenizer.peek();
            if (self.defaultShouldStop(peek) catch unreachable) {
                if (peek != .@"}") self.tokenizer.skip();
                break :blk .{ .@"return" = null };
            }
            break :blk .{ .@"return" = try self.parseExpression(defaultShouldStop) };
        },

        else => try self.parseAssignmentOrIgnore(),
    };
    //1.if token == return => .return
    //2.if token == break => .break
    //3.try parsing qualifier, if can => continue to parse var_decl
    //4. parse expr if next token is '='
    //its an assignment and we parse the assigned value
    //else expr is .ignored

}
fn parseAssignmentOrIgnore(self: *Parser) Error!Statement {
    const target = try self.parseExpressionRecursive(assignmentShouldStop, false, 0);
    const peek = try self.tokenizer.peek();
    if (self.defaultShouldStop(peek) catch unreachable) return .{ .ignore = target };
    if (!try self.isExprMutable(target)) return Error.MutatingImmutableVariable;

    const modifier: ?BinaryOperator = if (peek == .bin_op) op: {
        self.tokenizer.skip();
        break :op peek.bin_op;
    } else null;
    self.tokenizer.skip(); //will always be '=' since we checked for it in 'shouldStop'

    var value = try self.refine(try self.parseExpression(defaultShouldStop));

    if (modifier) |mod| {
        const create_value = try self.createVal(value);
        value = try self.refine(.{ .bin_op = .{
            .left = try self.createVal(target),
            .right = create_value,
            .op = mod,
        } });
    }
    value = try self.implicitCast(value, try self.typeOf(target));
    return .{ .assignment = .{
        .target = target,
        .value = value,
    } };
}
fn parseVariableDecl(self: *Parser, token: Token) Error!Statement {
    const qualifier = try self.parseQualifier(token);

    const name_token = try self.tokenizer.next();
    if (name_token != .identifier) return Error.UnexpectedToken;

    var @"type": Type = blk: {
        const next = try self.tokenizer.peek();
        if (next != .@":") break :blk .unknown;
        self.tokenizer.skip();

        const expr = try self.parseExpressionSide();
        break :blk self.asType(expr) orelse return Error.UnexpectedToken;
    };

    const value: ?Expression = blk: {
        const next = try self.tokenizer.peek();
        if (next != .@"=") break :blk null;
        self.tokenizer.skip();
        var expr = try self.parseExpression(defaultShouldStop);
        if (@"type" != .unknown)
            expr = try self.implicitCast(expr, @"type")
        else
            @"type" = try self.typeOf(expr);

        if (!@"type".isComplete()) {
            expr = try self.makeExprCompleteType(expr);
            @"type" = try self.typeOf(expr);
            if (!@"type".isComplete()) return Error.CannotInferType;
        }
        break :blk expr;
    };
    if (value == null and qualifier == .@"const") return Error.NoValueConstant;
    if (value != null and qualifier == .in) return Error.StageInputCantHaveInitialValue;

    return .{ .var_decl = .{
        .qualifier = qualifier,
        .name = name_token.identifier,
        .type = @"type",
        .value = if (value) |v| v else Expression.empty,
    } };
}

fn makeExprCompleteType(self: *Parser, expr: Expression) Error!Expression {
    _ = self;
    return expr;
}
pub const Statement = union(enum) {
    var_decl: VariableDecl,
    //   [qualifier] [name] {:} {type expr} {=} {value expr}
    assignment: Assignment,
    //   [target expr] {binop} [=] [value expr]
    ignore: Expression,
    @"return": ?Expression,
    @"break": ?Expression,
    pub const format = debug.formatStatement;
};
pub const Assignment = struct {
    target: Expression,
    value: Expression,
};
pub const VariableDecl = struct {
    qualifier: Qualifier,
    name: []const u8,
    type: Type,
    value: Expression,
    reference_count: u32 = 0,

    pub fn variableReference(self: *VariableDecl) VariableReference {
        return .{
            .is_mutable = self.qualifier.isMutable(),
            .type = self.type,
            .value = &self.value,
        };
    }
};

pub fn parseExpression(self: *Parser, should_stop: ShouldStopFn) Error!Expression {
    return try self.parseExpressionRecursive(should_stop, true, 0);
}
pub fn parseExpressionRecursive(self: *Parser, should_stop: ShouldStopFn, should_consume_end: bool, last_bp: u8) Error!Expression {
    var left = try self.refine(try self.parseExpressionSide());
    var token = try self.tokenizer.peek();
    while (!try should_stop(self, token)) {
        if (token != .bin_op) {
            std.debug.print("UT: {f}, left: {f}\n", .{ token, left });
            return Error.UnexpectedToken;
        }

        const op = token.bin_op;
        //go left
        if (Tokenizer.bindingPower(op) <= last_bp) break;

        self.tokenizer.skip();

        const right = try self.refine(try self.parseExpressionRecursive(should_stop, false, Tokenizer.bindingPower(op)));
        const add_left = try self.createVal(left);
        //go right
        left = try self.refine(.{ .bin_op = .{
            .left = add_left,
            .right = try self.createVal(right),
            .op = op,
        } });

        token = try self.tokenizer.peek();
    }
    if (should_consume_end) self.tokenizer.skip();

    return left;
}

fn assignmentShouldStop(self: *Parser, token: Token) Error!bool {
    const peek = try self.tokenizer.peekTimes(2);
    const def = token == .endl or token == .eof or (token == .@"}" and @intFromPtr(self.current_scope) != @intFromPtr(&self.global_scope.scope));
    return token == .@"=" or token == .bin_op and peek == .@"=" or def;
}

fn bracketShouldStop(self: *Parser, token: Token) !bool {
    _ = self;
    return token == .@")";
}

fn argsShouldStop(self: *Parser, token: Token) !bool {
    _ = self;
    return token == .@")" or token == .@",";
}

fn constructorShouldStop(self: *Parser, token: Token) !bool {
    _ = self;
    return token == .@"}" or token == .@",";
}

fn defaultShouldStop(self: *Parser, token: Token) !bool {
    return token == .endl or token == .eof or (token == .@"}" and @intFromPtr(self.current_scope) != @intFromPtr(&self.global_scope.scope));
}

fn parseExpressionSide(self: *Parser) Error!Expression {
    const token = try self.tokenizer.next();
    var expr: Expression = switch (token) {
        .u_op => |u_op| .{ .u_op = .{
            .op = u_op,
            .target = try self.createVal(try self.parseExpressionSide()),
        } },
        .identifier => |id| blk: {
            const name = id;
            break :blk .{ .identifier = name };
        },
        .@"(" => try self.parseExpression(bracketShouldStop),
        .@"." => switch (try self.tokenizer.next()) {
            .@"{" => try self.parseCastOrConstructor(.unknown),
            else => return Error.UnexpectedToken,
        },
        // .@"fn" => try self.parseFunctionOrType(),
        .entrypoint => try self.parseEntryPointTypeOrValue(),
        // .compfloat, .compint => .@"if",
        else => .{ .value = try self.parseValue(token) },
    };
    var peek: Token = undefined;
    while (true) {
        peek = try self.tokenizer.peek();
        if (peek == .@"{") {
            if (self.asType(expr)) |at| {
                self.tokenizer.skip();
                expr = try self.parseCastOrConstructor(at);
                continue;
            } else return Error.UnexpectedToken;
        }
        break;
    }

    //while(true)
    //if nothing matches break

    //expr =
    //::::secondary::::
    //(check proceeding tokens)
    //if swizzle => swizzle(expr)
    //if '[' => indexing into expr
    //if '(' => function call
    //if '.' =>  member access
    return expr;
}

fn parseCastOrConstructor(self: *Parser, @"type": Type) Error!Expression {
    var list: List(Expression) = try .initCapacity(self.arena.allocator(), 1);

    var token = try self.tokenizer.peek();
    while (token != .@"}") {
        const expr = try self.refine(try self.parseExpressionRecursive(constructorShouldStop, false, 0));
        try list.append(self.arena.allocator(), expr);
        if (try self.tokenizer.peek() == .@",") self.tokenizer.skip();
        token = try self.tokenizer.peek();
    }
    self.tokenizer.skip();

    if (list.items.len == 0) return Error.InvalidConstructor;

    return if (list.items.len == 1) .{ .cast = .{
        .type = @"type",
        .expr = @ptrCast(list.items.ptr),
    } } else .{ .constructor = .{ .type = @"type", .components = list.items } };
}
fn parseEntryPointTypeOrValue(self: *Parser) Error!Expression {
    if (try self.tokenizer.next() != .@"(") return Error.UnexpectedToken;
    var token = try self.tokenizer.next();
    while (token != .@")") token = try self.tokenizer.next();

    const stage_info: ShaderStageInfo = .fragment;
    const ep_type: Type = .{ .entrypoint = std.meta.activeTag(stage_info) };

    if (try self.tokenizer.peek() != .@"{") return .{ .value = .{ .type = .type, .payload = .{ .type = ep_type } } };
    self.tokenizer.skip();

    var entry_point: EntryPoint = .new(self, stage_info);
    try self.parseScope(&entry_point.scope);

    const ep_ptr = try self.createVal(entry_point);
    return .{ .value = .{
        .type = ep_type,
        .payload = .{ .ptr = @ptrCast(@alignCast(ep_ptr)) },
    } };
}

fn parseScope(self: *Parser, scope: *Scope) Error!void {
    self.current_scope = scope;
    defer self.current_scope = self.current_scope.parent;
    while (try self.parseStatement()) |statement| {
        try self.addStatement(statement);
        if (try self.tokenizer.peek() == .@"}") {
            self.tokenizer.skip();
            break;
        }
    }
}

pub const EntryPoint = struct {
    stage_info: ShaderStageInfo,
    scope: Scope,

    allocator: Allocator,
    body: List(Statement),

    pub fn new(self: *Parser, stage_info: ShaderStageInfo) EntryPoint {
        return .{
            .stage_info = stage_info,
            .allocator = self.arena.allocator(),
            .body = .empty,
            .scope = .{
                .getVariableReferenceFn = &getVariableReferenceFn,
                .addStatementFn = &addStatementFn,
                .trackReferenceFn = &trackReferenceFn,
                .parent = self.current_scope,
            },
        };
    }
    pub inline fn addStatement(scope: *Scope, statement: Statement) Error!void {
        try scope.addStatementFn(scope, statement);
    }
    fn addStatementFn(scope: *Scope, statement: Statement) Error!void {
        const entry_point: *EntryPoint = @fieldParentPtr("scope", scope);
        try entry_point.body.append(entry_point.allocator, statement);
    }
    pub fn getVariableReferenceFn(scope: *Scope, name: []const u8) Error!VariableReference {
        const entry_point: *EntryPoint = @fieldParentPtr("scope", scope);
        return for (entry_point.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name))
                break statement.var_decl.variableReference();
        } else try scope.parent.getVariableReferece(name);
    }
    fn trackReferenceFn(scope: *Scope, name: []const u8, ref_type: Scope.DeclReferenceType) Error!void {
        const entry_point: *EntryPoint = @fieldParentPtr("scope", scope);
        for (entry_point.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name)) {
                statement.var_decl.reference_count = if (ref_type == .track) statement.var_decl.reference_count + 1 else statement.var_decl.reference_count -| 1;
                return;
            }
        }
        try scope.parent.trackReference(name, ref_type);
    }
};

pub const ShaderStageInfo = union(tp.ShaderStage) {
    vertex,
    fragment,
    compute: [3]u32,
};
fn parseValue(self: *Parser, token: Token) Error!Value {
    //true, false
    _ = self;
    return switch (token) {
        .compint => |compint| .{ .type = .compint, .payload = .{ .wide = @bitCast(compint) } },
        .compfloat => |compfloat| .{ .type = .compfloat, .payload = .{ .wide = @bitCast(compfloat) } },
        .type_literal => |tl| .{ .type = .type, .payload = .{ .type = tl } },
        else => Error.UnexpectedToken,
    };
}
pub fn typeOf(self: *Parser, expr: Expression) Error!Type {
    return switch (expr) {
        .value => |value| value.type,
        .constructor => |constructor| constructor.type,
        .cast => |cast| cast.type,
        .identifier => |identifier| (try self.current_scope.getVariableReferece(identifier)).type,
        .bin_op => |bin_op| try self.typeOfBinOp(bin_op),
        .indexing => |indexing| (try self.typeOf(indexing.target.*)).constructorStructure().component,
        else => .unknown,
    };
}
fn typeOfBinOp(self: *Parser, bin_op: BinOp) Error!Type {
    return switch (bin_op.op) {
        .@"+", .@"-", .@"*", .@"^" => blk: {
            const left_type = try self.typeOf(bin_op.left.*);
            const right_type = try self.typeOf(bin_op.right.*);
            break :blk if (left_type.eql(right_type)) left_type else .unknown;
        },
    };
}
pub fn asType(self: *Parser, expr: Expression) ?Type {
    _ = self;
    return switch (expr) {
        .value => |value| if (value.type == .type) value.payload.type else null,
        else => null,
    };
}

pub const Expression = union(enum) {
    bin_op: BinOp,
    u_op: UOp,
    fn_call,
    //   [name] ['('] {args} [')']
    identifier: []const u8,
    swizzle: Swizzle,
    //   [expr] [swizzle]
    member_access: MemberAccess,
    //   [expr] ['.'] [name]
    indexing: Indexing,
    //   [expr] ['['] [index] [']']

    //type fields should be Exprssession for both of those
    //ex. @TypeOf([dependent on generic or smth]){value}// cast to @TypeOf(...)
    cast: Cast,
    constructor: Constructor,

    //unified loop syntax?
    @"for",
    @"while",
    @"if",
    @"switch",

    value: Value,
    pub const empty = Expression{ .value = .{ .type = .type, .payload = .{ .type = .unknown } } };
    pub const format = debug.formatExpression;

    pub fn shouldTurnIntoIntermediate(self: Expression) bool {
        return switch (self) {
            .value, .identifier => false,
            else => true,
        };
    }
    pub fn isEmptyExpression(self: Expression) bool {
        return self == .value and self.value.type == .type and self.value.payload.type == .unknown;
    }
};
pub fn isExprMutable(self: *Parser, expr: Expression) Error!bool {
    return switch (expr) {
        .identifier => |identifier| (try self.current_scope.getVariableReferece(identifier)).is_mutable,
        .indexing => |indexing| try self.isExprMutable(indexing.target.*),
        else => false,
    };
}
pub const Cast = struct {
    type: Type,
    expr: *Expression,
};
pub const Constructor = struct {
    type: Type,
    components: []Expression,
};
pub const Indexing = struct {
    target: *Expression,
    index: *Expression,
};
pub const MemberAccess = struct {
    target: *Expression,
    member_name: []const u8,
};
pub const Swizzle = struct {
    target: *Expression,
    swizzle: []const u8,
};
pub const UOp = struct {
    op: UnaryOperator,
    target: *Expression,
};
pub const BinOp = struct {
    left: *Expression,
    op: BinaryOperator,
    right: *Expression,
};

pub const GlobalScope = struct {
    scope: Scope,
    allocator: Allocator,

    body: List(Statement) = .empty,

    pub fn new(allocator: Allocator) @This() {
        return .{
            .allocator = allocator,
            .scope = .{
                .getVariableReferenceFn = &getVariableReferenceFn,
                .trackReferenceFn = &trackReferenceFn,
                .addStatementFn = &addStatementFn,
            },
        };
    }
    pub fn addStatement(self: *GlobalScope, statement: Statement) Error!void {
        try addStatementFn(&self.scope, statement);
    }

    fn addStatementFn(scope: *Scope, statement: Statement) Error!void {
        const global_scope: *GlobalScope = @fieldParentPtr("scope", scope);
        try global_scope.body.append(global_scope.allocator, statement);
    }
    fn getVariableReferenceFn(scope: *Scope, name: []const u8) Error!VariableReference {
        const global_scope: *GlobalScope = @fieldParentPtr("scope", scope);
        return for (global_scope.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name))
                break statement.var_decl.variableReference();
        } else Error.UndeclaredVariable;
    }
    fn trackReferenceFn(scope: *Scope, name: []const u8, ref_type: Scope.DeclReferenceType) Error!void {
        const global_scope: *GlobalScope = @fieldParentPtr("scope", scope);
        for (global_scope.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name)) {
                statement.var_decl.reference_count = if (ref_type == .track) statement.var_decl.reference_count + 1 else statement.var_decl.reference_count -| 1;
                return;
            }
        }
        return Error.UndeclaredVariable;
    }
};

pub const Scope = struct {
    addStatementFn: *const fn (*Scope, Statement) Error!void = undefined,
    getVariableReferenceFn: *const fn (*Scope, []const u8) Error!VariableReference = undefined,
    trackReferenceFn: *const fn (*Scope, []const u8, DeclReferenceType) Error!void = undefined,

    parent: *Scope = undefined,

    pub const DeclReferenceType = enum { track, untrack };

    pub inline fn addStatement(self: *Scope, statement: Statement) Error!void {
        try self.addStatementFn(self, statement);
    }

    pub inline fn getVariableReferece(self: *Scope, name: []const u8) Error!VariableReference {
        return try self.getVariableReferenceFn(self, name);
    }

    pub inline fn trackReference(self: *Scope, name: []const u8, ref_type: DeclReferenceType) Error!void {
        // std.debug.print("TRACKREF\n", .{});
        try self.trackReferenceFn(self, name, ref_type);
        // @panic("RIaiornstoirns");
    }
};
pub const VariableReference = struct {
    is_mutable: bool,
    type: Type,
    value: *Expression,
    pub fn isComptime(self: VariableReference) bool {
        return !self.is_mutable and self.value.* == .value;
    }
};

fn parseQualifier(self: *Parser, token: Token) Error!Qualifier {
    _ = self;
    return switch (token) {
        .in => .{ .in = .smooth },
        .out => .{ .out = .smooth },
        .@"const" => .@"const",
        .@"var" => .@"var",
        .uniform => .uniform,
        .property => .property,
        .shared => .shared,
        else => Error.UnexpectedToken,
    };
}
pub const Qualifier = union(enum) {
    @"const",
    @"var",
    shared,
    uniform,
    property,
    in: bi.InterpolationQualifier,
    out: bi.InterpolationQualifier,
    pub fn isMutable(self: Qualifier) bool {
        return switch (self) {
            .@"const", .in, .uniform, .property => false,
            else => true,
        };
    }
};

pub const implicitCast = ct.implicitCast;

// pub fn parseFunctionOrType(self: *Parser) Error!Expression {
//     const arg_types: List(Type) = .empty;
//     const arg_names: List([]const u8) = .empty;

//     var rtype: Type = .void;
//     _ = &.{ arg_names, arg_types, &rtype };

//     var token = try self.tokenizer.next();
//     if (token == .@"(") {
//         var arg_index: usize = 0;
//         _ = &arg_index;
//         //parse args
//         //function type examples
//         //fn(f32, type) type
//         //fn(Light, u32) Ligth
//         //fn(GetSomeType(), @Interpolation) type
//         //function def examples

//         //if token is identifier then try to parse expression and see if its undecl identifier
//         //if so its add arg_name

//     }
// }
pub const Function = struct {
    arg_names: [][]const u8,
    type: tp.FunctionType,

    has_side_effects: bool,

    body: List(Expression),
    scope: Scope,
};

pub const Value = struct {
    type: Type,
    payload: ValuePayload = undefined,
    pub const format = debug.formatValue;
};

pub const ValuePayload = union {
    wide: u128,
    type: Type,
    ptr: *const anyopaque,
};

const ShouldStopFn = fn (*Parser, Token) Error!bool;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
pub const refine = ct.refine;
pub const Type = tp.Type;
pub const FunctionType = tp.FunctionType;
const UnaryOperator = Tokenizer.UnaryOperator;
pub const Token = Tokenizer.Token;
