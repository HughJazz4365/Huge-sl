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

pub const Error = error{
    OutOfMemory,

    UnexpectedToken,
    UndeclaredIdentifier,

    CannotImplicitlyCast,
    CannotInferType,
    NoTypeVariable,

    InvalidOperands,
    InvalidConstructor,

    NumericError,
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
pub fn createVal(self: *Parser, value: anytype) !*@TypeOf(value) {
    const ptr = try self.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}
pub fn create(self: *Parser, T: type) !*T {
    return try self.arena.allocator().create(T);
}

pub fn parseStatement(self: *Parser) Error!?Statement {
    var token = try self.tokenizer.peek();
    while (token == .endl) {
        self.tokenizer.skip();
        token = try self.tokenizer.peek();
    }

    const statement: ?Statement = switch (token) {
        .eof => null,
        .@"const", .@"var", .uniform, .property, .shared, .out, .in => blk: {
            self.tokenizer.skip();
            break :blk try self.parseVariableDecl(token);
        },

        else => blk: {
            const target = try self.parseExpressionRecursive(assignmentShouldStop, false, 0);
            const peek = try self.tokenizer.peek();
            if (self.defaultShouldStop(peek) catch unreachable) break :blk Statement{ .ignore = target };

            const modifier: ?BinaryOperator = if (peek == .bin_op) op: {
                self.tokenizer.skip();
                break :op peek.bin_op;
            } else null;

            self.tokenizer.skip(); //should always be '='

            break :blk Statement{ .assignment = .{
                .target = target,
                .value = try self.parseExpression(defaultShouldStop),
                .modifier = modifier,
            } };
        },
    };
    return statement;
    //1.if token == return => .return
    //2.if token == break => .break
    //3.try parsing qualifier, if can => continue to parse var_decl
    //4. parse expr if next token is '='
    //its an assignment and we parse the assigned value
    //else expr is .ignored

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
            @"type" = self.typeOf(expr);

        if (!@"type".isComplete()) {
            expr = try self.makeExprCompleteType(expr);
            @"type" = self.typeOf(expr);
            if (!@"type".isComplete()) return Error.CannotInferType;
        }
        break :blk expr;
    };

    const variable: Variable = .{
        .qualifier = qualifier,
        .name = name_token.identifier,
        .type = @"type",
    };

    return .{ .var_decl = .{
        .variable = variable,
        .value = value,
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
    modifier: ?BinaryOperator,
};
pub const VariableDecl = struct {
    variable: Variable,
    value: ?Expression,
};

pub fn parseExpression(self: *Parser, should_stop: ShouldStopFn) Error!Expression {
    return try self.parseExpressionRecursive(should_stop, true, 0);
}
pub fn parseExpressionRecursive(self: *Parser, should_stop: ShouldStopFn, should_consume_end: bool, last_bp: u8) Error!Expression {
    var left = try self.simplify(try self.parseExpressionSide());
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

        const right = try self.simplify(try self.parseExpressionRecursive(should_stop, false, Tokenizer.bindingPower(op)));
        const add_left = try self.createVal(left);
        //go right
        left = try self.simplify(.{ .bin_op = .{
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
    _ = &expr;

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
        const expr = try self.parseExpressionRecursive(constructorShouldStop, false, 0);
        try list.append(self.arena.allocator(), expr);
        if (try self.tokenizer.peek() == .@",") self.tokenizer.skip();
        token = try self.tokenizer.peek();
    }
    self.tokenizer.skip();

    if (list.items.len == 0) return Error.InvalidConstructor;
    //check if its a valid constructor
    //if(@"type" != .unknown)

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
        _ = try self.current_scope.addStatement(statement);
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
                .getVarFn = &getVarFn,
                .addStatementFn = &addStatementFn,
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
    pub fn getVarFn(scope: *Scope, name: []const u8) Error!Variable {
        // return for (scope.vars) |*v| {
        //     if (util.strEql(v.name, name)) break v;
        // } else Error.UndeclaredIdentifier;
        _ = scope;
        _ = name;
        const v: Variable = undefined;
        return v;
    }
};

const ShaderStageInfo = union(tp.ShaderStage) {
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
pub fn typeOf(self: *Parser, expr: Expression) Type {
    _ = self;
    return switch (expr) {
        .value => |value| value.type,
        .constructor => |constructor| constructor.type,
        else => .compint,
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
    cast: Cast,
    constructor: Constructor,
    @"for",
    @"while",
    @"if",
    @"switch",

    value: Value,
    pub fn isMutable(self: Expression) bool {
        return switch (self) {
            .identifier => true,
            .indexing => true,
            .member_access => true,
            .swizzle => true,
            else => false,
        };
    }
    pub const format = debug.formatExpression;
};
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
                .getVarFn = &getVarFn,
                .referenceFn = &referenceFn,
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
    fn getVarFn(scope: *Scope, name: []const u8) Error!Variable {
        // return for (scope.vars) |*v| {
        //     if (util.strEql(v.name, name)) break v;
        // } else Error.UndeclaredIdentifier;
        _ = scope;
        _ = name;
        const v: Variable = undefined;
        return v;
    }
    fn referenceFn(scope: *Scope, name: []const u8, ref_type: Scope.DeclReferenceType) Error!void {
        _ = &.{ name, ref_type, scope };
    }
};

pub const Scope = struct {
    addStatementFn: *const fn (*Scope, Statement) Error!void = undefined,

    getVarFn: *const fn (*Scope, []const u8) Error!Variable = undefined,
    referenceFn: *const fn (*Scope, []const u8, DeclReferenceType) Error!void = undefined,

    parent: *Scope = undefined,

    pub const DeclReferenceType = enum { track, untrack };

    pub inline fn addStatement(self: *Scope, statement: Statement) Error!void {
        try self.addStatementFn(self, statement);
    }

    pub inline fn getVar(self: *Scope, name: []const u8) Error!Variable {
        return try self.getVarFn(self, name);
    }

    pub inline fn reference(self: *Scope, name: []const u8, ref_type: DeclReferenceType) Error!void {
        try self.referenceFn(self, name, ref_type);
    }
};
pub const Variable = struct {
    qualifier: Qualifier,
    name: []const u8,
    type: Type,
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
};

pub fn explicitCast(self: *Parser, expr: Expression, @"type": Type) Error!Expression {
    return try self.implicitCast(expr, @"type");
}
pub fn implicitCast(self: *Parser, expr: Expression, @"type": Type) Error!Expression {
    const type_of = self.typeOf(expr);
    if (std.meta.eql(type_of, @"type")) return expr;
    return switch (expr) {
        .value => |value| .{ .value = try ct.implicitCastValue(value, @"type", true) },
        // .constructor => |constructor| try self.implicitCastConstructor(constructor, @"type"),
        else => expr,
    };
}
// fn implicitCastConstructor(self: *Parser, constructor: Constructor, @"type": Type) Error!Expression {
//     if (constructor.type == @"type") return .{ .constructor = constructor };
//     if (constructor.type != .unknown) return Error.CannotImplicitlyCast;

//     const target_structure = @"type".constructorStructure();
//     if (constructor.components.len) return Error.CannotImplicitlyCast;

//     const slice = if (constructor.components.len == target_structure.len)
//         constructor.components
//     else
//         try self.arena.allocator().alloc(Expression, target_structure.len);

//     return .{ .constructor = .{ .type = @"type", .components = constructor.components } };
// }
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
    payload: ValuePayload,
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
const simplify = ct.simplify;
const Type = tp.Type;
const UnaryOperator = Tokenizer.UnaryOperator;
pub const Token = Tokenizer.Token;
