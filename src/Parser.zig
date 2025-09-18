const std = @import("std");
const debug = @import("debug.zig");
const bi = @import("builtin.zig");
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
    NoTypeVariable,
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

    const @"type": ?Type = blk: {
        const next = try self.tokenizer.peek();
        if (next != .@":") break :blk null;
        self.tokenizer.skip();

        const expr = try self.parseExpressionSide();
        break :blk self.asType(expr) orelse return Error.UnexpectedToken;
    };

    const value: ?Expression = blk: {
        const next = try self.tokenizer.peek();
        if (next != .@"=") break :blk null;
        self.tokenizer.skip();
        var expr = try self.parseExpression(defaultShouldStop);
        if (@"type") |tt| expr = try self.implicitCast(expr, tt);
        break :blk expr;
    };

    const variable: Variable = .{
        .qualifier = qualifier,
        .name = name_token.identifier,
        .type = @"type" orelse if (value) |v| self.typeOf(v) else return Error.NoTypeVariable,
    };

    return .{ .var_decl = .{
        .variable = variable,
        .value = value,
    } };
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
    var left = try self.parseExpressionSide();
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

        const right = try self.parseExpressionRecursive(should_stop, false, Tokenizer.bindingPower(op));
        const add_left = try self.createVal(left);
        //go right
        left = .{ .bin_op = .{
            .left = add_left,
            .right = try self.createVal(right),
            .op = op,
        } };

        token = try self.tokenizer.peek();
    }
    if (should_consume_end) self.tokenizer.skip();

    return left;
}

fn assignmentShouldStop(self: *Parser, token: Token) Error!bool {
    const peek = try self.tokenizer.peekTimes(2);
    return token == .@"=" or token == .bin_op and peek == .@"=" or self.defaultShouldStop(token) catch unreachable;
}

fn bracketShouldStop(self: *Parser, token: Token) !bool {
    _ = self;
    return token == .@")";
}

fn tupleShouldStop(self: *Parser, token: Token) !bool {
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
            //comp_identifier??
            break :blk .{ .identifier = name };
        },
        .@"(" => try self.parseExpression(bracketShouldStop),
        .@"." => switch (try self.tokenizer.next()) {
            .@"{" => blk: {
                //if '{' constructor( tuple of values not a struct constructor for now)
                var list: List(Expression) = .empty;
                var tkn = try self.tokenizer.peek();
                while (tkn != .@"}") {
                    const expr = try self.parseExpressionRecursive(tupleShouldStop, false, 0);
                    try list.append(self.arena.allocator(), expr);
                    if (try self.tokenizer.peek() == .@",") self.tokenizer.skip();
                    tkn = try self.tokenizer.peek();
                }
                self.tokenizer.skip();
                break :blk .{ .tuple = try list.toOwnedSlice(self.arena.allocator()) };
            },
            else => return Error.UnexpectedToken,

            //if name enum literal
        },
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
    pub inline fn addStatement(scope: *Scope, statement: Statement) Error!*const Statement {
        return try scope.addStatementFn(scope, statement);
    }
    fn addStatementFn(scope: *Scope, statement: Statement) Error!*const Statement {
        const entry_point: *EntryPoint = @fieldParentPtr("scope", scope);

        try entry_point.body.append(entry_point.allocator, statement);
        const ptr = &entry_point.body.items[entry_point.body.items.len - 1];

        return ptr;
    }
    pub fn getVarFn(scope: *Scope, name: []const u8) Error!*const Variable {
        // return for (scope.vars) |*v| {
        //     if (util.strEql(v.name, name)) break v;
        // } else Error.UndeclaredIdentifier;
        _ = scope;
        _ = name;
        const v: Variable = undefined;
        return &v;
    }
};

const ShaderStageInfo = union(tp.ShaderStage) {
    vertex,
    fragment,
    compute: [3]u32,
};
fn parseValue(self: *Parser, token: Token) Error!Value {
    _ = self;
    //true, false
    return switch (token) {
        .compint => |compint| .{ .type = .compint, .payload = .{ .wide = @bitCast(compint) } },
        .compfloat => |compfloat| .{ .type = .compfloat, .payload = .{ .wide = @bitCast(compfloat) } },
        .type_literal => |tl| .{ .type = .type, .payload = .{ .type = tl } },
        else => Error.UnexpectedToken,
    };
}
pub fn typeOf(self: *Parser, expr: Expression) Type {
    if (expr == .value) return expr.value.type;
    _ = self;
    return .compint;
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
    comp_identifier: []const u8,
    swizzle: Swizzle,
    //   [expr] [swizzle]
    member_access: MemberAccess,
    //   [expr] ['.'] [name]
    indexing: Indexing,
    //   [expr] ['['] [index] [']']
    tuple: []Expression,
    @"for",
    @"while",
    @"if",
    @"switch",

    value: Value, // D:
    pub fn isMutable(self: Expression) bool {
        return switch (self) {
            .identifier => true,
            .comp_identifier => true,
            .indexing => true,
            .member_access => true,
            .swizzle => true,
            else => false,
        };
    }
    pub const format = debug.formatExpression;
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
        _ = try addStatementFn(&self.scope, statement);
    }

    fn addStatementFn(scope: *Scope, statement: Statement) Error!*const Statement {
        const global_scope: *GlobalScope = @fieldParentPtr("scope", scope);
        try global_scope.body.append(global_scope.allocator, statement);
        return &global_scope.body.items[global_scope.body.items.len - 1];
    }
    fn getVarFn(scope: *Scope, name: []const u8) Error!*const Variable {
        // return for (scope.vars) |*v| {
        //     if (util.strEql(v.name, name)) break v;
        // } else Error.UndeclaredIdentifier;
        _ = scope;
        _ = name;
        const v: Variable = undefined;
        return &v;
    }
    fn referenceFn(scope: *Scope, name: []const u8, ref_type: Scope.DeclReferenceType) Error!void {
        _ = &.{ name, ref_type, scope };
    }
};

pub const Scope = struct {
    addStatementFn: *const fn (*Scope, Statement) Error!*const Statement = undefined,

    getVarFn: *const fn (*Scope, []const u8) Error!*const Variable = undefined,
    referenceFn: *const fn (*Scope, []const u8, DeclReferenceType) Error!void = undefined,

    parent: *Scope = undefined,

    pub const DeclReferenceType = enum { track, untrack };

    pub inline fn addStatement(self: *Scope, statement: Statement) Error!*const Statement {
        return try self.addStatementFn(self, statement);
    }

    pub inline fn getVar(self: *Scope, name: []const u8) Error!*const Variable {
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

pub fn implicitCast(self: *Parser, expr: Expression, @"type": Type) Error!Expression {
    const type_of = self.typeOf(expr);
    if (std.meta.eql(type_of, @"type")) return expr;
    switch (expr) {
        .value => |value| {
            if (@"type" == .compfloat) return if (value.type == .compint) .{ .value = .{
                .type = .compfloat,
                .payload = .{ .wide = @bitCast(@as(f128, @floatFromInt(@as(i128, @intCast(value.payload.wide))))) },
            } } else Error.CannotImplicitlyCast;
        },
        else => {},
    }
    return expr;
}

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
const Type = tp.Type;
const UnaryOperator = Tokenizer.UnaryOperator;
pub const Token = Tokenizer.Token;
