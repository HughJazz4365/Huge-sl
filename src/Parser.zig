const std = @import("std");
const debug = @import("debug.zig");
const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");

tokenizer: *Tokenizer = undefined,

allocator: Allocator = undefined,
arena: std.heap.ArenaAllocator = undefined,

global_scope: GlobalScope = undefined,
current_scope: *Scope = undefined,

const Error = error{
    OutOfMemory,

    UnexpectedToken,
    UndeclaredIdentifier,
} || Tokenizer.Error;

pub fn testicle(self: *Parser) Error!void {
    const statement = (try self.parseStatement()).?;
    std.debug.print("[S]: {f}\n", .{statement});
    // const expr = try self.parseExpression(defaultShouldStop);
    // std.debug.print("[E]: {f}\n", .{expr});
}
pub fn init(self: *Parser, allocator: Allocator, tokenizer: *Tokenizer) void {
    self.allocator = allocator;
    self.tokenizer = tokenizer;
    self.arena = .init(allocator);
    self.global_scope = .new(self.arena.allocator());
    self.current_scope = &self.global_scope.scope;
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
    var token = try self.tokenizer.next();
    while (token == .endl) token = try self.tokenizer.next();

    return switch (token) {
        .eof => null,
        .@"const", .@"var", .uniform, .property, .shared, .out, .in => try self.parseVariableDecl(token),

        else => Error.UnexpectedToken,
    };
    //1.if token == return => .return
    //2.if token == break => .break
    //3.try parsing qualifier, if can => continue to parse var_decl
    //4. parse expr if next token is '='
    //its an assignment and we parse the assigned value
    //else expr is .ignored

}
fn parseVariableDecl(self: *Parser, token: Token) Error!Statement {
    _ = token;
    const name = (try self.tokenizer.next()).identifier;
    const t: tp.Type = .compfloat;
    _ = try self.tokenizer.next();
    const value = try self.parseExpression(defaultShouldStop);

    const variable: Variable = .{
        .qualifier = {},
        .name = name,
        .type = t,
    };
    try self.current_scope.addVar(variable);
    const add_var = try self.current_scope.getVar(variable.name);
    return .{ .var_decl = .{ .variable = add_var, .value = value } };
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
    left: Expression,
    right: Expression,
    modifier: ?BinaryOperator,
};
pub const VariableDecl = struct {
    variable: *Variable,
    value: ?Expression,
};

pub fn parseExpression(self: *Parser, should_stop: ShouldStopFn) Error!Expression {
    return try self.parseExpressionRecursive(should_stop, true, 0);
}
pub fn parseExpressionRecursive(self: *Parser, should_stop: ShouldStopFn, should_consume_end: bool, last_bp: u8) Error!Expression {
    var left = try self.parseExpressionSide();
    var token = try self.tokenizer.peek();
    while (!should_stop(self, token)) {
        if (token != .bin_op) return Error.UnexpectedToken;
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

fn bracketShouldStop(self: *Parser, token: Token) bool {
    _ = self;
    return token == .@")";
}

fn defaultShouldStop(self: *Parser, token: Token) bool {
    _ = self;
    return token == .endl or token == .eof;
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
        // .compfloat, .compint => .@"if",
        else => .{ .val = try self.parseValue(token) },
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
fn parseValue(self: *Parser, token: Token) Error!Value {
    _ = self;
    //true, false
    return switch (token) {
        .compint => |compint| .{ .type = .compint, .payload = .{ .wide = @bitCast(compint) } },
        .compfloat => |compfloat| .{ .type = .compfloat, .payload = .{ .wide = @bitCast(compfloat) } },
        else => Error.UnexpectedToken,
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
    @"for",
    @"while",
    @"if",
    @"switch",

    val: Value, // D:
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
    var_list: List(Variable),
    allocator: Allocator,

    pub fn new(allocator: Allocator) @This() {
        var result: @This() = .{
            .allocator = allocator,
            .var_list = .empty,
            .scope = .{
                .addVarFn = &addVarFn,
                .getVarFn = &getVarFn,
                .referenceFn = &referenceFn,
            },
        };
        result.scope.vars = result.var_list.items;
        return result;
    }
    pub fn addVarFn(scope: *Scope, variable: Variable) Error!void {
        const global: *@This() = @fieldParentPtr("scope", scope);
        try global.var_list.append(global.allocator, variable);
        global.scope.vars = global.var_list.items;
    }
    pub fn getVarFn(scope: *Scope, name: []const u8) Error!*Variable {
        return for (scope.vars) |*v| {
            if (util.strEql(v.name, name)) break v;
        } else Error.UndeclaredIdentifier;
    }
    pub fn referenceFn(scope: *Scope, name: []const u8, ref_type: Scope.DeclReferenceType) Error!void {
        _ = &.{ name, ref_type, scope };
    }
};

pub const Scope = struct {
    addVarFn: *const fn (*Scope, Variable) Error!void,
    getVarFn: *const fn (*Scope, []const u8) Error!*Variable,
    referenceFn: *const fn (*Scope, []const u8, DeclReferenceType) Error!void,

    vars: []Variable = undefined,
    parent: *Scope = undefined,

    pub const DeclReferenceType = enum { track, untrack };

    pub fn addVar(self: *Scope, variable: Variable) Error!void {
        try self.addVarFn(self, variable);
    }

    pub fn getVar(self: *Scope, name: []const u8) Error!*Variable {
        return try self.getVarFn(self, name);
    }

    pub fn reference(self: *Scope, name: []const u8, ref_type: DeclReferenceType) Error!void {
        try self.referenceFn(self, name, ref_type);
    }
};
pub const Variable = struct {
    qualifier: void,
    name: []const u8,
    type: tp.Type,
};

pub const Value = struct {
    type: tp.Type,
    payload: ValuePayload,
    pub const format = debug.formatValue;
};

pub const ValuePayload = union {
    wide: u128,
    any: *anyopaque,
};

const ShouldStopFn = fn (*Parser, Token) bool;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
const UnaryOperator = Tokenizer.UnaryOperator;
pub const Token = Tokenizer.Token;
