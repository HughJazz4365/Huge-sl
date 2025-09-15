const std = @import("std");
const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");

tokenizer: *Tokenizer = undefined,
current_scope: *Scope = undefined,
arena: std.heap.ArenaAllocator = undefined,
global_scope: GloalScope = undefined,

const Error = error{
    OutOfMemory,

    UnexpectedToken,
    UndeclaredIdentifier,
};

pub fn init(self: *Parser, allocator: Allocator, tokenizer: *Tokenizer) void {
    self.tokenizer = tokenizer;
    self.arena = .init(allocator);
    self.global_scope = .new(self.arena.allocator());
}
pub fn parseStatement(self: *Parser) Error!?Statement {
    _ = self;
    //1.if token == return => .return
    //2.if token == break => .break
    //3.try parsing qualifier, if can => continue to parse var_decl
    //4. parse expr if next token is '='
    //its an assignment and we parse the assigned value
    //else expr is .ignored

    return null;
}

pub const Statement = union(enum) {
    var_decl: VariableDecl,
    //   [qualifier] [name] {:} {type expr} {=} {value expr}
    assignment: Assignment,
    //   [target expr] {binop} [=] [value expr]
    ignore: Expression,
    @"return": ?Expression,
    @"break": ?Expression,
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

pub fn parseExpression() Error!Expression {}
fn parseExpressionSide(self: *Parser) Error!Expression {
    _ = self;
    //var expr =
    //::::primary::::
    //if UnaryOperator => u_op
    //if name:
    //   if proceeded by bracket => fn call(parse args)
    //   else => if(identifier) is struct_type or ...
    //      comp_identifier
    //      else
    //      identifier
    //if '(' parse expression
    //if for parse for loop
    //if 'if', 'switch', 'while' - same
    //else try to parse value

    //expr =
    //::::secondary::::
    //(check proceeding tokens)
    //if swizzle => swizzle(expr)
    //if '[' => indexing into expr
    //if '.' =>  member access
    return .@"for";
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
    right: *Expression,
    op: BinaryOperator,
};

pub const GloalScope = struct {
    scope: Scope,
    var_list: List(Variable),
    allocator: Allocator,

    pub fn new(allocator: Allocator) @This() {
        var result: @This() = .{
            .allocator = allocator,
            .var_list = .empty,
            .scope = .{
                .addVarFn = &addVarFn,
                .declTypeFn = &declTypeFn,
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
    pub fn declTypeFn(scope: *Scope, name: []const u8) Error!tp.Type {
        return for (scope.vars) |v| {
            if (util.strEql(v.name, name)) break v.type;
        } else Error.UndeclaredIdentifier;
    }
    pub fn referenceFn(scope: *Scope, name: []const u8, ref_type: Scope.DeclReferenceType) Error!void {
        _ = &.{ name, ref_type, scope };
    }
};

pub const Scope = struct {
    addVarFn: *const fn (*Scope, Variable) Error!void,
    declTypeFn: *const fn (*Scope, []const u8) Error!tp.Type,
    referenceFn: *const fn (*Scope, []const u8, DeclReferenceType) Error!void,

    vars: []Variable = undefined,
    parent: *Scope = undefined,

    pub const DeclReferenceType = enum { track, untrack };
};
pub const Variable = struct {
    qualifier: void,
    name: []const u8,
    type: tp.Type,
};

pub const Value = struct {
    type: tp.Type,
    payload: ValuePayload,
};

pub const ValuePayload = union {
    wide: u128,
    any: *anyopaque,
};

const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
const UnaryOperator = Tokenizer.UnaryOperator;
