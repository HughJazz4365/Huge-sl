const std = @import("std");
const debug = @import("debug.zig");
const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");

tokenizer: *Tokenizer = undefined,

allocator: Allocator = undefined,
arena: std.heap.ArenaAllocator = undefined,

global_scope: GloalScope = undefined,
current_scope: *Scope = undefined,

expr_buf: List(Expression) = undefined,

const Error = error{
    OutOfMemory,

    UnexpectedToken,
    UndeclaredIdentifier,
} || Tokenizer.Error;

pub fn testicle(self: *Parser) Error!void {
    const expr = try self.parseExpression({});
    debug.print(expr);
}
pub fn init(self: *Parser, allocator: Allocator, tokenizer: *Tokenizer) void {
    self.allocator = allocator;
    self.tokenizer = tokenizer;
    self.arena = .init(allocator);
    self.global_scope = .new(self.arena.allocator());
    self.expr_buf = .empty;
}
pub fn create(self: *Parser, value: anytype) *@TypeOf(value) {
    const T = @TypeOf(value);
    return switch (T) {
        Expression => blk: {
            try self.expr_buf.append(self.allocator, value);
            break :blk &self.expr_buf.items[self.expr_buf.items.len - 1];
        },
        else => @compileError("cant create " ++ @typeName(T)),
    };
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

pub fn parseExpression(self: *Parser, stop_at: void) Error!Expression {
    return try self.parseExpressionRecursive(stop_at, comptime blk: {
        const enum_info = @typeInfo(BinaryOperator).@"enum";
        var min_op: BinaryOperator = @enumFromInt(enum_info.fields[0].value);
        break :blk for (enum_info.fields[1..]) |ef| {
            if (Tokenizer.bindingPower(min_op) > Tokenizer.bindingPower(@as(BinaryOperator, @enumFromInt(ef.value)))) min_op = @as(BinaryOperator, @enumFromInt(ef.value));
        } else min_op;
    });
}
pub fn parseExpressionRecursive(self: *Parser, stop_at: void, last_op: BinaryOperator) Error!Expression {
    var left = try self.parseExpressionSide();
    while (true) {
        const token = try self.tokenizer.next();
        if (token != .bin_op) return left;

        const op = token.bin_op;
        const right = try self.parseExpressionRecursive(stop_at, op);
        const add_left = try 
        left = .{ .bin_op = .{
            .left = left,
            .op = op,
            .right = right,
        } };
        if (Tokenizer.bindingPower(op) >= Tokenizer.bindingPower(last_op)) return left;
    }
    return left;
}
fn parseExpressionSide(self: *Parser) Error!Expression {
    switch (try self.tokenizer.next()) {
        .u_op => |u_op| return .{ .u_op = .{
            .op = u_op,
            .target = try self.parseExpressionSide(),
        } },
        .compfloat, .compint => return .@"if",
        .eof, .endl => {},
        else => return Error.UnexpectedToken,
    }
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
    op: BinaryOperator,
    right: *Expression,
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
