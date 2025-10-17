const std = @import("std");
const debug = @import("debug.zig");
const bi = @import("builtin.zig");
const ct = @import("comptime.zig");
const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");

pub const CF = f64;
pub const CI = i128;
pub const WIDE = u128;

tokenizer: *Tokenizer = undefined,

allocator: Allocator = undefined,
arena: std.heap.ArenaAllocator = undefined,

global_scope: GlobalScope = undefined,
current_scope: *Scope = undefined,

intermediate_value_index: u32 = 0,

pub const Error = error{
    OutOfMemory,

    UnexpectedToken,

    RecursionNotSupported,
    UnclosedScope,
    IncompleteStatement,
    MissingFunctionBody,

    VariableRedeclaration,
    MissingInitializer,
    RepeatingArgumentNames,
    VariableTypeAndQualifierDontMatch,
    UnexpectedInitializer,

    MutatingImmutableVariable,
    UndeclaredVariable,
    InvalidCall,
    InvalidBuiltin,
    NonMatchingArgumentCount,

    CannotImplicitlyCast,
    CannotExplicitlyCast,
    CannotInferType,

    InvalidOperands,
    InvalidUnaryOperationTarget,
    InvalidConstructor,
    InvalidIndex,

    NumericError,
    OutOfBoundsAccess,
    NegativePower, //a ^ (- |r|)
    InvalidBitshift,
} || Tokenizer.Error;
///creates its own arena
pub fn parse(allocator: Allocator, tokenizer: *Tokenizer) Error!Parser {
    var self: Parser = .{};
    self.allocator = allocator;
    self.tokenizer = tokenizer;
    self.arena = .init(allocator);
    self.global_scope = .new();
    self.current_scope = &self.global_scope.scope;

    while (try self.parseStatement()) |statement| {
        try self.global_scope.addStatement(&self, statement);
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
        .initializer = expr,
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
    try self.current_scope.addStatement(self, statement);
}
pub fn isStatementComplete(self: *Parser, statement: Statement) Error!bool {
    return switch (statement) {
        .var_decl => |var_decl| (Type.eql(var_decl.type, try self.typeOf(var_decl.initializer)) or
            var_decl.initializer.isEmpty()) and
            var_decl.type != .unknown,
        else => true,
    };
}

pub fn parseStatement(self: *Parser) Error!?Statement {
    var token = try self.tokenizer.peek();
    while (token == .endl) {
        self.tokenizer.skip();
        token = try self.tokenizer.peek();
    }
    if (self.defaultShouldStop(token) catch unreachable) {
        self.tokenizer.skip();
        return null;
    }

    return switch (token) {
        .eof => null,
        .@"const", .mut, .uniform, .push, .shared, .out, .in => blk: {
            self.tokenizer.skip();
            break :blk .{ .var_decl = try self.parseVarDecl(token) };
        },

        .@"return" => blk: {
            self.tokenizer.skip();
            const peek = try self.tokenizer.peek();
            if (self.defaultShouldStop(peek) catch unreachable) {
                if (peek != .@"}") self.tokenizer.skip();
                break :blk .{ .@"return" = .empty };
            }
            break :blk .{ .@"return" = try self.implicitCast(
                try self.parseExpression(defaultShouldStop),
                self.current_scope.result_type,
            ) };
        },

        else => try self.parseAssignmentOrIgnore(),
    };
}
fn parseAssignmentOrIgnore(self: *Parser) Error!Statement {
    const target = try self.parseExpressionRecursive(assignmentShouldStop, false, 0);
    const peek = try self.tokenizer.peek();
    if (self.defaultShouldStop(peek) catch unreachable) return .{ .ignore = target };
    if (!try self.isExprMutable(target)) return self.errorOutFmt(Error.MutatingImmutableVariable, "Trying to mutate unmutable variable: {f}", .{target});

    const modifier: ?BinaryOperator = if (peek == .bin_op) op: {
        self.tokenizer.skip();
        break :op peek.bin_op;
    } else null;
    self.tokenizer.skip(); //will always be '=' since we checked for it in 'shouldStop'

    var value = try self.parseExpression(defaultShouldStop);

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
fn parseVarDecl(self: *Parser, token: Token) Error!VariableDeclaration {
    const qualifier = try self.parseQualifier(token);

    const name_token = try self.tokenizer.next();
    if (name_token != .identifier)
        return self.errorOutFmt(Error.UnexpectedToken, "Expected variable name after qualifier", .{});

    blk: {
        _ = self.current_scope.getVariableReference(self, name_token.identifier) catch break :blk;
        return self.errorOutFmt(Error.VariableRedeclaration, "Variable redeclaration: '{s}'", .{name_token.identifier});
    }

    var @"type": Type = .unknownempty;

    //type is explicitly specified
    if (try self.tokenizer.peek() == .@":") {
        self.tokenizer.skip();

        @"type" = try self.asTypeCreate(try self.implicitCast(
            try self.refine(try self.parseExpressionSide()),
            .type,
        ));
    }

    const initializer_expr: Expression = switch (try self.tokenizer.peek()) {
        .endl, .eof => if (qualifier == .@"const")
            return self.errorOutDefault(Error.MissingInitializer)
        else
            .empty,
        .@"=" => blk: {
            self.tokenizer.skip();

            if (!qualifier.canHaveInitializer())
                return self.errorOutFmt(Error.UnexpectedInitializer, "Variable with \'{s}\' qualifier cant have initializers", .{@tagName(qualifier)});

            const expr = try self.implicitCast(try self.parseExpression(defaultShouldStop), @"type");
            if (@"type".isEmpty()) @"type" = try self.typeOf(expr);
            break :blk expr;
        },
        else => return self.errorOut(Error.UnexpectedToken),
    };

    try self.matchVariableTypeWithQualifier(@"type", qualifier);
    return .{
        .qualifier = qualifier,
        .name = name_token.identifier,
        .type = @"type",
        .initializer = initializer_expr,
    };
}
fn matchVariableTypeWithQualifier(self: *Parser, @"type": Type, qualifier: Qualifier) Error!void {
    switch (@"type") {
        // .buffer => onply uniform etc
        .entrypoint, .function => if (qualifier != .@"const") return self.errorOutFmt(
            Error.VariableTypeAndQualifierDontMatch,
            "Functions and entry points must only be 'const' and not '{s}'",
            .{@tagName(qualifier)},
        ),
        else => {},
    }
}

pub const Statement = union(enum) {
    var_decl: VariableDeclaration,
    //   [qualifier] [name] {:} {type expr} {=} {value expr}
    assignment: Assignment,
    //   [target expr] {binop} [=] [value expr]
    ignore: Expression,
    @"return": Expression,
    // @"break": ?Expression,
    pub const format = debug.formatStatement;
};
pub const Assignment = struct {
    target: Expression,
    value: Expression,
};
pub const VariableDeclaration = struct {
    qualifier: Qualifier,
    name: []const u8,
    type: Type,
    initializer: Expression,
    reference_count: u32 = 0,

    pub fn variableReference(self: *VariableDeclaration) VariableReference {
        return .{
            .is_mutable = self.qualifier.isMutable(),
            .type = self.type,
            .value = if (self.initializer.isEmpty() or self.initializer != .value) .{ .identifier = self.name } else self.initializer,
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
            return self.errorOut(Error.UnexpectedToken);
        }

        const op = token.bin_op;
        //go left
        if (Tokenizer.bindingPower(op) <= last_bp) break;

        self.tokenizer.skip();

        const right = try self.parseExpressionRecursive(should_stop, false, Tokenizer.bindingPower(op));
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
    return token == .@"=" or token == .bin_op and peek == .@"=" or self.defaultShouldStop(token) catch unreachable;
}

fn bracketShouldStop(_: *Parser, token: Token) !bool {
    return token == .@")";
}
fn argDeclShouldStop(_: *Parser, token: Token) !bool {
    return token == .@")" or token == .@"," or token == .@":";
}
fn argShouldStop(_: *Parser, token: Token) !bool {
    return token == .@")" or token == .@",";
}
fn constructorShouldStop(_: *Parser, token: Token) !bool {
    return token == .@"}" or token == .@",";
}

fn defaultShouldStop(self: *Parser, token: Token) !bool {
    return token == .endl or token == .eof or (token == .@"}" and @intFromPtr(self.current_scope) != @intFromPtr(&self.global_scope.scope));
}

fn parseExpressionSide(self: *Parser) Error!Expression {
    var expr = try self.refine(try self.parseExpressionSidePrimary());
    var peek: Token = undefined;
    while (true) {
        //parseConstructorFunction that will handle all constructrors
        peek = try self.tokenizer.peek();
        if (peek == .@"{") { //constructor
            if (try self.typeOf(expr) == .type) {
                self.tokenizer.skip();
                expr = try self.parseCastOrConstructor(try self.asTypeCreate(expr));
                continue;
            } else return self.errorOut(Error.UnexpectedToken);
        }
        if (peek == .@"(") { //call
            self.tokenizer.skip();

            expr = try self.refine(.{ .call = try self.parseFunctionCall(expr) });
            continue;
        }
        return expr;
    }

    //::::secondary::::
    //(check proceeding tokens)
    //if swizzle => swizzle(expr)
    //if '[' => indexing into expr
    //if '(' => function call
    //if '.' =>  member access

}
fn parseExpressionSidePrimary(self: *Parser) Error!Expression {
    const token = try self.tokenizer.next();
    return switch (token) {
        .u_op => |u_op| .{ .u_op = .{
            .op = u_op,
            .target = try self.createVal(try self.refine(try self.parseExpressionSide())),
        } },
        .identifier => |id| .{ .identifier = id },
        .builtin => |builtin| .{ .builtin = try bi.getBuiltin(builtin) },
        .@"(" => try self.parseExpression(bracketShouldStop),
        .@"." => switch (try self.tokenizer.next()) {
            .@"{" => try self.refine(try self.parseUnknownConstructor()),
            else => return self.errorOut(Error.UnexpectedToken),
        },
        .@"fn" => try self.parseFunctionTypeOrValue(),
        .entrypoint => try self.parseEntryPointTypeOrValue(),
        // .compfloat, .compint => .@"if",
        else => .{ .value = try self.parseValue(token) },
    };
}
fn parseFunctionCall(self: *Parser, callee: Expression) Error!Call {
    const @"type" = try self.typeOf(callee);

    const callee_ptr = try self.createVal(callee);
    const args = try self.parseExpressionSequence(.@")");

    if (@"type" == .unknown) return .{ .callee = callee_ptr, .args = args };
    if (@"type" != .function) return self.errorOut(Error.InvalidCall);
    const ft = @"type".function;

    if (ft.arg_types.len != args.len) return self.errorOut(Error.NonMatchingArgumentCount);
    for (ft.arg_types, args) |t, *arg| arg.* = try self.implicitCast(arg.*, t);
    return .{ .callee = callee_ptr, .args = args };
}

fn parseUnknownConstructor(self: *Parser) Error!Expression {
    const components = try self.parseExpressionSequence(.@"}"); /////////////
    return if (components.len == 1) .{ .cast = .{
        .type = .unknownempty,
        .expr = &components[0],
    } } else .{ .constructor = .{ .type = .unknownempty, .components = components } };
}
fn parseCastOrConstructor(self: *Parser, @"type": Type) Error!Expression {
    const components = try self.parseExpressionSequence(.@"}");

    if (components.len == 0) return self.errorOut(Error.InvalidConstructor);

    return if (components.len == 1) .{ .cast = .{
        .type = @"type",
        .expr = &components[0],
    } } else .{ .constructor = .{ .type = @"type", .components = components } };
}
fn parseExpressionSequence(self: *Parser, comptime until: Token) Error![]Expression {
    const should_stop = struct {
        pub fn f(_: *Parser, token: Token) Error!bool {
            return std.meta.eql(token, until) or token == .@",";
        }
    }.f;
    var list: List(Expression) = .empty;

    var token = try self.tokenizer.peek();
    while (!std.meta.eql(token, until)) {
        const expr = try self.parseExpressionRecursive(should_stop, false, 0);
        try list.append(self.arena.allocator(), expr);
        if (try self.tokenizer.peek() == .@",") self.tokenizer.skip();
        token = try self.tokenizer.peek();
    }
    self.tokenizer.skip();
    return try list.toOwnedSlice(self.arena.allocator());
}

fn parseFunctionTypeOrValue(self: *Parser) Error!Expression {
    var mode: enum { unsure, type, value } = .value;
    var peek = try self.tokenizer.peek();
    //fn <==> fn()void
    if (try defaultShouldStop(self, peek)) return .{ .value = .{
        .type = .type,
        .payload = .{ .type = .{ .function = .{
            .rtype = @constCast(&(Type{ .void = {} })),
            .arg_types = &.{},
        } } },
    } };

    var arg_types: List(Type) = .empty;
    var set_index: ?usize = null;
    var arg_names: List([]const u8) = .empty;
    if (peek == .@"(") {
        self.tokenizer.skip();
        peek = try self.tokenizer.peek();

        defer self.tokenizer.skip();
        while (peek != .@")") {
            switch (mode) {
                .type => {
                    const type_expr = try self.parseExpression(argDeclShouldStop);
                    try arg_types.append(self.arena.allocator(), try self.asTypeCreate(type_expr));
                },
                .value => {
                    if (peek != .identifier) return self.errorOut(Error.UnexpectedToken);
                    const name = peek.identifier;
                    self.tokenizer.skip();

                    for (arg_names.items) |past_name| if (util.strEql(name, past_name)) return self.errorOut(Error.RepeatingArgumentNames);
                    blk: {
                        _ = self.global_scope.getVariableReference(self, name) catch break :blk;
                        return self.errorOutFmt(Error.VariableRedeclaration, "Function argument name '{s}' aliases with a global variable", .{name});
                    }
                    const index = arg_types.items.len;
                    var @"type": Type = .unknownempty;
                    if (try self.tokenizer.peek() == .@":") {
                        self.tokenizer.skip();

                        if (try self.tokenizer.peek() == .@"anytype") {
                            self.tokenizer.skip();
                            set_index = index;
                        } else {
                            const type_expr = try self.parseExpressionRecursive(argShouldStop, false, 0);
                            @"type" = try self.asTypeCreate(type_expr);
                            if (arg_types.items.len > 0) for (arg_types.items[(if (set_index) |si| si + 1 else 0)..]) |*t| {
                                t.* = @"type";
                            };
                            set_index = index;
                        }
                    }
                    try arg_types.append(self.arena.allocator(), @"type");
                    try arg_names.append(self.arena.allocator(), name);
                },
                .unsure => _ = &mode,
            }
            const peek_end = try self.tokenizer.peek();
            switch (peek_end) {
                .@")" => {},
                .@"," => self.tokenizer.skip(),
                else => return self.errorUnexpectedToken(peek_end),
            }
            peek = try self.tokenizer.peek();
        }
    }
    const rtype = try self.asTypeCreate(try self.refine(try self.parseExpressionSidePrimary()));

    if (arg_types.items.len > 0 and if (set_index) |si| si < arg_types.items.len - 1 else true) for (arg_types.items[if (set_index) |si| si + 1 else 0..]) |*t| {
        t.* = rtype;
    };
    //determine rtype
    const ftype: tp.FunctionType = .{
        .rtype = try self.createVal(rtype),
        .arg_types = try arg_types.toOwnedSlice(self.arena.allocator()),
    };
    if (mode == .unsure) @panic("UNSURE FUNC");
    if (mode == .type) {
        return .{ .value = .{
            .type = .type,
            .payload = .{ .type = .{ .function = ftype } },
        } };
    } else {
        var ptr: *Function = try self.createVal(Function.new(self, ftype, try arg_names.toOwnedSlice(self.arena.allocator())));
        if (try self.tokenizer.peek() != .@"{") return self.errorOut(Error.MissingFunctionBody);
        self.tokenizer.skip();

        try self.parseScope(&ptr.scope);

        return .{ .value = .{
            .type = .{ .function = ftype },
            .payload = .{ .ptr = @ptrCast(@alignCast(ptr)) },
        } };
    }
}
fn parseEntryPointTypeOrValue(self: *Parser) Error!Expression {
    if (try self.tokenizer.next() != .@"(") return self.errorOut(Error.UnexpectedToken);
    var token = try self.tokenizer.next();
    while (token != .@")") token = try self.tokenizer.next();

    const stage_info: ShaderStageInfo = .fragment;
    const ep_type: Type = .{ .entrypoint = std.meta.activeTag(stage_info) };

    if (try self.tokenizer.peek() != .@"{") return ep_type.asExpr();
    self.tokenizer.skip();

    var entry_point: EntryPoint = .new(self, stage_info);
    try self.parseScope(&entry_point.scope);

    return .{ .value = .{
        .type = ep_type,
        .payload = .{ .ptr = @ptrCast(@alignCast(try self.createVal(entry_point))) },
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
    } else return self.errorOut(Error.UnclosedScope);
}

pub const Function = struct {
    arg_names: [][]const u8,
    type: tp.FunctionType,

    //not dependant on nor mutates any global variables
    is_pure: bool = true,

    body: List(Statement) = .empty,
    scope: Scope,

    pub fn new(self: *Parser, ftype: tp.FunctionType, arg_names: [][]const u8) Function {
        return .{
            .type = ftype,
            .arg_names = arg_names,
            .body = .empty,
            .scope = .{
                .getVariableReferenceFn = &getVariableReferenceFn,
                .addStatementFn = &addStatementFn,
                .trackReferenceFn = &trackReferenceFn,

                .parent = self.current_scope,
                .result_type = ftype.rtype.*,
            },
        };
    }
    fn addStatementFn(scope: *Scope, parser: *Parser, statement: Statement) Error!void {
        const function: *Function = @fieldParentPtr("scope", scope);
        if (!(try parser.isStatementComplete(statement))) return parser.errorOutFmt(
            Error.IncompleteStatement,
            "Statement debug representation:\n {f}",
            .{statement},
        );
        try function.body.append(parser.arena.allocator(), statement);
    }
    fn trackReferenceFn(scope: *Scope, name: []const u8, ref_type: Scope.DeclReferenceType) Error!void {
        const function: *Function = @fieldParentPtr("scope", scope);
        for (function.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name)) {
                statement.var_decl.reference_count = if (ref_type == .track) statement.var_decl.reference_count + 1 else statement.var_decl.reference_count -| 1;
                return;
            }
        } else for (function.arg_names) |n| if (util.strEql(n, name)) return;
        return Error.UndeclaredVariable;
    }
    fn getVariableReferenceFn(scope: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
        const function: *Function = @fieldParentPtr("scope", scope);
        return for (function.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name))
                break statement.var_decl.variableReference();
        } else for (function.type.arg_types, function.arg_names) |t, n| {
            if (util.strEql(name, n)) break .{
                .is_mutable = false,
                .type = t,
                .value = .{ .identifier = name },
            };
        } else try parser.global_scope.getVariableReference(parser, name);
    }
};

pub const EntryPoint = struct {
    stage_info: ShaderStageInfo,
    scope: Scope,

    body: List(Statement),

    pub fn new(self: *Parser, stage_info: ShaderStageInfo) EntryPoint {
        return .{
            .stage_info = stage_info,
            .body = .empty,
            .scope = .{
                .getVariableReferenceFn = &getVariableReferenceFn,
                .addStatementFn = &addStatementFn,
                .trackReferenceFn = &trackReferenceFn,

                .parent = self.current_scope,
            },
        };
    }
    pub fn addStatement(scope: *Scope, parser: *Parser, statement: Statement) Error!void {
        try scope.addStatementFn(scope, parser, statement);
    }
    fn addStatementFn(scope: *Scope, parser: *Parser, statement: Statement) Error!void {
        const entry_point: *EntryPoint = @fieldParentPtr("scope", scope);
        if (!(try parser.isStatementComplete(statement))) return parser.errorOutFmt(
            Error.IncompleteStatement,
            "Statement debug representation:\n {f}",
            .{statement},
        );
        try entry_point.body.append(parser.arena.allocator(), statement);
    }
    pub fn getVariableReferenceFn(scope: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
        const entry_point: *EntryPoint = @fieldParentPtr("scope", scope);
        return for (entry_point.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name))
                break statement.var_decl.variableReference();
        } else try parser.global_scope.scope.getVariableReference(parser, name);
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

pub const ShaderStageInfo = union(tp.ExecutionModel) {
    vertex,
    fragment,
    compute: [3]u32,
};
fn parseValue(self: *Parser, token: Token) Error!Value {
    //true, false
    _ = self;
    return switch (token) {
        .compint => |compint| .{ .type = .compint, .payload = .{ .wide = util.fit(WIDE, compint) } },
        .compfloat => |compfloat| .{ .type = .compfloat, .payload = .{ .wide = util.fit(WIDE, compfloat) } },
        .type_literal => |tl| .{ .type = .type, .payload = .{ .type = tl } },
        else => Error.UnexpectedToken,
    };
}

pub fn isExpressionComptime(self: *Parser, expr: Expression) Error!bool {
    _ = self;
    //recursively descend
    return expr == .value;
}
pub const Expression = union(enum) {
    call: Call,
    identifier: []const u8,
    builtin: bi.Builtin,

    value: Value,

    bin_op: BinOp,
    u_op: UOp,

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

    pub fn isEmpty(self: Expression) bool {
        return self == .value and self.value.type == .void;
    }
    pub const empty: Expression = .{ .value = .{ .type = .void } };
    pub const format = debug.formatExpression;

    pub fn shouldTurnIntoIntermediate(self: Expression) bool {
        return self != .value;
    }
};
pub fn asTypeCreate(self: *Parser, expr: Expression) Error!Type {
    var as_type = try self.asType(@constCast(&expr));
    if (as_type == .unknown) as_type.unknown = try self.createVal(expr);
    return as_type;
}
pub fn asType(self: *Parser, expr: *Expression) Error!Type {
    const unknown: Type = .{ .unknown = expr };
    return switch (expr.*) {
        .value => |value| if (value.type == .type) value.payload.type else unknown,
        .identifier => |identifier| blk: {
            const var_ref = try self.current_scope.getVariableReference(self, identifier);
            break :blk if (var_ref.type == .type and var_ref.value == .value) var_ref.value.value.payload.type else unknown;
        },
        .builtin => @panic("TODO: asType of builtin"),
        else => unknown,
    };
}
pub fn isExprMutable(self: *Parser, expr: Expression) Error!bool {
    return switch (expr) {
        .identifier => |identifier| (try self.current_scope.getVariableReference(self, identifier)).is_mutable,
        .indexing => |indexing| try self.isExprMutable(indexing.target.*),
        else => false,
    };
}
pub const Call = struct {
    callee: *Expression,
    args: []Expression,
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

    body: List(Statement) = .empty,

    pub fn new() @This() {
        return .{
            .scope = .{
                .getVariableReferenceFn = &getVariableReferenceFn,
                .trackReferenceFn = &trackReferenceFn,
                .addStatementFn = &addStatementFn,
            },
        };
    }
    pub fn addStatement(self: *GlobalScope, parser: *Parser, statement: Statement) Error!void {
        try addStatementFn(&self.scope, parser, statement);
    }
    fn getVariableReference(self: *GlobalScope, parser: *Parser, name: []const u8) Error!VariableReference {
        return try getVariableReferenceFn(&self.scope, parser, name);
    }

    fn addStatementFn(scope: *Scope, parser: *Parser, statement: Statement) Error!void {
        const global_scope: *GlobalScope = @fieldParentPtr("scope", scope);
        if (!(try parser.isStatementComplete(statement))) return parser.errorOut(Error.IncompleteStatement);
        try global_scope.body.append(parser.arena.allocator(), statement);
    }
    fn getVariableReferenceFn(scope: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
        const global_scope: *GlobalScope = @fieldParentPtr("scope", scope);
        return for (global_scope.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name))
                break statement.var_decl.variableReference();
        } else parser.errorUndeclaredVariable(name);
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
    addStatementFn: *const fn (*Scope, *Parser, Statement) Error!void = undefined,
    //add a Parser field so scope can accerr global scope directly
    getVariableReferenceFn: *const fn (*Scope, *Parser, []const u8) Error!VariableReference = undefined,
    trackReferenceFn: *const fn (*Scope, []const u8, DeclReferenceType) Error!void = undefined,

    parent: *Scope = undefined,
    result_type: Type = Type{ .void = {} },

    pub const DeclReferenceType = enum { track, untrack };

    pub inline fn addStatement(self: *Scope, parser: *Parser, statement: Statement) Error!void {
        try self.addStatementFn(self, parser, statement);
    }

    pub inline fn getVariableReference(self: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
        return try self.getVariableReferenceFn(self, parser, name);
    }

    pub inline fn trackReference(self: *Scope, name: []const u8, ref_type: DeclReferenceType) Error!void {
        try self.trackReferenceFn(self, name, ref_type);
    }
    pub fn defaltGetVariableReference(T: type) fn (*Scope, *Parser, []const u8) Error!VariableReference {
        return struct {
            pub fn f(scope: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
                const impl: *T = @fieldParentPtr("scope", scope);
                return for (impl.body.items) |*statement| {
                    if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name))
                        break statement.var_decl.variableReference();
                } else try scope.parent.getVariableReference(parser, name);
            }
        }.f;
    }
};
pub const VariableReference = struct {
    is_mutable: bool,
    type: Type,
    value: Expression,
};

fn parseQualifier(self: *Parser, token: Token) Error!Qualifier {
    _ = self;
    return switch (token) {
        .in => .{ .in = .smooth },
        .out => .{ .out = .smooth },
        .@"const" => .@"const",
        .mut => .mut,
        .uniform => .{ .uniform = .private },
        .push => .{ .push = .private },
        .shared => .shared,
        else => Error.UnexpectedToken,
    };
}
pub const Qualifier = union(enum) {
    @"const",
    mut,

    uniform: bi.UniformAccessQualifier,
    push: bi.UniformAccessQualifier,

    in: bi.InterpolationQualifier,
    out: bi.InterpolationQualifier,

    shared,
    pub fn canHaveInitializer(self: Qualifier) bool {
        return self != .in;
    }
    pub fn isMutable(self: Qualifier) bool {
        return switch (self) {
            .mut, .shared, .out => true,
            .@"const", .in, .uniform, .push => false,
        };
    }
};

pub const Value = struct {
    type: Type,
    payload: ValuePayload = undefined,
    pub const format = debug.formatValue;
};

pub const ValuePayload = union {
    wide: WIDE,
    type: Type,
    ptr: *const anyopaque,
};

pub fn errorInvalidOperandTypes(self: *Parser, left_type: Type, op: Tokenizer.BinaryOperator, right_type: Type) Error {
    return errorOutFmt(self, Error.InvalidOperands, "Invalid operand types: {f} {s} {f}", .{ left_type, @tagName(op), right_type });
}
pub fn errorUndeclaredVariable(self: *Parser, name: []const u8) Error {
    return self.errorOutFmt(
        Error.UndeclaredVariable,
        "Undeclared variable \'{s}\'",
        .{name},
    );
}
pub fn errorUnexpectedToken(self: *Parser, token: Token) Error {
    return self.errorOutFmt(
        Error.UnexpectedToken,
        "Unexpected token \'{f}\'",
        .{token},
    );
}
pub fn errorOutDefault(self: *Parser, comptime err: Error) Error {
    return self.errorOutFmt(err, switch (err) {
        Error.MissingInitializer => "Constants must have initializers",
        else => return self.errorOut(err),
    }, .{});
}
pub fn errorOut(self: *Parser, err: Error) Error {
    return self.errorOutFmt(err, "", .{});
}
pub fn errorOutFmt(self: *Parser, err: Error, comptime fmt: []const u8, args: anytype) Error {
    const offset = @intFromPtr(self.tokenizer.state.last_ptr) - @intFromPtr(self.tokenizer.full_source.ptr);
    self.tokenizer.err_ctx.printError(offset, err, fmt, args);
    return err;
}

const ShouldStopFn = fn (*Parser, Token) Error!bool;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
pub const refine = ct.refineTopLevel;
pub const implicitCast = ct.implicitCast;
pub const typeOf = tp.typeOf;
pub const Type = tp.Type;
pub const FunctionType = tp.FunctionType;
const UnaryOperator = Tokenizer.UnaryOperator;
pub const Token = Tokenizer.Token;

pub const pr = std.debug.print;
