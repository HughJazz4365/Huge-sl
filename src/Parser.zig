const std = @import("std");
const zigbuiltin = @import("builtin");
const debug = @import("debug.zig");
const bi = @import("builtin.zig");
const ct = @import("comptime.zig");
pub const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");

pub const CF = f64;
pub const CI = i128;
pub const WIDE = u128;

tokenizer: *Tokenizer = undefined,

allocator: Allocator = undefined,
arena: std.heap.ArenaAllocator = undefined,

global_io: List([]const u8) = .empty,
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
    InvalidIndexingTarget,
    InvalidAssignmentTarget,
    NonVoidValueIgnored,

    NumericError,
    OutOfBoundsAccess,
    NegativePower,
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

    try self.parseScope(&self.global_scope.scope);

    std.debug.print("[GLOBAL IO: ", .{});
    for (self.global_io.items) |gi| std.debug.print("{s}, ", .{gi});
    std.debug.print("]\n", .{});

    if (zigbuiltin.mode == .Debug) for (self.global_scope.body.items) |statement| std.debug.print("{f}\n", .{statement});
    return self;
}
pub fn deinit(self: *Parser) void {
    self.arena.deinit();
}
pub fn turnIntoIntermediateVariableIfNeeded(self: *Parser, expr: Expression) Error!Expression {
    return if (expr == .named_value or (expr == .value and !expr.value.shouldBeNamed()) or expr == .identifier) expr else try self.turnIntoIntermediateVariable(expr);
}
pub fn turnIntoIntermediateVariable(self: *Parser, expr: Expression) Error!Expression {
    const name = try self.createIntermediateVariableName();
    try self.addStatement(.{ .var_decl = .{
        .qualifier = .@"const",
        .name = name,
        .type = try self.typeOf(expr),
        .initializer = expr,
    } });
    return try self.refine(.{ .identifier = name });
}
pub fn createIntermediateVariableName(self: *Parser) Error![]u8 {
    if (zigbuiltin.mode == .Debug) {
        const prefix = "IV #";
        var digit_buf: [6]u8 = @splat(0);
        var writer: std.Io.Writer = .fixed(&digit_buf);
        if (self.intermediate_value_index >= std.math.powi(u32, 10, digit_buf.len) catch unreachable) @panic("too many IV`s");
        writer.print("{d}", .{self.intermediate_value_index}) catch unreachable;
        const slice = try self.arena.allocator().alloc(u8, prefix.len + writer.buffered().len);
        @memcpy(slice[0..prefix.len], prefix);
        @memcpy(slice[prefix.len..], writer.buffered());
        self.intermediate_value_index += 1;
        return slice;
    } else {
        const prefix = "IV ";
        const slice = try self.arena.allocator().alloc(u8, prefix.len + 4);
        @memcpy(slice[0..prefix.len], prefix);
        const byteptr: [*]const u8 = @ptrCast(@alignCast(&self.intermediate_value_index));
        inline for (0..4) |i| slice[slice.len - (4 - i)] = byteptr[i];
        self.intermediate_value_index += 1;
        return slice;
    }
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
    try self.trackReferencesInStatement(statement, .track);
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
    if (token == .endl) {
        self.tokenizer.skip();
        token = try self.tokenizer.peek();
    }
    if (self.defaultShouldStop(token) catch unreachable) return null;

    const statement: ?Statement = switch (token) {
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
                try self.parseExpression(defaultShouldStop, false),
                self.current_scope.result_type,
            ) };
        },

        else => try self.parseAssignmentOrIgnore(),
    };
    return statement;
}
pub fn trackReferencesInStatement(self: *Parser, statement: Statement, ref_type: Scope.DeclReferenceType) Error!void {
    switch (statement) {
        .var_decl => |var_decl| try ct.trackReferencesDescend(ref_type, self, var_decl.initializer),
        .assignment => |assignment| {
            try ct.trackReferencesDescend(ref_type, self, assignment.target);
            try ct.trackReferencesDescend(ref_type, self, assignment.value);
        },
        .ignore => |expr| try ct.trackReferencesDescend(ref_type, self, expr),
        else => {},
    }
}
fn parseAssignmentOrIgnore(self: *Parser) Error!Statement {
    const tokenizer_state = self.tokenizer.state;
    const is_assignment: bool = sw: switch (try self.tokenizer.next()) {
        .endl, .eof => false,
        .@"=" => true,
        else => continue :sw try self.tokenizer.next(),
    };
    self.tokenizer.state = tokenizer_state;
    if (!is_assignment) {
        const ignored_expr = try self.parseExpression(defaultShouldStop, false);
        const type_of_ignored = try self.typeOf(ignored_expr);
        if (type_of_ignored != .unknown and type_of_ignored != .void) return self.errorOut(Error.NonVoidValueIgnored);

        return .{ .ignore = ignored_expr };
    }

    const target = try self.parseExpressionSide(false);
    var peek = try self.tokenizer.next();
    if (peek != .@"=") return self.errorOut(Error.InvalidAssignmentTarget);

    if (!try self.isExpressionMutable(target)) return self.errorOutFmt(Error.MutatingImmutableVariable, "Trying to mutate unmutable variable: {f}", .{target});

    peek = try self.tokenizer.peek();
    const modifier: ?BinaryOperator = if (peek == .bin_op) op: {
        self.tokenizer.skip();
        break :op peek.bin_op;
    } else null;

    var value = try self.parseExpression(defaultShouldStop, false);

    if (modifier) |mod| {
        const create_value = try self.createVal(value);
        value = try self.refine(.{ .bin_op = .{
            .left = try self.createVal(try self.refine(target)),
            .right = create_value,
            .op = mod,
        } });
    }
    return .{ .assignment = .{
        .target = target,
        .value = try self.implicitCast(value, try self.typeOf(target)),
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
            try self.refine(try self.parseExpressionSide(true)),
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

            const expr = try self.implicitCast(try self.parseExpression(defaultShouldStop, false), @"type");
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
    if (@"type".isComptimeOnly() and qualifier != .@"const") return self.errorOut(Error.VariableTypeAndQualifierDontMatch);
}

pub const Statement = union(enum) {
    var_decl: VariableDeclaration,
    assignment: Assignment,
    ignore: Expression,
    @"return": Expression,
    // @"break": Expression,
    // @"continue"
    // or combine those into control flow thing
    pub const format = debug.formatStatement;
};
pub fn isStatementOmittable(self: *Parser, statement: Statement) Error!bool {
    return switch (statement) {
        .var_decl => |var_decl| var_decl.canBeOmitted(),
        .assignment => |assignment| switch (assignment.target) {
            .identifier => |identifier| (try self.current_scope.getVariableReference(self, identifier)).is_omittable,
            else => false,
        },
        else => false,
    };
}
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

    pub fn canBeOmitted(self: VariableDeclaration) bool {
        return self.reference_count == 0 and !self.qualifier.isIO() and self.type != .entrypoint;
    }
    pub fn variableReference(self: *VariableDeclaration) VariableReference {
        return .{
            .is_mutable = self.qualifier.isMutable(),
            .type = self.type,
            .value = if ((self.qualifier == .@"const" or self.qualifier == .mut) and isComptimePassable(self.initializer))
                if (self.initializer == .value and self.initializer.value.shouldBeNamed())
                    .{ .named_value = .{ .name = self.name, .value = self.initializer.value } }
                else
                    self.initializer
            else
                .{ .identifier = self.name },

            .is_omittable = self.canBeOmitted(),
        };
    }
};
pub fn isComptimePassable(expr: Expression) bool {
    return switch (expr) {
        .identifier, .value => !expr.isEmpty(),
        .constructor => |constructor| for (constructor.components) |c| (if (!isComptimePassable(c)) break false) else true,
        .cast => |cast| isComptimePassable(cast.expr.*),
        else => false,
    };
}

pub fn parseExpression(self: *Parser, should_stop: ShouldStopFn, consume_end: bool) Error!Expression {
    return try self.parseExpressionRecursive(should_stop, consume_end, 0);
}
pub fn parseExpressionRecursive(self: *Parser, should_stop: ShouldStopFn, consume_end: bool, last_bp: u8) Error!Expression {
    var left = try self.refine(try self.parseExpressionSide(true));
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
    if (consume_end) self.tokenizer.skip();

    return left;
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
    return token == .endl or token == .eof or (token == .@"}" and !self.inGlobalScope());
}

fn parseExpressionSide(self: *Parser, comptime access: bool) Error!Expression {
    const refine_func = if (access) refine else refineAssigmentTarget;
    var expr = try refine_func(self, try self.parseExpressionSidePrimary(access));
    sw: switch (try self.tokenizer.peek()) {
        .@"{" => {
            if (try self.typeOf(expr) == .type) {
                self.tokenizer.skip();
                expr = try self.parseCastOrConstructor(try self.asTypeCreate(expr));
                continue :sw try self.tokenizer.peek();
            } else return self.errorOut(Error.UnexpectedToken);
        },
        .@"(" => {
            self.tokenizer.skip();

            expr = try refine_func(self, .{ .call = try self.parseFunctionCall(expr) });
            continue :sw try self.tokenizer.peek();
        },
        else => {},
    }
    return expr;

    //::::secondary::::
    //(check proceeding tokens)
    //if swizzle => swizzle(expr)
    //if '[' => indexing into expr
    //if '(' => function call
    //if '.' =>  member access

}
fn parseExpressionSidePrimary(self: *Parser, comptime access: bool) Error!Expression {
    const refine_func = if (access) refine else refineAssigmentTarget;
    const token = try self.tokenizer.next();
    return switch (token) {
        .u_op => |u_op| .{ .u_op = .{
            .op = u_op,
            .target = try self.createVal(try refine_func(self, try self.parseExpressionSide(access))),
        } },
        .identifier => |id| .{ .identifier = id },
        .builtin => |builtin| .{ .builtin = try bi.getBuiltin(builtin) },
        .@"(" => try self.parseExpression(bracketShouldStop, true),
        .@"." => switch (try self.tokenizer.next()) {
            .@"{" => try refine_func(self, try self.parseUnknownConstructor()),
            else => return self.errorOut(Error.UnexpectedToken),
        },
        .@"fn" => try self.parseFunctionTypeOrValue(),
        .entrypoint => try self.parseEntryPointTypeOrValue(),
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
    //consider struct constructors
    const components = try self.parseExpressionSequence(.@"}");
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
                    const type_expr = try self.parseExpression(argDeclShouldStop, true);
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
    const rtype = try self.asTypeCreate(try self.refine(try self.parseExpressionSidePrimary(true)));

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
    const exec_model_info: ExecutionModelInfo =
        if (token == .@"." and try self.tokenizer.peek() == .identifier and util.strEql((try self.tokenizer.peek()).identifier, "fragment"))
            .fragment
        else
            .vertex;

    self.tokenizer.skip();
    token = try self.tokenizer.next();
    if (token != .@")") return self.errorOut(Error.InvalidInput);

    const ep_type: Type = .{ .entrypoint = std.meta.activeTag(exec_model_info) };

    if (try self.tokenizer.peek() != .@"{") return ep_type.asExpr();
    self.tokenizer.skip();

    var entry_point: EntryPoint = .new(self, exec_model_info);
    try self.parseScope(&entry_point.scope);

    entry_point.global_io_count = self.global_io.items.len;

    return .{ .value = .{
        .type = ep_type,
        .payload = .{ .ptr = @ptrCast(@alignCast(try self.createVal(entry_point))) },
    } };
}

fn parseScope(self: *Parser, scope: *Scope) Error!void {
    const last_scope = self.current_scope;
    self.current_scope = scope;
    defer self.current_scope = last_scope;
    while (try self.parseStatement()) |statement| {
        try self.addStatement(statement);
        var peek = try self.tokenizer.peek();
        if (peek == .endl) {
            self.tokenizer.skip();
            peek = try self.tokenizer.peek();
        }
        if (peek == .@"}") {
            self.tokenizer.skip();
            break;
        }
    } else if (try self.defaultShouldStop(try self.tokenizer.peek())) {
        self.tokenizer.skip();
    } else if (!self.inGlobalScope()) return self.errorOut(Error.UnclosedScope);

    //remove useless statements
    const body = self.current_scope.body();
    var i: usize = 0;
    while (i < body.len) {
        const index = body.len - 1 - i;
        const statement = body.*[index];
        if (statement != .var_decl or //
            (statement == .var_decl and !statement.var_decl.qualifier.isIO() and statement.var_decl.reference_count == 0))
            try self.trackReferencesInStatement(statement, .untrack);
        if (try self.isStatementOmittable(statement)) {
            util.removeAt(Statement, body, index);
        } else i += 1;
    }
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
                .bodyFn = bodyFn,

                .parent = self.current_scope,
                .result_type = ftype.rtype.*,
            },
        };
    }
    fn bodyFn(scope: *Scope) *[]Statement {
        return &@as(*@This(), @fieldParentPtr("scope", scope)).body.items;
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
    exec_model_info: ExecutionModelInfo,
    scope: Scope,

    global_io_count: usize = 0,
    io: [][]const u8 = &.{},
    body: List(Statement),

    pub fn new(self: *Parser, stage_info: ExecutionModelInfo) EntryPoint {
        return .{
            .exec_model_info = stage_info,
            .body = .empty,
            .scope = .{
                .getVariableReferenceFn = &getVariableReferenceFn,
                .addStatementFn = &addStatementFn,
                .trackReferenceFn = &trackReferenceFn,
                .bodyFn = bodyFn,

                .parent = self.current_scope,
            },
        };
    }
    fn bodyFn(scope: *Scope) *[]Statement {
        return &@as(*@This(), @fieldParentPtr("scope", scope)).body.items;
    }
    fn addStatementFn(scope: *Scope, parser: *Parser, statement: Statement) Error!void {
        const entry_point: *EntryPoint = @fieldParentPtr("scope", scope);
        if (!(try parser.isStatementComplete(statement))) return parser.errorOut(Error.IncompleteStatement);
        try entry_point.body.append(parser.arena.allocator(), statement);
        if (statement == .var_decl and statement.var_decl.qualifier.isIO())
            entry_point.io = try util.reallocAdd(parser.arena.allocator(), []const u8, entry_point.io, statement.var_decl.name);
    }
    fn getVariableReferenceFn(scope: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
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

pub const ExecutionModelInfo = union(tp.ExecutionModel) {
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

pub const Expression = union(enum) {
    call: Call,
    identifier: []const u8,
    named_value: NamedValue,

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

    pub const empty: Expression = .{ .value = .{ .type = .void } };
    pub const format = debug.formatExpression;
    pub fn isEmpty(self: Expression) bool {
        return self == .value and self.value.type == .void;
    }
    pub fn isComptime(self: Expression) bool {
        return self == .value or self == .named_value;
    }
    ///assumes that self.isComptime() == true
    pub fn getValue(self: Expression) Value {
        return if (self == .value) self.value else self.named_value.value;
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
        .named_value => |named_value| if (named_value.value.type == .type) named_value.value.payload.type else unknown,
        .value => |value| if (value.type == .type) value.payload.type else unknown,
        .identifier => |identifier| blk: {
            const var_ref = try self.current_scope.getVariableReference(self, identifier);
            break :blk if (var_ref.type == .type and var_ref.value == .value) var_ref.value.value.payload.type else unknown;
        },
        .builtin => @panic("TODO: asType of builtin"),
        else => unknown,
    };
}
pub fn isExpressionMutable(self: *Parser, expr: Expression) Error!bool {
    return switch (expr) {
        .identifier => |identifier| (try self.current_scope.getVariableReference(self, identifier)).is_mutable,
        .indexing => |indexing| try self.isExpressionMutable(indexing.target.*),
        else => false,
    };
}
pub const NamedValue = struct {
    name: []const u8,
    value: Value,
};
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
                .bodyFn = bodyFn,
            },
        };
    }
    fn bodyFn(scope: *Scope) *[]Statement {
        return &@as(*@This(), @fieldParentPtr("scope", scope)).body.items;
    }
    fn getVariableReference(self: *GlobalScope, parser: *Parser, name: []const u8) Error!VariableReference {
        return try getVariableReferenceFn(&self.scope, parser, name);
    }

    fn addStatementFn(scope: *Scope, parser: *Parser, statement: Statement) Error!void {
        const global_scope: *GlobalScope = @fieldParentPtr("scope", scope);
        if (!(try parser.isStatementComplete(statement))) return parser.errorOut(Error.IncompleteStatement);

        try global_scope.body.append(parser.arena.allocator(), statement);
        if (statement == .var_decl and statement.var_decl.qualifier.isIO())
            try parser.global_io.append(parser.arena.allocator(), statement.var_decl.name);
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
    getVariableReferenceFn: *const fn (*Scope, *Parser, []const u8) Error!VariableReference = undefined,
    trackReferenceFn: *const fn (*Scope, []const u8, DeclReferenceType) Error!void = undefined,
    bodyFn: *const fn (*Scope) *[]Statement = undefined,

    parent: *Scope = undefined,
    result_type: Type = Type{ .void = {} },

    pub const DeclReferenceType = enum { track, untrack };
    pub inline fn body(self: *Scope) *[]Statement {
        return self.bodyFn(self);
    }
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

    is_omittable: bool = false,
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
    pub fn isIO(self: Qualifier) bool {
        return switch (self) {
            .uniform, .push, .in, .out => true,
            else => false,
        };
    }
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
    pub fn shouldBeNamed(self: Value) bool {
        return switch (self.type) {
            .function => true,
            .type => self.payload.type == .@"struct" or self.payload.type == .@"enum",
            else => false,
        };
    }
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
pub fn inGlobalScope(self: *Parser) bool {
    return @intFromPtr(self.current_scope) == @intFromPtr(&self.global_scope.scope);
}

const ShouldStopFn = fn (*Parser, Token) Error!bool;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
pub const refine = ct.refine;
pub const refineAssigmentTarget = ct.refineAssigmentTarget;

pub const implicitCast = ct.implicitCast;
pub const typeOf = tp.typeOf;
pub const Type = tp.Type;
pub const FunctionType = tp.FunctionType;
const UnaryOperator = Tokenizer.UnaryOperator;
pub const Token = Tokenizer.Token;

pub const pr = std.debug.print;
