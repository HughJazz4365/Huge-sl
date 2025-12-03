const std = @import("std");
const zigbuiltin = @import("builtin");
const debug = @import("debug.zig");
const bi = @import("builtin.zig");
const ct = @import("comptime.zig");
pub const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");
const root = @import("root.zig");
const Settings = root.Settings;

pub const CF = f64;
pub const CI = i128;
pub const WIDE = u128;

tokenizer: *Tokenizer = undefined,

allocator: Allocator = undefined,
arena: std.heap.ArenaAllocator = undefined,

global_scope: Struct = undefined,
current_scope: *Scope = undefined,

intermediate_value_index: u32 = 0,
settings: Settings,

structs: List(Struct) = .empty,

pub const Error = error{
    OutOfMemory,

    UnexpectedToken,

    RecursionNotSupported,
    UnclosedScope,
    IncompleteStatement,
    IncompleteStructConstructor,
    MissingFunctionBody,

    VariableRedeclaration,
    MissingInitializer,
    RepeatingArgumentNames,
    VariableTypeAndQualifierDontMatch,
    UnexpectedInitializer,
    UnexpectedStatement,
    NoTypeVariable,

    MutatingImmutableVariable,
    UndeclaredVariable,
    NoMemberWithName,
    InvalidCall,
    InvalidBuiltin,
    NonMatchingArgumentCount,
    NonMatchingVariableInitializerCount,

    CannotImplicitlyCast,
    CannotExplicitlyCast,
    CannotInferType,

    InvalidOperands,
    InvalidUnaryOperationTarget,
    InvalidConstructor,
    InvalidIndex,
    InvalidIndexingTarget,
    InvalidAssignmentTarget,
    InvalidArrayLen,
    InvalidMemberAccess,
    InvalidSwizzle,
    NonVoidValueIgnored,

    NumericError,
    OutOfBoundsAccess,
    NegativePower,
    InvalidBitshift,
} || Tokenizer.Error;

///creates its own arena
pub fn parse(allocator: Allocator, tokenizer: *Tokenizer, settings: Settings, file_name: []const u8) Error!Parser {
    var self: Parser = .{ .settings = settings };
    self.allocator = allocator;
    self.tokenizer = tokenizer;
    self.arena = .init(allocator);
    self.global_scope = .new(.self);
    self.global_scope.name = file_name;
    self.current_scope = &self.global_scope.scope;

    try self.parseScope(&self.global_scope.scope);

    if (zigbuiltin.mode == .Debug) {
        // if (false and zigbuiltin.mode == .Debug) {
        std.debug.print("[GLOBAL IO: ", .{});
        for (self.global_scope.global_io.items) |gi| std.debug.print("{s}, ", .{gi});
        std.debug.print("]\n", .{});
        for (self.global_scope.body.items) |statement| std.debug.print("{f}\n", .{statement});
    }
    return self;
}
pub fn deinit(self: *Parser) void {
    self.arena.deinit();
}
pub fn turnIntoIntermediateVariableIfNeeded(self: *Parser, expr: Expression) Error!Expression {
    return if (switch (expr) {
        .named_value, .identifier, .builtin => false,
        .value => |value| value.shouldBeNamed(),
        else => true,
    }) try self.turnIntoIntermediateVariable(expr) else expr;
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
    // std.debug.print("S: {f}\n", .{statement});
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

pub fn parseStatement(self: *Parser) Error!usize {
    try self.skipEndl();
    const token = try self.tokenizer.peek();
    if (self.defaultShouldStop(token) catch unreachable) return 0;

    return switch (token) {
        .eof => 0,
        .@"const", .mut, .descriptor, .push, .shared, .out, .in, .member => self.parseVariableDeclarations(),
        .@"return" => blk: {
            self.tokenizer.skip();
            const peek = try self.tokenizer.peek();
            if (self.defaultShouldStop(peek) catch unreachable) {
                if (peek != .@"}") self.tokenizer.skip();
                try self.addStatement(.{ .@"return" = .empty });
            } else try self.addStatement(.{ .@"return" = try self.implicitCast(
                try self.parseExpression(defaultShouldStop, false),
                self.current_scope.result_type,
            ) });

            break :blk 1;
        },

        else => blk: {
            const statement = try self.parseAssignmentOrIgnore();
            try self.addStatement(statement);
            break :blk 1;
        },
    };
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

    const target = try self.parseExpressionSide(false, false);

    // var peek = try self.tokenizer.next();
    // if (peek != .@"=") return self.errorOut(Error.InvalidAssignmentTarget);

    if (!try self.isExpressionMutable(target)) return self.errorOutFmt(Error.MutatingImmutableVariable, "Trying to mutate unmutable variable: {f}", .{target});

    var peek = try self.tokenizer.peek();
    const modifier: ?BinaryOperator = if (peek == .bin_op) op: {
        self.tokenizer.skip();
        break :op peek.bin_op;
    } else null;
    peek = try self.tokenizer.next();
    if (peek != .@"=") return self.errorOut(Error.InvalidAssignmentTarget);

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
fn parseVariableDeclarations(self: *Parser) Error!usize {
    // const body_index = self.current_scope.body().*.len;

    var list: List(VariableDeclaration) = .empty;
    var index_list: List(usize) = .empty;
    var member_count: usize = 0;
    defer index_list.deinit(self.arena.allocator());

    defer self.current_scope.multi_variable_initialization = &.{};
    self.current_scope.current_variable_value_index = 0;

    var token = try self.tokenizer.next();

    var qualifier: Qualifier = try self.parseQualifier(token);
    try self.skipEndl();

    token = try self.tokenizer.next();

    var last: enum { qual, name, type, comma } = .qual;
    sw: switch (token) {
        .@"," => {
            if (last == .type) self.tokenizer.skip();

            if (last != .name and last != .type) return self.errorUnexpectedToken(token);
            last = .comma;
            try self.skipEndl();
            token = try self.tokenizer.next();
            continue :sw token;
        },

        .in, .out, .@"const", .mut, .descriptor, .push, .shared => {
            if (last == .type) self.tokenizer.skip();

            qualifier = try self.parseQualifier(token);
            try self.skipEndl();
            if (last != .comma) return self.errorUnexpectedToken(token);
            last = .qual;
            token = try self.tokenizer.next();
            continue :sw token;
        },
        .identifier => |identifier| {
            if (last == .type) self.tokenizer.skip();

            if (last != .comma and last != .qual) return self.errorUnexpectedToken(token);
            last = .name;
            blk: {
                _ = self.current_scope.getVariableReference(self, identifier) catch break :blk;
                return self.errorOut(Error.VariableRedeclaration);
            }
            try index_list.append(self.arena.allocator(), self.current_scope.body().len + list.items.len - member_count);

            if (qualifier == .member) member_count += 1;
            try list.append(self.arena.allocator(), .{
                .qualifier = qualifier,
                .name = identifier,
            });
            self.current_scope.multi_variable_initialization = list.items;

            token = try self.tokenizer.next();
            continue :sw token;
        },
        .@":" => {
            if (last != .name) return self.errorUnexpectedToken(token);

            const type_expr = try self.refine(try self.parseExpressionSide(false, true));
            const @"type" = try self.asTypeCreate(type_expr);

            var index = list.items.len - 1;
            while (list.items[index].type.isEmpty()) : (index -= 1) {
                list.items[index].type = @"type";
                if (index == 0) break;
            }
            last = .type;
            token = try self.tokenizer.peek();
            continue :sw token;
        },
        .@"=" => {
            if (last == .type) self.tokenizer.skip();
            try self.skipEndl();

            if (last != .name and last != .type) return self.errorUnexpectedToken(token);
            token = try self.tokenizer.peek();
            var index: usize = 0;
            var expr: Expression = .empty;

            sw2: switch (token) {
                .@"," => {
                    self.tokenizer.skip();
                    try self.skipEndl();
                    if (list.items[index].type.isEmpty()) return self.errorOut(Error.NoTypeVariable);
                    index += 1;

                    if (index + 1 == list.items.len and try self.defaultShouldStop(try self.tokenizer.peek())) break :sw2;
                    try self.skipEndl();
                    if (index + 1 > list.items.len) return self.errorOut(Error.NonMatchingVariableInitializerCount);

                    self.current_scope.multi_variable_initialization = list.items;

                    token = try self.tokenizer.peek();
                    continue :sw2 token;
                },
                else => if (try self.defaultShouldStop(token)) {
                    if (expr.isEmpty()) return self.errorUnexpectedToken(token);
                    if (index == 0 and list.items.len > 1) {
                        for (list.items[1..]) |*vd| try self.addInitializerToVarDecl(vd, expr);
                    }
                    break :sw;
                } else {
                    self.current_scope.current_variable_value_index = index;
                    const initializer = try self.parseExpression(initializerShouldStop, false);

                    try self.addInitializerToVarDecl(&list.items[index], initializer);
                    self.current_scope.multi_variable_initialization = list.items[0 .. index + 1];

                    if (expr.isEmpty()) expr = initializer;

                    token = try self.tokenizer.peek();
                    continue :sw2 token;
                },
            }
        },
        else => {
            if (try self.defaultShouldStop(token)) {
                for (list.items[1..]) |*vd| try self.addInitializerToVarDecl(vd, .empty);
                break :sw;
            }
            return self.errorUnexpectedToken(token);
        },
    }

    var offset: usize = 0;
    if (list.items.len == 1) {
        try self.addInitializerToVarDecl(&list.items[0], list.items[0].initializer);
        try self.addStatement(.{ .var_decl = list.items[0] });
    } else for (list.items, index_list.items) |vd, index| {
        const i = offset + index;
        const current_index = self.current_scope.body().len;
        const diff = current_index - i;
        defer offset += diff;

        if (diff == 0) {
            try self.addStatement(.{ .var_decl = vd });
        } else {
            const statement: Statement = .{ .var_decl = vd };
            try self.addStatement(statement);
            const body = self.current_scope.body().*;
            @memmove(body[i + 1 ..], body[i .. i + diff]);
            body[i] = statement;
        }
    }

    return list.items.len;
}
fn addInitializerToVarDecl(self: *Parser, var_decl: *VariableDeclaration, initializer: Expression) Error!void {
    var_decl.initializer = initializer;
    //name value
    if (var_decl.type == .type and var_decl.initializer == .value) {
        const t = var_decl.initializer.value.payload.type;
        if (t == .@"struct") {
            self.getStructFromID(t.@"struct").name = var_decl.name;
        }
    }

    if (initializer.isEmpty()) {
        if (var_decl.type.isEmpty()) return self.errorOut(Error.NoTypeVariable);
        try self.matchVariableTypeWithQualifier(var_decl.type, var_decl.qualifier);
        return;
    }

    if (var_decl.type.isEmpty()) var_decl.type = try self.typeOf(var_decl.initializer);

    try self.matchVariableTypeWithQualifier(var_decl.type, var_decl.qualifier);
    var_decl.initializer = try self.implicitCast(var_decl.initializer, var_decl.type);
}
fn matchVariableTypeWithQualifier(self: *Parser, @"type": Type, qualifier: Qualifier) Error!void {
    // std.debug.print("Q: {s}, T: {f}\n", .{ @tagName(qualifier), @"type" });
    if ((@"type".isBindable() != (qualifier == .descriptor)) or //
        (@"type".isComptimeOnly() and qualifier != .@"const")) return self.errorOut(Error.VariableTypeAndQualifierDontMatch);
    if (qualifier == .push and if (@"type".scalarPrimitive()) |sp| sp.width == .short else false) return self.errorOut(Error.VariableTypeAndQualifierDontMatch);
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
    type: Type = .unknownempty,
    initializer: Expression = .empty,
    reference_count: u32 = 0,

    pub fn canBeOmitted(self: VariableDeclaration) bool {
        return self.reference_count == 0 and
            !self.qualifier.isIO() and
            self.qualifier != .push and
            self.type != .entrypoint;
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
    var left = try self.refine(try self.parseExpressionSide(false, true));
    var token = try self.tokenizer.peek();
    while (!try should_stop(self, token)) {
        if (token != .bin_op) return self.errorUnexpectedToken(token);

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
fn initializerShouldStop(self: *Parser, token: Token) !bool {
    return token == .@"," or try self.defaultShouldStop(token);
}
fn defaultShouldStop(self: *Parser, token: Token) !bool {
    return token == .endl or token == .eof or (token == .@"}" and !self.inGlobalScope());
}

fn squareBracketShouldStop(self: *Parser, token: Token) !bool {
    _ = self;
    return token == .@"]";
}

fn parseExpressionSide(self: *Parser, ignore_curly: bool, comptime access: bool) Error!Expression {
    const refine_func = if (access) refine else refineAssigmentTarget;
    var expr = try refine_func(self, try self.parseExpressionSidePrimary(access));

    sw: switch (try self.tokenizer.peek()) {
        .@"{" => {
            if (ignore_curly) return expr;
            if (try self.typeOf(expr) == .type) {
                self.tokenizer.skip();
                expr = try refine_func(
                    self,
                    try self.parseCastOrConstructor(try self.asTypeCreate(expr)),
                );
                continue :sw try self.tokenizer.peek();
            } else return self.errorOut(Error.UnexpectedToken);
        },
        .@"(" => {
            self.tokenizer.skip();

            expr = try refine_func(self, .{ .call = try self.parseFunctionCall(expr, null) });
            continue :sw try self.tokenizer.peek();
        },
        .@"[" => {
            self.tokenizer.skip();
            const index = try self.parseExpression(squareBracketShouldStop, true);
            expr = try refine_func(self, .{ .indexing = .{
                .target = try self.createVal(expr),
                .index = try self.createVal(index),
            } });
            continue :sw try self.tokenizer.peek();
        },
        .@"." => {
            self.tokenizer.skip();
            const next = try self.tokenizer.next();
            if (next != .identifier and next != .builtin) return self.errorUnexpectedToken(next);
            if (try self.tokenizer.peek() == .@"(") {
                self.tokenizer.skip();
                const callee: Expression = if (next == .identifier)
                    refine_func(self, .{ .member_access = .{
                        .target = try self.createVal((try self.typeOf(expr)).asExpr()),
                        .member_name = next.identifier,
                    } }) catch |err| switch (err) {
                        Error.InvalidMemberAccess => try refine_func(self, .{ .identifier = next.identifier }),
                        else => return err,
                    }
                else
                    .{ .builtin = try bi.getBuiltin(next.builtin) };

                expr = try refine_func(self, .{ .call = try self.parseFunctionCall(callee, expr) });
                continue :sw try self.tokenizer.peek();
            } else if (next != .identifier) return self.errorUnexpectedToken(next);
            expr = try refine_func(self, .{ .member_access = .{
                .target = try self.createVal(expr),
                .member_name = next.identifier,
            } });
            continue :sw try self.tokenizer.peek();
        },
        else => return expr,
    }

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
            .target = try self.createVal(try refine_func(self, try self.parseExpressionSide(false, access))),
        } },
        .identifier => |id| .{ .identifier = id },
        .builtin => |builtin| .{ .builtin = bi.getBuiltin(builtin) catch |err| return self.errorOut(err) },
        .@"[" => blk: {
            //if array len is not a value return @Array(len, T)
            if (try self.tokenizer.peek() == .@"]") {
                self.tokenizer.skip();
                const component_type_expr = try self.refine(try self.parseExpressionSide(false, true));
                break :blk (Type{ .runtime_array = try self.createVal(try self.asTypeCreate(component_type_expr)) }).asExpr();
            }
            const array_len_expr = try self.parseExpression(squareBracketShouldStop, true);
            if (array_len_expr != .value) @panic("return @Array(len, T) since len is not comptime");

            var len_val = array_len_expr.value;
            len_val = sw: switch (len_val.type) {
                .compfloat => {
                    len_val = try ct.implicitCastValue(self, len_val, .compint);
                    continue :sw .compint;
                },
                .compint => if ((len_val.payload.wide >> 32) != 0)
                    return self.errorOut(Error.InvalidArrayLen)
                else
                    .{ .type = tp.u32_type, .payload = .{ .wide = len_val.payload.wide } },

                .scalar => |scalar| if (scalar.type == .float or (len_val.payload.wide >> 32) != 0)
                    return self.errorOut(Error.InvalidArrayLen)
                else
                    .{ .type = tp.u32_type, .payload = .{ .wide = len_val.payload.wide } },

                else => return self.errorOut(Error.InvalidArrayLen),
            };
            const len: u32 = util.extract(u32, len_val.payload.wide);
            if (len < 2) return self.errorOut(Error.InvalidArrayLen);

            const component_type_expr = try self.refine(try self.parseExpressionSide(false, true));
            break :blk (Type{ .array = .{
                .len = len,
                .component = try self.createVal(try self.asTypeCreate(component_type_expr)),
            } }).asExpr();
        },
        .@"(" => try self.parseExpression(bracketShouldStop, true),
        .@"." => switch (try self.tokenizer.next()) {
            .@"{" => try refine_func(self, try self.parseCastOrConstructor(.unknownempty)),
            .identifier => |identifier| .{ .enum_literal = identifier },
            else => return self.errorOut(Error.UnexpectedToken),
        },
        .@"fn" => try self.parseFunctionTypeOrValue(),
        .entrypoint => blk: {
            const res = try self.parseEntryPointTypeOrValue();
            if (try self.tokenizer.peek() == .@"}") self.tokenizer.skip();
            break :blk res;
        },
        .@"struct" => .{ .value = .{
            .type = .type,
            .payload = .{ .type = .{ .@"struct" = try self.parseStructDeclaration() } },
        } },
        else => .{ .value = try self.parseValue(token) },
    };
}
fn parseStructDeclaration(self: *Parser) Error!StructID {
    if (try self.tokenizer.next() != .@"{") return self.errorOut(Error.UnexpectedToken);

    const id: StructID = @enumFromInt(self.structs.items.len);
    try self.structs.append(self.arena.allocator(), Struct.new(id));
    const s = self.getStructFromID(id);
    s.scope.parent = self.current_scope;

    if (self.current_scope.current_variable_value_index < self.current_scope.multi_variable_initialization.len) {
        const vd = self.current_scope.multi_variable_initialization[self.current_scope.current_variable_value_index];
        // std.debug.print("VDNAME: {s}, {f}\n", .{ vd.name, vd.type });
        // s.name = vd.name;
        s.name = if (vd.type == .@"struct") vd.name else try self.createIntermediateVariableName();
    } else s.name = try self.createIntermediateVariableName();

    if (try self.tokenizer.peek() == .@"}") {
        self.tokenizer.skip();
        return id;
    }
    try self.parseScope(&s.scope);

    return id;
}

fn parseFunctionCall(self: *Parser, callee: Expression, prepend: ?Expression) Error!Call {
    const @"type" = try self.typeOf(callee);

    const callee_ptr = try self.createVal(callee);
    const args = try self.parseExpressionSequence(prepend, .@")");

    if (@"type" == .unknown) return .{ .callee = callee_ptr, .args = args };
    if (@"type" != .function) return self.errorOut(Error.InvalidCall);
    const ft = @"type".function;

    if (ft.arg_types.len != args.len) return self.errorOut(Error.NonMatchingArgumentCount);
    for (ft.arg_types, args) |t, *arg| arg.* = try self.implicitCast(arg.*, t);
    return .{ .callee = callee_ptr, .args = args };
}

fn parseCastOrConstructor(self: *Parser, @"type": Type) Error!Expression {
    const is_struct = if (@"type".isEmpty()) blk: {
        try self.skipEndl();
        const next3 = try self.tokenizer.peekArray(3);
        break :blk next3[0] == .@"." and next3[1] == .identifier and next3[2] == .@"=";
    } else @"type" == .@"struct";

    if (is_struct) return try self.parseStructConstructor(@"type");

    const components = try self.parseExpressionSequence(null, .@"}");

    if (components.len == 0)
        return if (@"type" == .@"struct" or @"type" == .unknown)
            .{ .struct_constructor = .{ .type = @"type", .members = &.{} } }
        else
            self.errorOut(Error.InvalidConstructor);

    return if (components.len == 1) .{ .cast = .{
        .type = @"type",
        .expr = &components[0],
    } } else .{ .constructor = .{ .type = @"type", .components = components } };
}

fn parseStructConstructor(self: *Parser, @"type": Type) Error!Expression {
    var list: List(NameExpr) = .empty;
    var last: enum { open, elem, comma } = .open;
    sw: switch (try self.tokenizer.peek()) {
        .@"." => {
            last = .elem;
            self.tokenizer.skip();
            const next = try self.tokenizer.next();
            if (next != .identifier) return self.errorOut(Error.UnexpectedToken);

            if (try self.tokenizer.next() != .@"=") return self.errorOut(Error.UnexpectedToken);
            try list.append(self.arena.allocator(), .{
                .name = next.identifier,
                .expr = try self.parseExpression(constructorShouldStop, false),
            });
            continue :sw try self.tokenizer.peek();
        },
        .@"}" => {
            self.tokenizer.skip();
            try self.skipEndl();
            break :sw;
        },
        .@"," => {
            if (last != .elem) return self.errorOut(Error.UnexpectedToken);
            last = .comma;
            self.tokenizer.skip();
            try self.skipEndl();
            continue :sw try self.tokenizer.peek();
        },
        else => {
            if ((self.tokenizer.peek() catch unreachable) == .endl) {
                self.tokenizer.skip();
                continue :sw try self.tokenizer.peek();
            }
            return self.errorOut(Error.UnexpectedToken);
        },
    }

    return .{ .struct_constructor = .{
        .type = @"type",
        .members = try list.toOwnedSlice(self.arena.allocator()),
    } };
}
fn parseExpressionSequence(self: *Parser, prepend: ?Expression, comptime until: Token) Error![]Expression {
    const should_stop = struct {
        pub fn f(parser: *Parser, token: Token) Error!bool {
            // return std.meta.eql(token, until) or token == .@",";
            return std.meta.eql(token, until) or token == .@"," or try defaultShouldStop(parser, token);
        }
    }.f;
    var list: List(Expression) = .empty;
    if (prepend) |p| try list.append(self.arena.allocator(), p);

    try self.skipEndl();
    var token = try self.tokenizer.peek();
    while (!std.meta.eql(token, until)) {
        const expr = try self.parseExpression(should_stop, false);
        try list.append(self.arena.allocator(), expr);
        if (try self.tokenizer.peek() == .@",") self.tokenizer.skip();

        try self.skipEndl();

        token = try self.tokenizer.peek();
    }
    self.tokenizer.skip();
    return try list.toOwnedSlice(self.arena.allocator());
}
fn skipEndl(self: *Parser) Error!void {
    if (try self.tokenizer.peek() == .endl) self.tokenizer.skip();
}

fn parseFunctionTypeOrValue(self: *Parser) Error!Expression {
    var mode: enum { unsure, type, value } = .value;
    var peek = try self.tokenizer.peek();
    //fn <==> fn()void
    if (try defaultShouldStop(self, peek)) return .{ .value = .{
        .type = .type,
        .payload = .{ .type = .{ .function = .{
            .rtype = &.void,
            .arg_types = &.{},
        } } },
    } };

    var arg_types: List(Type) = .empty;
    var set_index: ?usize = null;
    var arg_names: List([]const u8) = .empty;
    var function_decl_scope: FunctionDeclarationScope = .new(self.current_scope, &arg_types.items, &arg_names.items);
    self.current_scope = &function_decl_scope.scope;
    defer self.current_scope = function_decl_scope.scope.parent;

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
                        _ = self.global_scope.scope.getVariableReference(self, name) catch break :blk;
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
    const rtype = try self.asTypeCreate(try self.refine(try self.parseExpressionSide(true, true)));

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

const FunctionDeclarationScope = struct {
    scope: Scope,
    arg_types: *[]Type,
    arg_names: *[][]const u8,

    pub fn new(parent: *Scope, arg_types: *[]Type, arg_names: *[][]const u8) FunctionDeclarationScope {
        return .{
            .arg_types = arg_types,
            .arg_names = arg_names,

            .scope = .{
                .parent = parent,
                .getVariableReferenceFn = getVariableReferenceFn,
                .addStatementFn = addStatementFn,
                .trackReferenceFn = trackReferenceFn,
                .bodyFn = bodyFn,

                .result_type = .unknownempty,
            },
        };
    }
    fn bodyFn(_: *Scope) *[]Statement {
        unreachable;
    }
    fn getVariableReferenceFn(scope: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
        const f: *FunctionDeclarationScope = @fieldParentPtr("scope", scope);
        return for (f.arg_names.*, 0..) |arg_name, i| {
            if (util.strEql(name, arg_name)) break .{
                .is_mutable = false,
                .type = f.arg_types.*[i],
                .value = .{ .identifier = name },
            };
        } else try scope.parent.getVariableReference(parser, name);
    }

    // fn getVariableReferenceFn(scope: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
    //     const s: *Struct = @fieldParentPtr("scope", scope);
    //     return for (s.body.items) |*statement| {
    //         if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name))
    //             break statement.var_decl.variableReference();
    //     } else if (util.strEql(name, s.name)) .{
    //         .is_mutable = true,
    //         .type = .type,
    //         .value = .{ .value = .{
    //             .type = .type,
    //             .payload = .{ .type = .{ .@"struct" = s.id } },
    //         } },
    //     } else if (!parser.isScopeGlobal(scope))
    //         try parser.global_scope.scope.getVariableReference(parser, name)
    //     else
    //         parser.errorUndeclaredVariable(name);
    // }

    fn addStatementFn(_: *Scope, _: *Parser, _: Statement) Error!void {}
    fn trackReferenceFn(_: *Scope, _: []const u8, _: Scope.DeclReferenceType) Error!void {}
};

fn parseEntryPointTypeOrValue(self: *Parser) Error!Expression {
    if (try self.tokenizer.next() != .@"(") return self.errorOut(Error.UnexpectedToken);
    const stage_enum_expr = try self.implicitCast(try self.parseExpressionSide(false, true), bi.stage_type);
    if (stage_enum_expr != .value) return self.errorOut(Error.IncompleteStatement);

    const exec_model: Stage = @enumFromInt(@as(u64, @truncate(stage_enum_expr.value.payload.wide)));
    const exec_model_info: StageInfo = switch (exec_model) {
        .vertex => .vertex,
        .fragment => .fragment,
        .compute => .{ .compute = @splat(0) },
    };

    const token = try self.tokenizer.next();
    if (token != .@")") return self.errorOut(Error.InvalidInput);

    const ep_type: Type = .{ .entrypoint = std.meta.activeTag(exec_model_info) };

    if (try self.tokenizer.peek() != .@"{") return ep_type.asExpr();
    self.tokenizer.skip();

    var entry_point: EntryPoint = .new(self, exec_model_info);
    try self.parseScope(&entry_point.scope);

    entry_point.global_io_count = self.global_scope.global_io.items.len;

    return .{ .value = .{
        .type = ep_type,
        .payload = .{ .ptr = @ptrCast(@alignCast(try self.createVal(entry_point))) },
    } };
}

fn parseScope(self: *Parser, scope: *Scope) Error!void {
    debug.p = self; //TODO: REMOVE

    const last_scope = self.current_scope;
    self.current_scope = scope;
    defer self.current_scope = last_scope;
    while (true) {
        const added_statement_count = try self.parseStatement();
        if (added_statement_count == 0) break;

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
    // if (true) return;

    const body = scope.body();
    var i: usize = 0;
    while (i < body.len) {
        const index = body.len - 1 - i;
        const statement = body.*[index];
        if (shouldUntrackInStatement(statement))
            try self.trackReferencesInStatement(statement, .untrack);
        if (try self.isStatementOmittable(statement) //
        // and false //
        ) {
            util.removeAt(Statement, body, index);
        } else i += 1;
    }
}
fn shouldUntrackInStatement(statement: Statement) bool {
    return switch (statement) {
        .var_decl => |var_decl| !var_decl.qualifier.isIO() and var_decl.reference_count == 0,
        else => false,
    };
}
pub fn getStructFromID(self: *Parser, id: StructID) *Struct {
    return &self.structs.items[@intFromEnum(id)];
}

pub const StructID = enum(u32) { self = ~@as(u32, 0), _ };
pub const StructMember = struct {
    name: []const u8,
    type: Type,
    default_value: Expression = .empty,
};
pub const Struct = struct {
    name: []const u8 = "",
    members: List(StructMember) = .empty,
    scope: Scope,

    body: List(Statement) = .empty,
    global_io: List([]const u8) = .empty,

    id: StructID,

    pub fn new(id: StructID) Struct {
        return .{
            .id = id,
            .scope = .{
                .getVariableReferenceFn = getVariableReferenceFn,
                .addStatementFn = addStatementFn,
                .trackReferenceFn = trackReferenceFn,
                .bodyFn = bodyFn,

                .result_type = .unknownempty,
            },
        };
    }
    pub fn memberIndex(self: *Struct, name: []const u8) ?u32 {
        return for (self.members.items, 0..) |m, i| (if (util.strEql(m.name, name)) break @truncate(i)) else null;
    }
    pub fn getMemberType(self: *Struct, name: []const u8) Error!Type {
        return self.members.items[self.memberIndex(name) orelse return Error.NoMemberWithName].type;
    }
    fn bodyFn(scope: *Scope) *[]Statement {
        return &@as(*@This(), @fieldParentPtr("scope", scope)).body.items;
    }
    fn addStatementFn(scope: *Scope, parser: *Parser, statement: Statement) Error!void {
        const s: *Struct = @fieldParentPtr("scope", scope);
        if (statement != .var_decl) return parser.errorOut(Error.UnexpectedStatement);

        if (!(try parser.isStatementComplete(statement))) return parser.errorIncompleteStatement(statement);
        if (statement.var_decl.qualifier == .member) {
            try s.members.append(parser.arena.allocator(), .{
                .name = statement.var_decl.name,
                .type = statement.var_decl.type,
                .default_value = statement.var_decl.initializer,
            });
        } else {
            try s.body.append(parser.arena.allocator(), statement);
            if (statement.var_decl.qualifier.isIO())
                try s.global_io.append(parser.arena.allocator(), statement.var_decl.name);
        }
    }
    pub fn getVariableReference(self: *Struct, parser: *Parser, name: []const u8) Error!VariableReference {
        return try self.scope.getVariableReference(parser, name);
    }

    fn getVariableReferenceFn(scope: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
        const s: *Struct = @fieldParentPtr("scope", scope);
        if (util.strEql(name, s.name)) {
            return .{
                .is_mutable = false,
                .type = .type,
                .value = (Type{ .@"struct" = s.id }).asExpr(),
            };
        }
        return for (s.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name))
                break statement.var_decl.variableReference();
        } else if (util.strEql(name, s.name)) .{
            .is_mutable = true,
            .type = .type,
            .value = .{ .value = .{
                .type = .type,
                .payload = .{ .type = .{ .@"struct" = s.id } },
            } },
        } else if (!parser.isScopeGlobal(scope))
            try parser.global_scope.scope.getVariableReference(parser, name)
        else
            parser.errorUndeclaredVariable(name);
    }
    fn trackReferenceFn(scope: *Scope, name: []const u8, ref_type: Scope.DeclReferenceType) Error!void {
        const s: *Struct = @fieldParentPtr("scope", scope);
        for (s.body.items) |*statement| {
            if (statement.* == .var_decl and util.strEql(statement.var_decl.name, name)) {
                statement.var_decl.reference_count = if (ref_type == .track) statement.var_decl.reference_count + 1 else statement.var_decl.reference_count -| 1;
                return;
            }
        }
        return Error.UndeclaredVariable;
    }
};

pub const Function = struct {
    arg_names: []const []const u8,
    type: tp.FunctionType,

    //not dependant on nor mutates any global variables
    builtin_handle: ?bi.BuiltinFunction = null,
    is_pure: bool = true,

    body: List(Statement) = .empty,
    scope: Scope = undefined,

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
        if (statement == .var_decl) switch (statement.var_decl.qualifier) {
            .mut, .@"const" => {},
            else => return parser.errorOut(Error.IncompleteStatement),
        };
        if (!(try parser.isStatementComplete(statement))) return parser.errorIncompleteStatement(statement);
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
    shader_stage_info: StageInfo,
    scope: Scope,

    global_io_count: usize = 0,
    io: List([]const u8) = .empty,

    body: List(Statement),

    pub fn new(self: *Parser, stage_info: StageInfo) EntryPoint {
        return .{
            .shader_stage_info = stage_info,
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
        if (statement == .var_decl and statement.var_decl.qualifier == .member)
            return parser.errorOut(Error.IncompleteStatement);
        if (statement == .var_decl and statement.var_decl.qualifier.isIO())
            try entry_point.io.append(parser.arena.allocator(), statement.var_decl.name);
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

pub const Stage = root.Stage;
pub const StageInfo = root.StageInfo;
fn parseValue(self: *Parser, token: Token) Error!Value {
    //true, false
    return switch (token) {
        .true, .false => .{ .type = .bool, .payload = .{ .wide = util.fit(WIDE, token == .true) } },
        .compint => |compint| .{ .type = .compint, .payload = .{ .wide = util.fit(WIDE, compint) } },
        .compfloat => |compfloat| .{ .type = .compfloat, .payload = .{ .wide = util.fit(WIDE, compfloat) } },
        .type_literal => |tl| .{ .type = .type, .payload = .{ .type = tl } },
        else => return self.errorUnexpectedToken(token),
    };
}

pub const Expression = union(enum) {
    enum_literal: []const u8,

    call: Call,
    identifier: []const u8,
    named_value: NamedValue,

    builtin: bi.Builtin,

    value: Value,

    bin_op: BinOp,
    u_op: UOp,

    swizzle: Swizzle,
    member_access: MemberAccess,
    method_call: void, //?
    indexing: Indexing,

    cast: Cast,
    constructor: Constructor,
    struct_constructor: StructConstructor,

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
pub fn asType(self: *Parser, expr: *const Expression) Error!Type {
    const unknown: Type = .{ .unknown = expr };
    return switch (expr.*) {
        .named_value => |named_value| if (named_value.value.type == .type) named_value.value.payload.type else unknown,
        .value => |value| if (value.type == .type) value.payload.type else unknown,
        .identifier => |identifier| blk: {
            const var_ref = try self.current_scope.getVariableReference(self, identifier);
            break :blk if (var_ref.type == .type and var_ref.value == .value) var_ref.value.value.payload.type else unknown;
        },
        // .builtin => @panic("TODO: asType of builtin"),
        else => unknown,
    };
}
pub fn isExpressionMutable(self: *Parser, expr: Expression) Error!bool {
    return switch (expr) {
        .identifier => |identifier| (try self.current_scope.getVariableReference(self, identifier)).is_mutable,
        .indexing => |indexing| try self.isExpressionMutable(indexing.target.*),
        .builtin => |builtin| if (builtin == .variable) builtin.variable.isMutable() else false,
        else => false,
    };
}
pub const StructConstructor = struct {
    type: Type,
    members: []NameExpr,
};

pub const NameExpr = struct { name: []const u8, expr: Expression };
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

pub const Scope = struct {
    addStatementFn: *const fn (*Scope, *Parser, Statement) Error!void = undefined,
    getVariableReferenceFn: *const fn (*Scope, *Parser, []const u8) Error!VariableReference = undefined,
    trackReferenceFn: *const fn (*Scope, []const u8, DeclReferenceType) Error!void = undefined,
    bodyFn: *const fn (*Scope) *[]Statement = undefined,

    parent: *Scope = undefined,
    result_type: Type = .void,

    multi_variable_initialization: []VariableDeclaration = &.{},
    current_variable_value_index: usize = 0,

    pub const DeclReferenceType = enum { track, untrack };
    pub fn body(self: *Scope) *[]Statement {
        return self.bodyFn(self);
    }
    pub fn addStatement(self: *Scope, parser: *Parser, statement: Statement) Error!void {
        try self.addStatementFn(self, parser, statement);
    }

    pub fn getVariableReference(self: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
        return self.getVariableReferenceFn(self, parser, name) catch |err| switch (err) {
            Error.UndeclaredVariable => try self.getMultivarableInitializationReference(parser, name),
            else => return err,
        };
    }
    pub fn getMultivarableInitializationReference(self: *Scope, parser: *Parser, name: []const u8) Error!VariableReference {
        if (self.multi_variable_initialization.len == 0) return parser.errorOut(Error.UndeclaredVariable);
        return for (self.multi_variable_initialization) |*vd| {
            if (util.strEql(name, vd.name)) break vd.variableReference();
        } else if (!parser.isScopeGlobal(self)) try self.parent.getMultivarableInitializationReference(parser, name) else parser.errorOut(Error.UndeclaredVariable);
    }

    pub fn trackReference(self: *Scope, name: []const u8, ref_type: DeclReferenceType) Error!void {
        return self.trackReferenceFn(self, name, ref_type) catch |err| switch (err) {
            Error.UndeclaredVariable => (for (self.multi_variable_initialization) |*vd| (if (util.strEql(name, vd.name)) break) else return Error.UndeclaredVariable),
            else => return err,
        };
    }
};
pub const VariableReference = struct {
    is_mutable: bool,
    type: Type,
    value: Expression,

    is_omittable: bool = false,
};

fn parseQualifier(self: *Parser, token: Token) Error!Qualifier {
    return switch (token) {
        .in, .out => blk: {
            const smooth: Qualifier = if (token == .in) .{ .in = .smooth } else .{ .out = .smooth };
            if (try self.tokenizer.peek() != .@"(") break :blk smooth;
            self.tokenizer.skip();
            if (try self.tokenizer.peek() == .@")") {
                self.tokenizer.skip();
                break :blk smooth;
            }
            var interpolation: bi.Interpolation = .smooth;
            const expr = try self.implicitCast(try self.parseExpressionSide(false, true), bi.interpolation_type);
            if (expr != .value) return self.errorOut(Error.IncompleteStatement);

            interpolation = @enumFromInt(util.extract(u64, expr.value.payload.wide));
            break :blk if (token == .in) .{ .in = interpolation } else .{ .out = interpolation };
            // .{ .in = .smooth },
        },
        .member => .member,
        .@"const" => .@"const",
        .mut => .mut,
        .descriptor => blk: {
            if (try self.tokenizer.peek() != .@"(") break :blk .{ .descriptor = .{} };
            self.tokenizer.skip();
            if (try self.tokenizer.peek() == .@")") {
                self.tokenizer.skip();
                break :blk .{ .descriptor = .{} };
            }
            const set_expr = try self.implicitCast(try self.parseExpression(argShouldStop, false), tp.u32_type);
            if (set_expr != .value) return self.errorOut(Error.IncompleteStatement);
            const set: u32 = util.extract(u32, set_expr.value.payload.wide);

            if (try self.tokenizer.next() == .@")") break :blk .{ .descriptor = .{ .set = set } };

            const binding_expr = try self.implicitCast(try self.parseExpression(argShouldStop, true), tp.u32_type);
            if (set_expr != .value) return self.errorOut(Error.IncompleteStatement);
            break :blk .{ .descriptor = .{
                .set = set,
                .binding = util.extract(u32, binding_expr.value.payload.wide),
            } };
        },
        .push => .push,
        .shared => .shared,
        else => Error.UnexpectedToken,
    };
}

pub const DescriptorBinding = struct {
    set: u32 = 0,
    binding: ?u32 = null,
};

pub const Qualifier = union(enum) {
    member,
    @"const",
    mut,

    descriptor: DescriptorBinding,
    push,

    in: bi.Interpolation,
    out: bi.Interpolation,

    shared,
    pub fn isIO(self: Qualifier) bool {
        return switch (self) {
            .push, .descriptor, .in, .out => true,
            else => false,
        };
    }
    pub fn canHaveInitializer(self: Qualifier) bool {
        return switch (self) {
            .in, .push, .descriptor, .member => false,
            else => true,
        };
    }
    pub fn isMutable(self: Qualifier) bool {
        return switch (self) {
            .mut, .shared, .out, .member => true,
            .@"const", .in, .descriptor, .push => false,
        };
    }
};

pub const Value = struct {
    type: Type,
    payload: ValuePayload = undefined,
    pub const format = debug.formatValue;
    pub fn shouldBeNamed(self: Value) bool {
        return switch (self.type) {
            .function => @as(*const Parser.Function, @ptrCast(@alignCast(self.payload.ptr))).builtin_handle == null,
            // .type => self.payload.type == .@"struct" or self.payload.type == .@"enum",
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
pub fn errorIncompleteStatement(self: *Parser, statement: Statement) Error {
    return self.errorOutFmt(
        Error.IncompleteStatement,
        "Statement debug representation:\n {f}",
        .{statement},
    );
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
pub fn isScopeGlobal(self: *Parser, scope: *Scope) bool {
    return @intFromPtr(scope) == @intFromPtr(&self.global_scope.scope);
}
pub fn inGlobalScope(self: *Parser) bool {
    return self.isScopeGlobal(self.current_scope);
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
