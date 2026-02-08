const std = @import("std");
const util = @import("util.zig");
const zigbuiltin = @import("builtin");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");
const error_message = @import("errorMessage.zig");
const hgsl = @import("root.zig");

tokenizer: Tokenizer = undefined,
allocator: Allocator,

types: List(TypeEntry) = .empty,

scalar_values: List(u128) = .empty,
composite_values: List(u8) = .empty,

functions: List(FunctionEntry) = .empty,
structs: List(StructEntry) = .empty,

scopes: List(ScopeEntry) = .empty,
current_scope: Scope = .entry_file,

token: Token = 0,

error_info: ErrorInfo = .unknown,

pub fn dump(self: *Parser) void {
    std.debug.print("types:\n", .{});
    for (self.types.items) |t| std.debug.print("--- {any}\n", .{t});
    std.debug.print("structs:\n", .{});
    for (self.structs.items) |t| std.debug.print("--- {any}\n", .{t});
    // std.debug.print("entry_points:\n", .{});
    // for (self.entry_points.items) |t| std.debug.print("--- {any}\n", .{t});

    std.debug.print("---BODY: \n", .{});
    self.current_scope = .entry_file;
    self.dumpCurrentScope(true);
}

fn dumpCurrentScope(self: *Parser, full: bool) void {
    const scope = self.getScope(self.current_scope);
    if (full)
        for (scope.body.items) |node| std.debug.print("\t{any}\n", .{node});
    var node: Node = 0;
    while (node < scope.body.items.len) {
        std.debug.print("\t{f}\n", .{FatNodeEntry{
            .self = self,
            .node = &node,
        }});
    }
}

pub fn new(allocator: Allocator) Error!Parser {
    const types_initial_capacity = 16;
    const scalar_values_initial_capacity = 16;
    const composite_values_initial_capacity = 8 * 4 * 4;
    var self: Parser = .{
        .allocator = allocator,
    };
    self.types = try .initCapacity(allocator, types_initial_capacity);
    self.scalar_values = try .initCapacity(allocator, scalar_values_initial_capacity);
    self.composite_values = try .initCapacity(allocator, composite_values_initial_capacity);

    self.types.appendAssumeCapacity(.type);

    return self;
}

pub fn parse(self: *Parser, tokenizer: Tokenizer) Error!void {
    self.tokenizer = tokenizer;

    const s = try self.parseFile(tokenizer);
    self.current_scope = self.getStruct(s).scope;
    try self.foldScope(self.current_scope);
}
fn foldScope(self: *Parser, scope: Scope) Error!void {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = scope;

    if (self.getScope(self.current_scope).container.isDecl())
        try self.foldDeclScope()
    else
        try self.foldBlockScope();
}
fn foldBlockScope(self: *Parser) Error!void {
    const body = self.getScope(self.current_scope).body.items;

    var node_buf: [32]u32 = undefined;
    var statements: List(Node) = .initBuffer(&node_buf);
    defer if (statements.capacity > node_buf.len) statements.deinit(self.allocator);
    var i: Node = 0;
    while (i < body.len) {
        try statements.append(self.allocator, i);
        i += self.nodeLength(i);
    }

    for (0..statements.items.len) |index| {
        const node = statements.items[statements.items.len - index - 1];
        if (self.isStatementSignificant(node))
            try self.foldNode(node);
    }
}
fn foldDeclScope(self: *Parser) Error!void {
    const body = self.getScope(self.current_scope).body.items;
    var i: Node = 0;
    while (i < body.len) {
        if (self.isStatementSignificant(i))
            try self.foldNode(i);
        i += self.nodeLength(i);
    }
}

fn isStatementSignificant(self: *Parser, node: Node) bool {
    return switch (self.getNodeEntry(node).*) {
        .var_decl => |var_decl| util.enumInRange(Qualifier, var_decl.qualifier, .vertex, .compute),
        else => false,
    };
}
fn foldNode(self: *Parser, node: Node) Error!void {
    const entry = self.getNodeEntry(node);
    switch (entry.*) {
        .var_decl => |var_decl| {
            //check for redelaration
            std.debug.print("FOLD VARIABLE: {s}\n", .{self.tokenizer.slice(var_decl.name)});
            if (try self.getVarRef(self.current_scope, node, var_decl.name)) |_|
                return self.errorOut(.{ .redeclaration = var_decl.name });

            // if(try self.getVarRef(scope: Scope, node: u32, token: u32)
            const qualifier_info_node_len = self.nodeLength(node + 1);
            const type_node = node + 1 + qualifier_info_node_len;
            try self.foldNode(type_node);

            const type_node_len = self.nodeLength(type_node);
            const initializer_node = type_node + type_node_len;
            try self.foldNode(initializer_node);

            const t: Type = undefined;
            if (self.getNodeEntry(type_node).* != .null) {
                if (!self.isNodeTypeValue(type_node))
                    return self.errorOut(.{ .not_a_type = type_node });

                try self.implicitCast(
                    initializer_node,
                    @enumFromInt(self.getNodeEntry(type_node).value.payload),
                );
            } else {
                self.getNodeEntry(type_node).* = .{ .value = .{
                    .type = .type,
                    .payload = @intFromEnum(try self.typeOf(initializer_node)),
                } };
            }
            if (!self.isQualifierCompatibleWithType(var_decl.qualifier, node + 1, t))
                return self.errorOut(.{ .qualifier_incompatible_with_type = .{
                    .var_decl = node,
                    .type = t,
                } });
        },
        .function_decl => |fn_decl| {
            try self.foldScope(self.getFunction(fn_decl).scope);
        },
        .identifier => |token| if (try self.getVarRef(self.current_scope, node, token)) |vr| {
            entry.* = .{ .var_ref = vr };
        } else return self.errorOut(.{ .undeclared_identifier = token }),
        else => {},
    }
}
fn isQualifierCompatibleWithType(self: *Parser, qualifier: Qualifier, q_info: Node, @"type": Type) bool {
    _ = .{ qualifier, q_info, @"type", self };
    return true;
}
fn getVarRef(self: *Parser, scope: Scope, node: Node, token: Token) Error!?VariableReference {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = scope;

    const scope_entry = self.getScope(scope);
    const current = if (scope_entry.container == .@"struct")
        try self.getDeclScopeVarRef(scope, node, token)
    else
        try self.getDeclScopeVarRef(scope, node, token);

    std.debug.print("FOUND IN CURRENT SCOPE : {}, name: {s}\n", .{ current != null, self.tokenizer.slice(token) });
    return if (current) |c| .{ .node = c, .scope = scope } else //
    if (self.isScopeFile(scope)) null else //
    try self.getVarRef(scope_entry.parent, scope_entry.getDeclNode(self), token);
}

fn getDeclScopeVarRef(self: *Parser, scope: Scope, node: Node, token: Token) Error!?Node {
    const body = self.getScope(scope).body.items;
    var i: Node = 0;
    while (i < body.len) {
        const len = self.nodeLength(i);
        defer i += len;

        if (i == node) continue;
        switch (self.getNodeEntry(i).*) {
            .var_decl => |vd| if (vd.name == token or util.strEql(
                self.tokenizer.slice(token),
                self.tokenizer.slice(vd.name),
            )) {
                try self.foldNode(i);
                return i;
            },
            else => {},
        }
    }
    return null;
}
fn implicitCast(self: *Parser, node: Node, @"type": Type) Error!void {
    _ = .{ self, node, @"type" };
}
fn isNodeTypeValue(self: *Parser, node: Node) bool {
    const entry = self.getNodeEntry(node).*;
    return entry == .value and entry.value.type == .type;
}

fn typeOf(self: *Parser, node: Node) Error!Type {
    return switch (self.getNodeEntry(node).*) {
        .value => |value| value.type,
        .bin_op => |op| try self.typeOfBinOp(
            op,
            try self.typeOf(node + 1),
            try self.typeOf(node + 1 + self.nodeLength(node + 1)),
        ),
        .u_op => |op| try self.typeOfUOp(op, try self.typeOf(node + 1)),
        .var_ref => |vr| @enumFromInt(self.getNodeEntryScope(vr.scope, vr.node + 1 + self.nodeLength(vr.node + 1)).value.payload),

        .function_decl => |fn_decl| try self.typeOfFunctionDecl(node, fn_decl),

        else => |e| {
            std.debug.print("idk type of '{s}'\n", .{@tagName(e)});
            return self.errorOut(.unknown);
        },
    };
}
fn typeOfFunctionDecl(self: *Parser, node: Node, function: Function) Error!Type {
    const entry = self.getFunction(function);
    if (entry.arg_count != 0) @panic("type of function decl with args!");

    const arg_offset: Node = 0;
    const prev_types_len = self.types.items.len;

    const type_header = try self.appendType(.{ .function = entry.arg_count });
    const rtype = try self.asType(node + 1 + arg_offset, true);
    const only_references = self.getType(rtype) == .ref;
    if (!only_references) return type_header;

    if (self.findFunctionType(type_header)) |ft| {
        self.types.items.len = prev_types_len;
        return ft;
    } else return type_header;
}
fn findFunctionType(self: *Parser, ft: Type) ?Type {
    const arg_count = self.getType(ft).function;
    return for (self.types.items[0..@intFromEnum(ft)], 0..) |entry, i| {
        var target_arg_offset: u32 = 0;
        if (entry == .function and entry.function == arg_count and for (0..arg_count + 1) |j| {
            const arg_id, const target_arg_id = .{ @intFromEnum(ft) + j + 1, i + target_arg_offset + 1 };
            const arg = self.types.items[arg_id];
            const target_arg = self.types.items[target_arg_id];
            defer target_arg_offset += self.typeLength(@enumFromInt(target_arg_id));

            if (arg.ref != @as(Type, @enumFromInt(target_arg_id)) and (target_arg != .ref or arg.ref != target_arg.ref))
                break false;
        } else true) return @enumFromInt(i);
    } else null;
}

fn asType(self: *Parser, node: Node, comptime create_ref: bool) Error!Type {
    return switch (self.getNodeEntry(node).*) {
        .value => |value| blk: {
            if (value.type != .type) return self.errorOut(.{ .invalid_type = node });
            break :blk if (create_ref)
                try self.appendType(.{ .ref = @enumFromInt(value.payload) })
            else
                @enumFromInt(value.payload);
        },
        else => @panic("as type idk"),
    };
}
fn typeOfBinOp(self: *Parser, op: BinaryOperator, left: Type, right: Type) Error!Type {
    _ = .{ self, right };
    return switch (op) {
        else => left,
    };
}
fn typeOfUOp(self: *Parser, op: UnaryOperator, operand: Type) Error!Type {
    _ = self;
    return switch (op) {
        else => operand,
    };
}
fn nodeLength(self: *Parser, node: Node) u32 {
    var base: u32 = switch (self.getNodeEntry(node).*) {
        .var_decl => 1 + self.nodeSeqenceLength(node + 1, 3),
        .bin_op => 1 + self.nodeSeqenceLength(node + 1, 2),
        .u_op => 1 + self.nodeLength(node + 1),
        .null => 1,
        .function_decl => 1 + self.nodeLength(node + 1), //args
        else => 1,
    };
    const body = self.getScope(self.current_scope).body.items;
    while (node + base < body.len and body[node + base] == .pad) base += 1;
    return base;
}
fn nodeSeqenceLength(self: *Parser, node: Node, count: usize) u32 {
    var len: u32 = 0;
    for (0..count) |_|
        len += self.nodeLength(node + len);
    return len;
}

fn parseFile(self: *Parser, tokenizer: Tokenizer) Error!Struct {
    const last_tokenizer = self.tokenizer;
    defer self.tokenizer = last_tokenizer;
    self.tokenizer = tokenizer;

    const struct_handle = try self.addStruct(.{ .is_file = true });
    const struct_type = try self.addType(.{ .@"struct" = struct_handle });
    _ = .{struct_type};

    self.getStruct(struct_handle).scope = try self.parseScope(.{
        .container = .{ .@"struct" = struct_handle },
        .parent = self.current_scope,
    });

    return struct_handle;
}

fn parseScope(self: *Parser, entry: ScopeEntry) Error!Scope {
    const scope = try self.addScope(entry);

    const last_scope = self.current_scope;
    self.current_scope = scope;
    defer self.current_scope = last_scope;

    while (true) {
        _ = self.skipEndl();
        const peek = self.tokenEntry().kind;
        if (!self.isScopeFile(self.current_scope)) {
            if (peek == .eof) return self.errorOut(.unclosed_scope);
            if (peek == .@"}") {
                self.token += 1;
                break;
            }
        } else if (peek == .eof) {
            self.token += 1;
            break;
        }

        try self.parseStatement();
    }
    return scope;
}

fn parseStatement(self: *Parser) Error!void {
    const entry = self.tokenEntry();
    self.token += 1;
    const nn = self.nextNode();
    switch (entry.kind) {
        .@"const", .@"var", .in, .out, .shared, .push, .fragment, .vertex, .compute => //
        try self.parseVarDecl(
            @enumFromInt(@intFromEnum(entry.kind) -
                @intFromEnum(Tokenizer.TokenKind.@"const")),
        ),

        else => return self.errorOut(.{ .unexpected_token = self.token - 1 }),
    }
    if (self.getScope(self.current_scope).container == .@"struct" and
        self.getNodeEntry(nn).* != .var_decl)
        return self.errorOut(.unknown);
}
fn parseVarDecl(self: *Parser, qualifier: Qualifier) Error!void {
    const name_token = self.token;
    if (self.tokenEntry().kind != .identifier)
        return self.errorOut(.{ .unexpected_token = name_token });
    self.token += 1;

    _ = try self.appendNode(.{ .var_decl = .{
        .qualifier = qualifier,
        .name = name_token,
    } });
    try self.parseQualifierInfo();

    if (self.tokenEntry().kind == .@":") {
        self.token += 1;
        _ = try self.parseExpression1();
    } else _ = try self.appendNode(.null);

    if (self.tokenEntry().kind != .@"=") {
        if (!self.defaultShouldStop(self.token))
            return self.errorOut(.{ .unexpected_token = self.token });

        _ = try self.appendNode(.null);
        if (qualifier == .@"const")
            return self.errorOut(.missing_initializer);
    } else {
        if (!qualifier.canHaveInitializer())
            return self.errorOut(.qualifier_cant_have_initializer);
        self.token += 1;
        _ = try self.parseExpression(defaultShouldStop);
    }
}
fn parseQualifierInfo(self: *Parser) Error!void {
    _ = try self.appendNode(.null); //qualifier info
}

const ShouldStopFn = fn (*Parser, Token) bool;

inline fn parseExpression(self: *Parser, should_stop_fn: *const ShouldStopFn) Error!u32 {
    return self.parseExpressionRecursive(should_stop_fn, 0);
}
fn parseExpressionRecursive(self: *Parser, should_stop_fn: *const ShouldStopFn, bp: u8) Error!u32 {
    const left_node: Node = self.nextNode();
    var left_len = try self.parseExpression1();

    var iter: usize = 0;
    while (!should_stop_fn(self, self.token)) {
        const op = if (Tokenizer.binOpFromTokenKind(self.tokenEntry().kind)) |bop|
            bop
        else
            return self.errorOut(.{ .unexpected_token = self.token });

        if (Tokenizer.bindingPower(op) <= bp) break;
        self.token += 1;

        // try self.tokenizer.skipEndl(); //?

        try self.shiftLastNNodes(left_len, 1); //shift left expression 1 to the right
        self.getScope(self.current_scope).body.items[left_node] = .{ .bin_op = op };
        iter += 1;

        left_len += 1 + try self.parseExpressionRecursive(
            should_stop_fn,
            Tokenizer.bindingPower(op),
        );
    }
    return left_len;
}
fn shiftLastNNodes(self: *Parser, count: u32, shift: usize) Error!void {
    const scope = self.getScope(self.current_scope);
    try scope.body.ensureUnusedCapacity(self.allocator, shift);
    const len = scope.body.items.len;
    scope.body.items.len += shift;
    @memmove(
        scope.body.items[len - count + shift .. len + shift],
        scope.body.items[len - count .. len],
    );
}

//returns the node list size not the amount of consumed tokens
fn parseExpression0(self: *Parser) Error!u32 {
    return switch (self.tokenEntry().kind) {
        .int_literal => blk: {
            _ = try self.appendNode(.{ .value = .{
                .type = try self.addType(.compint),
                .payload = try self.addScalarValue(
                    parseIntLiteral(self.tokenEntry().slice(self.tokenizer)),
                ),
            } });

            self.token += 1;
            break :blk 1;
        },
        // .float_literal
        .@"fn" => blk: {
            self.token += 1;
            break :blk try self.parseFunctionTypeOrDecl();
        },
        .type_literal => blk: {
            const entry = @as(Tokenizer.TypeLiteral, @bitCast(self.tokenEntry()._len)).entry();
            self.token += 1;
            const t = try self.addType(entry);
            _ = try self.appendNode(.{ .value = .{ .type = .type, .payload = @intFromEnum(t) } });
            break :blk 1;
        },
        .identifier => blk: {
            _ = try self.appendNode(.{ .identifier = self.token });
            self.token += 1;
            break :blk 1;
        },
        .true, .false => |tf| blk: {
            _ = try self.appendNode(.{ .value = .{
                .type = try self.addType(.bool),
                .payload = try self.addScalarValue(util.fit(u128, tf == .true)),
            } });
            self.token += 1;
            break :blk 1;
        },
        .neg, .pos, .not, .sat, .abs, .sqrt, .invsqrt, .norm, .mag, .sqrmag, .ptr, .indexable_ptr => |op| blk: {
            const u_op = Tokenizer.uOpFromTokenKind(op).?;
            _ = try self.appendNode(.{ .u_op = u_op });
            self.token += 1;
            break :blk 1 + try self.parseExpression1();
        },
        else => return self.errorOut(.{ .unexpected_token = self.token }),
    };
}
fn parseExpression1(self: *Parser) Error!u32 {
    return self.parseExpression0();
}
const ParseArgumentsResult = struct {
    count: u32 = 0,
    node_consumption: u32 = 0,
};
fn parseFunctionTypeOrDecl(self: *Parser) Error!u32 {
    //PARSE ARGUMENTS

    // const stack_args = 16;
    // var name_buf: [stack_args]Token = undefined;
    // var name_list: List(Token) = .initBuffer(&name_buf);

    // var type_buf: [stack_args]Node = undefined;
    // var type_list: List(Node) = .initBuffer(&type_buf);

    // var qual_buf: [stack_args * 2 / @bitSizeOf(usize)]usize = undefined;
    // var qual_list: List(usize) = .initBuffer(&qual_buf);

    // const Mode = enum {
    //     ideez, //[](Token , is_comptime)
    //     types, //[]Node
    //     args, // [] FunctionArgumentDescriptor
    // };
    // //[TYPE][TYPE] /// [DESC][TYPE][DESC][TYPE]
    // var mode: Mode = undefined;
    // s: switch (mode) {
    //     .ideez => {},
    // }
    var arg_count: u32 = 0;
    const arg_len = arg_count;
    _ = &arg_count;
    const header = try self.appendNode(.{ .function_type_decl = arg_count });

    const entry = self.tokenEntryPastEndl().kind;
    const rtype_len: u32 = switch (entry) {
        .@":" => blk: {
            self.token += 1;
            break :blk try self.parseExpression0();
        },
        .@"{" => blk: {
            _ = try self.appendNode(.{ .value = .{
                .type = .type,
                .payload = @intFromEnum(try self.addType(.void)),
            } });
            break :blk 1;
        },
        else => if (self.defaultShouldStop(self.token)) {
            _ = try self.appendNode(.{ .value = .{
                .type = .type,
                .payload = @intFromEnum(try self.addType(.void)),
            } });
            return 2 + arg_len;
        } else @panic("unexpected token in parse fn decl(bug)"),
        // return self.errorOut(.{ .unexpected_token = self.token }),
        // else => if(self.
    };
    _ = self.skipEndl();

    if (self.tokenEntry().kind == .@"{") {
        self.token += 1;
        const function = try self.addFunction(.{ .node = header, .arg_count = arg_count });
        const scope = try self.parseScope(.{
            .parent = self.current_scope,
            .container = .{ .function = function },
        });
        self.getFunction(function).scope = scope;
        self.getNodeEntry(header).* = .{ .function_decl = function };
    }
    return 1 + arg_len + rtype_len;
}
fn parseSequence(self: *Parser) Error!ParseArgumentsResult {
    _ = self.skipEndl();
    var result: ParseArgumentsResult = .{};

    while (self.tokenEntryPastEndl().kind != .@")") {
        result.node_consumption += try self.parseExpression(argumentShouldStop);
        result.count += 1;
        if (self.tokenEntry().kind == .@",") {
            self.token += 1;
            _ = self.skipEndl();
        }
    }
    self.token += if (self.tokenEntry().kind == .endl) 2 else 1;
    return result;
}

fn nextNode(self: *Parser) Node {
    return @truncate(self.getScope(self.current_scope).body.items.len);
}

fn parseIntLiteral(str: []const u8) i128 {
    return str[0] - '0';
}

fn defaultShouldStop(self: *Parser, token: Token) bool {
    const kind = self.tokenizer.entry(token).kind;
    return kind == .endl or kind == .eof or kind == .@"}";
}
fn argumentShouldStop(self: *Parser, token: Token) bool {
    const kind = self.tokenizer.entry(token).kind;
    return kind == .@")" or kind == .@",";
}
fn isScopeFile(self: *Parser, scope: Scope) bool {
    const entry = self.getScope(scope);
    return if (entry.container == .@"struct")
        self.getStruct(entry.container.@"struct").is_file
    else
        false;
}

fn skipEndl(self: *Parser) bool {
    const is_endl = self.tokenEntry().kind == .endl;
    self.token += @intFromBool(is_endl);
    return is_endl;
}

fn tokenEntryPastEndl(self: *Parser) TokenEntry {
    return if (self.tokenEntry().kind == .endl)
        self.tokenizer.entry(self.token + 1)
    else
        self.tokenEntry();
}
fn tokenEntry(self: *Parser) TokenEntry {
    return self.tokenizer.entry(self.token);
}
fn getScalarValue(self: *Parser, id: u32) u128 {
    return self.scalar_values.items[id];
}
fn getType(self: *Parser, @"type": Type) TypeEntry {
    return self.types.items[@intFromEnum(@"type")];
}
fn addType(self: *Parser, entry: TypeEntry) Error!Type {
    return self.addTypeOpt(entry, false);
}
fn addTypeOpt(self: *Parser, entry: TypeEntry, comptime create_ref: bool) Error!Type {
    if (entry != .ref)
        for (0..self.types.items.len) |i| {
            if (std.meta.eql(entry, self.getType(@enumFromInt(i))))
                return if (create_ref)
                    self.appendType(.{ .ref = @enumFromInt(i) })
                else
                    @enumFromInt(i);
        };
    return self.appendType(entry);
}
fn appendType(self: *Parser, entry: TypeEntry) Error!Type {
    const l = self.types.items.len;
    try self.types.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getNodeEntry(self: *Parser, node: Node) *NodeEntry {
    return self.getNodeEntryScope(self.current_scope, node);
}
fn getNodeEntryScope(self: *Parser, scope: Scope, node: Node) *NodeEntry {
    return &self.getScope(scope).body.items[node];
}
fn appendNode(self: *Parser, entry: NodeEntry) Error!Node {
    const scope = self.getScope(self.current_scope);
    const l: Node = @truncate(scope.body.items.len);
    try scope.body.append(self.allocator, entry);
    return l;
}

fn addScalarValue(self: *Parser, scalar: anytype) Error!u32 {
    for (self.scalar_values.items, 0..) |s, i| if (s == util.fit(u128, scalar)) return @truncate(i);
    const l: u32 = @truncate(self.scalar_values.items.len);
    try self.scalar_values.append(self.allocator, util.fit(u128, scalar));
    return l;
}

fn addScope(self: *Parser, entry: ScopeEntry) Error!Scope {
    const l = self.scopes.items.len;
    try self.scopes.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getScope(self: *Parser, handle: Scope) *ScopeEntry {
    return &self.scopes.items[@intFromEnum(handle)];
}
const Scope = enum(u32) { entry_file, _ };
pub const ScopeEntry = struct {
    body: List(NodeEntry) = .empty,
    parent: Scope = .entry_file,
    container: Container,
    const Container = union(enum) {
        @"struct": Struct,
        function: Function,
        pub fn isDecl(self: Container) bool {
            return self == .@"struct";
        }
    };
    pub fn getDeclNode(entry: ScopeEntry, self: *Parser) Node {
        return switch (entry.container) {
            .@"struct" => |s| self.getStruct(s).node,
            .function => |f| self.getFunction(f).node,
        };
    }
};

fn addStruct(self: *Parser, entry: StructEntry) Error!Struct {
    const l = self.structs.items.len;
    try self.structs.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getStruct(self: *Parser, handle: Struct) *StructEntry {
    return &self.structs.items[@intFromEnum(handle)];
}
pub const Struct = enum(u32) { entry_file = 0, _ };
pub const StructEntry = struct {
    name: Name = undefined,
    // name: []const u8 = undefined,
    scope: Scope = undefined,
    node: Node = undefined,

    len: u32 = 0,
    is_file: bool = false,
    const Name = union {
        token: Token,
        tokenizer: u32,
    };
};

fn addFunction(self: *Parser, entry: FunctionEntry) Error!Function {
    const l = self.functions.items.len;
    try self.functions.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getFunction(self: *Parser, handle: Function) *FunctionEntry {
    return &self.functions.items[@intFromEnum(handle)];
}
pub const Function = enum(u32) { _ };
pub const FunctionEntry = struct {
    scope: Scope = undefined,
    node: Node = undefined,

    name: Token = undefined,
    len: u32 = 0,

    arg_count: u32 = 0,

    stage_dependency: StageDependency = .none,
    pub const StageDependency = enum { any, none, fragment, vertex, compute };
};

// const CompositeHeader = packed struct(u8);

pub const Node = u32;
pub const NodeEntry = union(enum) {
    pad,
    null,

    identifier: Token,
    var_ref: VariableReference,

    bin_op: BinaryOperator, //[bin_op][left][right]
    u_op: UnaryOperator, //[u_op][operand]
    value: Value,

    function_decl: Function, //[fn_decl][args(2)][rtype][body...]
    fn_param: FunctionParameterDescriptor,
    //[fn_param][type / null(if anytype) / pad(if the same as the next one)]

    function_type_decl: u32, //[arg_count][arg_types][rtype]

    //is there is no else its padded
    branch,
    loop: [11]u8,

    cast: Type,
    constructor: Type,

    //statement
    var_decl: VariableDeclaration, //[var_decl][qualifier_info][type][initializer]
};
const FunctionParameterDescriptor = struct {
    name: Token,
    is_comptime: bool = false,
};
const VariableReference = struct {
    scope: Scope,
    node: Node,
};
const VariableDeclaration = struct {
    qualifier: Qualifier,
    name: Token,
};
const Qualifier = enum {
    @"const",
    @"var",
    in, //[interpolation]
    out, //[interpolation]
    push,
    shared,

    vertex,
    fragment,
    compute, //[workgroup size]
    pub fn canHaveInitializer(self: Qualifier) bool {
        return !(self == .in or self == .push);
    }
    pub fn mustHaveInitializer(self: Qualifier) bool {
        return switch (self) {
            .fragment, .vertex, .compute, .@"const" => true,
            else => false,
        };
    }
};
pub const Interpolation = enum {
    smooth,
    flat,
    noperspective,
};
const Value = struct {
    type: Type,
    payload: u32,
};

fn typeLength(self: *Parser, @"type": Type) u32 {
    return switch (self.getType(@"type")) {
        .function => |function| //
        1 + self.typeSequenceLength(@enumFromInt(@intFromEnum(@"type") + 1), function + 1),
        else => 1,
    };
}
fn typeSequenceLength(self: *Parser, @"type": Type, count: usize) u32 {
    var len: u32 = 0;
    for (0..count) |_| len += self.typeLength(@enumFromInt(@intFromEnum(@"type") + len));
    return len;
}
pub const Type = enum(u32) { type, _ };
pub const TypeEntry = union(enum) {
    void,
    type,
    bool,
    compint,
    compfloat,

    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    array: u32, //length

    device_ptr,

    @"enum": u32,
    @"struct": Struct,
    function: u32, //[function][args...][rtype]

    ref: Type,
    pub const Tag = std.meta.Tag(@This());
    pub fn format(self: TypeEntry, writer: *std.Io.Writer) !void {
        switch (self) {
            .scalar => |scalar| try writer.print("{c}{s}", .{
                "fui"[@intFromEnum(scalar.layout)],
                @tagName(scalar.width)[1..],
            }),
            .vector => |vector| try writer.print("{f}x{s}", .{
                TypeEntry{ .scalar = vector.scalar },
                @tagName(vector.len)[1..],
            }),
            .ref => |id| try writer.print("ref({d})", .{@intFromEnum(id)}),
            else => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};

pub const Matrix = packed struct {
    scalar: Scalar,
    m: Vector.Len,
    n: Vector.Len,
};
pub const Vector = packed struct {
    scalar: Scalar,
    len: Len,
    pub const Len = enum(u2) { _2, _3, _4 };
};
pub const Scalar = packed struct {
    width: Width,
    layout: Layout,
    pub const Width = enum(u2) { _8, _16, _32, _64 };
    pub const Layout = enum(u2) { float, uint, int };

    pub const all: [4 * 3 - 1]Scalar = blk: {
        const len = 4 * 3 - 1;
        var result: [len]Scalar = undefined;
        var count: usize = 0;
        for (@typeInfo(Width).@"enum".fields) |wef| {
            for (@typeInfo(Layout).@"enum".fields) |lef| {
                const s: Scalar = .{
                    .width = @as(Width, @enumFromInt(wef.value)),
                    .layout = @as(Layout, @enumFromInt(lef.value)),
                };
                if (s.width == ._8 and s.layout == .float) continue;
                result[count] = s;
                count += 1;
            }
        }
        break :blk result;
    };
    pub fn ToZig(comptime scalar: Scalar) type {
        return if (scalar.layout == .float)
            std.meta.Float(@max(16, @intFromEnum(scalar.width)))
        else
            @Int(if (scalar.layout == .int) .signed else .unsigned, @intFromEnum(scalar.width));
    }
};
pub inline fn errorOut(self: *Parser, error_info: ErrorInfo) Error {
    self.error_info = error_info;
    return error_message.errorOut(error_info);
}
//debug structs for formatting

const FatTypeEntry = struct {
    self: *Parser,
    type: Type,
    pub fn format(entry: FatTypeEntry, writer: *std.Io.Writer) !void {
        const te = entry.self.getType(entry.type);
        switch (te) {
            .function => |function| {
                try writer.print("fn(", .{});
                for (0..function) |i| try writer.print("{f}, ", .{FatTypeEntry{
                    .self = entry.self,
                    .type = @enumFromInt(@intFromEnum(entry.type) + i + 1),
                }});
                try writer.print("):{f}", .{FatTypeEntry{
                    .self = entry.self,
                    .type = @enumFromInt(@intFromEnum(entry.type) + function + 1),
                }});
            },
            .ref => |ref| try format(.{ .self = entry.self, .type = ref }, writer),
            else => try TypeEntry.format(te, writer),
        }
    }
};
const FatValueEntry = struct {
    self: *Parser,
    value: Value,
    pub fn format(entry: FatValueEntry, writer: *std.Io.Writer) !void {
        const type_entry = entry.self.getType(entry.value.type);
        switch (type_entry) {
            .compint => try writer.print("{d}", .{
                util.extract(i128, entry.self.getScalarValue(entry.value.payload)),
            }),
            .bool => try writer.print("{}", .{util.extract(bool, entry.self.getScalarValue(entry.value.payload))}),
            .type => try writer.print("{f}", .{FatTypeEntry{
                .self = entry.self,
                .type = @enumFromInt(entry.value.payload),
            }}),
            else => {},
        }
    }
};
const FatNodeEntry = struct {
    self: *Parser,
    node: *Node,
    pub fn format(entry: FatNodeEntry, writer: *std.Io.Writer) !void {
        const scope = entry.self.getScope(entry.self.current_scope);
        switch (scope.body.items[entry.node.*]) {
            .function_decl => |fn_decl| {
                entry.node.* += 1;
                try writer.print("fn (...) {f}{{\n", .{entry});
                const last_scope = entry.self.current_scope;
                entry.self.current_scope = entry.self.getFunction(fn_decl).scope;
                defer entry.self.current_scope = last_scope;

                entry.self.dumpCurrentScope(false);
            },
            .var_decl => |var_decl| {
                entry.node.* += 1;
                try writer.print("{s}({f}) {s}: {f} = {f}", .{
                    @tagName(var_decl.qualifier),
                    entry,
                    entry.self.tokenizer.slice(var_decl.name),
                    entry,
                    entry,
                });
            },
            .bin_op => |op| {
                entry.node.* += 1;
                try writer.print("({f} <{s}> {f})", .{
                    entry,
                    @tagName(op),
                    entry,
                });
            },
            .u_op => |op| {
                entry.node.* += 1;
                try writer.print("<{s}>{f}", .{ @tagName(op), entry });
            },
            .value => |value| {
                try writer.print("{f}", .{FatValueEntry{ .self = entry.self, .value = value }});
                entry.node.* += 1;
            },
            .identifier => |identifier| {
                try writer.print("\"{s}", .{entry.self.tokenizer.slice(identifier)});
                entry.node.* += 1;
            },
            .var_ref => |vr| {
                entry.node.* += 1;
                try writer.print("'{s}", .{
                    entry.self.tokenizer.slice(entry.self.getNodeEntryScope(vr.scope, vr.node).var_decl.name),
                });
            },
            .null => {
                entry.node.* += 1;
                try writer.print("<null>", .{});
            },
            else => {
                entry.node.* += 1;
                if (entry.node.* < scope.body.items.len)
                    try writer.print("{f}", .{entry});
            },
        }
    }
};

const ErrorInfo = error_message.ErrorInfo;
const Error = hgsl.Error;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
const UnaryOperator = Tokenizer.UnaryOperator;
const Token = Tokenizer.Token;
const TokenEntry = Tokenizer.TokenEntry;
