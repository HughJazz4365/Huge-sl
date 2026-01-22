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

entry_points: List(EntryPointEntry) = .empty,
structs: List(StructEntry) = .empty,

scopes: List(ScopeEntry) = .empty,
current_scope: Scope = .entry_file,

token: Token = 0,

error_info: ErrorInfo = .unknown,

pub fn dump(self: *Parser) void {
    std.debug.print("types:\n", .{});
    for (self.types.items) |t| std.debug.print("--- {any}\n", .{t});
    std.debug.print("scopes:\n", .{});
    for (self.scopes.items) |t| {
        std.debug.print("--- {any}\n", .{t});
        std.debug.print("--- ---  items:\n", .{});
        for (t.body.items) |node|
            std.debug.print("\t{any}, ", .{node});
        std.debug.print("\n", .{});
    }
    std.debug.print("structs:\n", .{});
    for (self.structs.items) |t| std.debug.print("--- {any}\n", .{t});
    std.debug.print("entry_points:\n", .{});
    for (self.entry_points.items) |t| std.debug.print("--- {any}\n", .{t});
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
pub fn parseFile(self: *Parser, tokenizer: Tokenizer) Error!void {
    self.tokenizer = tokenizer;

    const struct_handle = try self.add(StructEntry{ .is_file = true });
    const struct_type = try self.getType(.{ .@"struct" = struct_handle });

    self.get(struct_handle).scope = try self.parseScope(.{
        .container = .{ .@"struct" = struct_handle },
        .parent = self.current_scope,
    });

    _ = .{struct_type};
}
pub fn parseScope(self: *Parser, entry: ScopeEntry) Error!Scope {
    const scope = try self.add(entry);

    const last_scope = self.current_scope;
    self.current_scope = scope;
    defer self.current_scope = last_scope;

    const len = try self.parseExpressionBase();
    std.debug.print("LEN : {d}\n", .{len});
    while (true and false) {
        self.skipEndl();
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

        // try self.parseStatement();
    }
    return scope;
}

fn parseStatement(self: *Parser) Error!void {
    std.debug.print("TOK: {f}\n", .{self.tokenEntry()});
    const token = self.nextTokenEntry();
    switch (token.kind) {
        .@"const", .@"var", .in, .out, .shared, .push => //
        try self.parseVarDecl(.@"const"),

        else => return self.errorOut(.unknown),
    }
    self.token += 1;
}
fn parseVarDecl(self: *Parser, qualifier: Qualifier) Error!void {
    const name_token = self.tokenEntry();
    if (name_token.kind != .identifier)
        return self.errorOut(.{ .unexpected_token = self.token });
    self.token += 1;

    const name = name_token.slice(self.tokenizer);
    std.debug.print("NAME: {s}\n", .{name});
    _ = qualifier;
}

const ShouldStopFn = fn (*Parser, Token) bool;
inline fn parseExpression(self: *Parser, should_stop_fn: *const ShouldStopFn) Error!NodeSlice {
    return self.parseExpressionRecursive(should_stop_fn, 0);
}
fn parseExpressionRecursive(self: *Parser, should_stop_fn: *const ShouldStopFn, bp: u8) Error!NodeSlice {
    var left = try self.parseExpressionBase();
    var fat = try self.tokenizer.peekFat();
    while (!should_stop_fn(self, fat.token)) {
        if (fat.token != .bin_op) return self.errorOut(.{ .unexpected_token = fat });

        const op = fat.token.bin_op;
        if (Tokenizer.bindingPower(op) <= bp) break;

        self.tokenizer.skip();
        try self.tokenizer.skipEndl(); //?

        const right = try self.parseExpressionRecursive(try self.parseExpressionAtom(), should_stop_fn, Tokenizer.bindingPower(op));
        const add_left = try self.createVal(left);
        left = .{ .bin_op = .{
            .left = add_left,
            .right = try self.createVal(right),
            .op = op,
        } };
        fat = try self.tokenizer.peekFat();
    }
    return left;
}
fn parseExpressionBase(self: *Parser) Error!NodeSlice { //should return {node: Node, len: usize}
    const len: usize = switch (self.tokenEntry().kind) {
        .int_literal => blk: {
            const int = parseIntLiteral(self.tokenEntry().slice(self.tokenizer));
            break :blk .{
                .node = try self.appendNode(.{ .value = .{
                    .type = try self.getType(.compint),
                    .payload = try self.addScalarValue(int),
                } }),
                .len = 1,
            };
        },
        else => return self.errorOut(.unknown),
        // .true => .{ .value = .create(.bool, true) },
        // .false => .{ .value = .create(.bool, false) },

        // .identifier => |identifier| .{ .identifier = identifier },
        // .int_literal => |il| .{ .value = Value.create(.compint, il) },
        // .float_literal => |fl| .{ .value = Value.create(.compfloat, fl) },
        // .type_literal => |@"type"| exprFromType(@"type"),
    };
    return len;
}
const NodeSlice = struct { node: Node, len: u32 };
fn parseIntLiteral(str: []const u8) i128 {
    return str[0] - '0';
}

fn isScopeFile(self: *Parser, scope: Scope) bool {
    const entry = self.get(scope);
    return if (entry.container == .@"struct")
        self.get(entry.container.@"struct").is_file
    else
        false;
}

fn skipEndl(self: *Parser) void {
    if (self.tokenEntry().kind == .endl) self.token += 1;
}

fn nextTokenEntry(self: *Parser) TokenEntry {
    const token = self.tokenEntry();
    self.token += 1;
    return token;
}
fn tokenEntry(self: *Parser) TokenEntry {
    return self.tokenizer.entry(self.token);
}

fn getType(self: *Parser, entry: TypeEntry) Error!Type {
    const l = self.types.items.len;
    try self.types.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn appendNode(self: *Parser, entry: NodeEntry) Error!Node {
    const scope = self.get(self.current_scope);
    const l: Node = @truncate(scope.body.items.len);
    try scope.body.append(self.allocator, entry);
    return l;
}

pub inline fn add(self: *Parser, entry: anytype) Error!HandleType(@TypeOf(entry)) {
    return switch (@TypeOf(entry)) {
        StructEntry => self.addStruct(entry),
        ScopeEntry => self.addScope(entry),
        else => comptime unreachable,
    };
}
fn HandleType(Entry: type) type {
    return switch (Entry) {
        StructEntry => Struct,
        ScopeEntry => Scope,

        else => comptime unreachable,
    };
}
pub inline fn get(self: *Parser, handle: anytype) *EntryType(@TypeOf(handle)) {
    return switch (@TypeOf(handle)) {
        Struct => self.getStruct(handle),
        Scope => self.getScope(handle),

        else => comptime unreachable,
    };
}
fn EntryType(Handle: type) type {
    return switch (Handle) {
        Struct => StructEntry,
        Scope => ScopeEntry,

        else => comptime unreachable,
    };
}
fn addScalarValue(self: *Parser, scalar: anytype) Error!u32 {
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
    };
};

// pub fn getVariableReference(
//     self: *Parser,
//     name: []const u8,
//     scope: Node,
//     until: Node,
// ) void {}

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
pub const EntryPoint = enum(u32) { _ };
pub const EntryPointEntry = struct {
    scope: Scope,
    //next node specifies additional stage info(pad if none)
    //next 'len' nodes after stage info are EP body
    node: Node = undefined,

    name: Token = undefined,
    len: u32 = 0,
    stage: hgsl.Stage,
};

// const CompositeHeader = packed struct(u8);

const Node = u32;
pub const NodeEntry = union(enum) {
    pad,
    null,

    identifier: Token,
    var_ref: Node,

    bin_op: BinaryOperator,
    u_op: UnaryOperator,
    value: Value,

    //is there is no else its padded
    branch,
    loop: [11]u8,

    cast: Type,
    constructor: Type,

    //statement
    var_decl: VariableDeclaration,
};
const VariableDeclaration = struct {
    qualifier: Qualifier,
    name: Token,
};
const Qualifier = union(enum) {
    @"const",
    mut,
    in: Interpolation,
    out: Interpolation,
    push,
    shared,
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

pub const Type = enum(u32) { type, _ };
pub const TypeEntry = union(enum) {
    unknown: Node,
    type_of: Node,

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
    function: u32,
    entry_point: hgsl.Stage,

    // pub const format = @import("debug.zig").formatType;
    pub const Tag = std.meta.Tag(@This());
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

const ErrorInfo = error_message.ErrorInfo;
const Error = hgsl.Error;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
const UnaryOperator = Tokenizer.UnaryOperator;
const Token = Tokenizer.Token;
const TokenEntry = Tokenizer.TokenEntry;
