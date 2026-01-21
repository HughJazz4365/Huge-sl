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

pub fn dump(self: Parser) void {
    std.debug.print("types:\n", .{});
    for (self.types.items) |t| std.debug.print("--- {any}\n", .{t});
    std.debug.print("scopes:\n", .{});
    for (self.scopes.items) |t| std.debug.print("--- {any}\n", .{t});
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

    while (true) {
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

        try self.parseStatement();
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

    // const type_node =

    // const name = name_token.identifier;

    // const @"type": ?Type = switch (try self.tokenizer.peek()) {
    //     .@":" => blk: {
    //         self.tokenizer.skip();
    //         break :blk try self.expressionAsTypeAlloc(
    //             try self.parseExpressionAtom(),
    //         );
    //     },
    //     .@"=" => null,
    //     else => |e| if (self.defaultShouldStop(e))
    //         return self.errorOut(.unknown)
    //     else
    //         null,
    // };

    // const peek = try self.tokenizer.next();
    // const initializer = if (peek == .@"=")
    //     try self.parseExpression(defaultShouldStop)
    // else if (self.defaultShouldStop(peek))
    //     .null
    // else
    //     return self.errorOut(.{ .unexpected_token = self.tokenizer.last });

    // if (initializer == .null and qualifier == .@"const")
    //     return self.errorOut(.unknown);

    // try self.addStatement(.{ .var_decl = .{
    //     .qualifier = qualifier,
    //     .name = name,
    //     .type = if (@"type") |t| t else try self.typeOfAlloc(initializer),
    //     .initializer = if (@"type") |t| try self.implicitCast(t, initializer) else initializer,
    // } });

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
