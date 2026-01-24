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
    std.debug.print("structs:\n", .{});
    for (self.structs.items) |t| std.debug.print("--- {any}\n", .{t});
    std.debug.print("entry_points:\n", .{});
    for (self.entry_points.items) |t| std.debug.print("--- {any}\n", .{t});

    std.debug.print("---BODY: \n", .{});
    self.current_scope = .entry_file;
    self.dumpCurrentScope(false);
}

fn dumpCurrentScope(self: *Parser, consize: bool) void {
    const scope = self.getScope(self.current_scope);
    if (!consize)
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

fn nodeLength(self: *Parser, node: Node) u32 {
    var base: u32 = switch (self.getNodeEntry(node).*) {
        .var_decl => self.nodeSeqenceLength(node, 2),
        .bin_op => self.nodeSeqenceLength(node, 2),
        .null => 1,
        else => 1,
    };
    const body = self.getScope(self.current_scope).body.items;
    while (node + base < body.len and body[node + base] == .pad) base += 1;
    return base;
}
fn nodeSeqenceLength(self: *Parser, node: Node, count: usize) u32 {
    var len: u32 = 1;
    for (0..count) |_|
        len += self.nodeLength(node + len);
    return len;
}

pub fn parseFile(self: *Parser, tokenizer: Tokenizer) Error!void {
    self.tokenizer = tokenizer;

    const struct_handle = try self.addStruct(.{ .is_file = true });
    const struct_type = try self.addType(.{ .@"struct" = struct_handle });

    self.getStruct(struct_handle).scope = try self.parseScope(.{
        .container = .{ .@"struct" = struct_handle },
        .parent = self.current_scope,
    });

    _ = .{struct_type};
}

pub fn parseScope(self: *Parser, entry: ScopeEntry) Error!Scope {
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
    const token = self.tokenEntryShift();
    switch (token.kind) {
        .@"const", .@"var", .in, .out, .shared, .push => //
        try self.parseVarDecl(.@"const"),

        else => return self.errorOut(.unknown),
    }
    self.token += 1;
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

    if (self.tokenEntry().kind == .@":") {
        self.token += 1;
        _ = try self.parseExpressionBase();
    } else _ = try self.appendNode(.null);

    if (self.tokenEntry().kind != .@"=") {
        if (!self.defaultShouldStop(self.token))
            return self.errorOut(.{ .unexpected_token = self.token });

        _ = try self.appendNode(.null);
    } else {
        self.token += 1;
        _ = try self.parseExpression(defaultShouldStop);
    }
}

const ShouldStopFn = fn (*Parser, Token) bool;

inline fn parseExpression(self: *Parser, should_stop_fn: *const ShouldStopFn) Error!u32 {
    return self.parseExpressionRecursive(should_stop_fn, 0);
}
fn parseExpressionRecursive(self: *Parser, should_stop_fn: *const ShouldStopFn, bp: u8) Error!u32 {
    const left_node: Node = self.nextNode();
    var left_len = try self.parseExpressionBase();

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
fn parseExpressionBase(self: *Parser) Error!u32 {
    const len: u32 = switch (self.tokenEntry().kind) {
        .int_literal => blk: {
            const int = parseIntLiteral(self.tokenEntry().slice(self.tokenizer));
            _ = try self.appendNode(.{ .value = .{
                .type = try self.addType(.compint),
                .payload = try self.addScalarValue(int),
            } });

            self.token += 1;
            break :blk 1;
        },
        .entry_point => blk: { //entry point decl or type
            self.token += 1;
            const header_node = try self.appendNode(.entry_point_type_decl);

            if (self.tokenEntry().kind != .@"(")
                return self.errorOut(.{ .unexpected_token = self.token });
            self.token += 1;
            const args = try self.parseArguments();
            if (args.count == 0 or args.count > 2)
                return self.errorOut(.entry_point_decl_arg_count);

            _ = self.skipEndl();
            if (self.tokenEntry().kind != .@"{") {
                if (args.count == 2)
                    return self.errorOut(.entry_point_type_decl_arg_count);
                break :blk 2;
            } //parses the entry point scope if its not a type decl
            self.token += 1; //skip the '}'
            if (args.count == 1) _ = try self.appendNode(.null);

            const epid = try self.addEntryPoint(.{ .node = header_node });
            const scope = try self.parseScope(.{
                .parent = self.current_scope,
                .container = .{ .entry_point = epid },
            });
            self.getEntryPoint(epid).scope = scope;
            self.getNodeEntry(header_node).* = .{ .entry_point_decl = epid };

            break :blk 3;
        },
        .type_literal => blk: {
            const entry = @as(Tokenizer.TypeLiteral, @bitCast(self.tokenEntry()._len)).entry();
            self.token += 1;
            const t = try self.addType(entry);
            _ = try self.appendNode(.{ .value = .{ .type = .type, .payload = @intFromEnum(t) } });
            break :blk 1;
        },
        else => |e| {
            std.debug.print("KIND: {}\n", .{e});
            return self.errorOut(.unknown);
        },
        // .true => .{ .value = .create(.bool, true) },
        // .false => .{ .value = .create(.bool, false) },

        // .identifier => |identifier| .{ .identifier = identifier },
        // .int_literal => |il| .{ .value = Value.create(.compint, il) },
        // .float_literal => |fl| .{ .value = Value.create(.compfloat, fl) },
        // .type_literal => |@"type"| exprFromType(@"type"),
    };
    return len;
}
const ParseArgumentsResult = struct {
    count: u32 = 0,
    node_consumption: u32 = 0,
};
fn parseArguments(self: *Parser) Error!ParseArgumentsResult {
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

fn tokenEntryShift(self: *Parser) TokenEntry {
    const token = self.tokenEntry();
    self.token += 1;
    return token;
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
    for (self.types.items, 0..) |t, i| if (TypeEntry.eql(entry, t)) return @enumFromInt(i);
    const l = self.types.items.len;
    try self.types.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getNodeEntry(self: *Parser, node: Node) *NodeEntry {
    return &self.getScope(self.current_scope).body.items[node];
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
        entry_point: EntryPoint,
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

fn addEntryPoint(self: *Parser, entry: EntryPointEntry) Error!EntryPoint {
    const l = self.entry_points.items.len;
    try self.entry_points.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getEntryPoint(self: *Parser, handle: EntryPoint) *EntryPointEntry {
    return &self.entry_points.items[@intFromEnum(handle)];
}
pub const EntryPoint = enum(u32) { _ };
pub const EntryPointEntry = struct {
    scope: Scope = undefined,
    //next node specifies additional stage info(pad if none)
    //next 'len' nodes after stage info are EP body
    node: Node = undefined,

    name: Token = undefined,
    len: u32 = 0,
};

// const CompositeHeader = packed struct(u8);

const Node = u32;
pub const NodeEntry = union(enum) {
    pad,
    null,

    identifier: Token,
    // var_ref: Node,

    bin_op: BinaryOperator, //[bin_op][left][right]
    u_op: UnaryOperator, //[u_op][operand]
    value: Value,

    entry_point_decl: EntryPoint, //[h][stage:Node][stage_info:Node]
    entry_point_type_decl, //[h][stage:Node]

    //is there is no else its padded
    branch,
    loop: [11]u8,

    cast: Type,
    constructor: Type,

    //statement
    var_decl: VariableDeclaration, //[var_decl][type][initializer]
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
    pub const eql = std.meta.eql;
    pub const format = formatType;
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

fn formatType(self: TypeEntry, writer: *std.Io.Writer) !void {
    switch (self) {
        .scalar => |scalar| try writer.print("{c}{s}", .{
            "fui"[@intFromEnum(scalar.layout)],
            @tagName(scalar.width)[1..],
        }),
        else => {},
    }
}
const FatValueEntry = struct {
    self: *Parser,
    value: Value,
    pub fn format(entry: FatValueEntry, writer: *std.Io.Writer) !void {
        const type_entry = entry.self.getType(entry.value.type);
        switch (type_entry) {
            .compint => try writer.print("{d}", .{
                util.extract(i128, entry.self.getScalarValue(entry.value.payload)),
            }),
            .type => try writer.print("{f}", .{entry.self.getType(@enumFromInt(entry.value.payload))}),
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
            .var_decl => |var_decl| {
                entry.node.* += 1;
                try writer.print("{s} {s}: {f} = {f}", .{
                    @tagName(var_decl.qualifier),
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
            .value => |value| {
                try writer.print("{f}", .{FatValueEntry{ .self = entry.self, .value = value }});
                entry.node.* += 1;
            },
            .entry_point_decl => |epd| {
                entry.node.* += 1;
                try writer.print("entry_point({f}, {f}){{\n", .{ entry, entry });
                const last_cs = entry.self.current_scope;
                defer entry.self.current_scope = last_cs;
                entry.self.current_scope = entry.self.getEntryPoint(epd).scope;

                entry.self.dumpCurrentScope(true);
                try writer.print("\t}}", .{});
            },
            .null => {
                entry.node.* += 1;
                try writer.print("<null>", .{});
            },
            else => {
                entry.node.* += 1;
                if (entry.node.* >= scope.body.items.len)
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
