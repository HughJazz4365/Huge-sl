const std = @import("std");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");
const error_message = @import("errorMessage.zig");
const hgsl = @import("root.zig");

error_info: ErrorInfo = .unknown,

tokenizer: Tokenizer = undefined,
allocator: Allocator,

nodes: List(NodeEntry) = .empty,
types: List(TypeEntry) = .empty,

scalar_values: List(u128) = .empty,
composite_values: List(u8) = .empty,

entry_points: List(EntryPoint) = .empty,
structs: List(Struct) = .empty,
current_scope: Node = file_scope,

token: Token = undefined,
const StructID = enum(u32) { file = 0, _ };

const file_scope: Node = 0;

pub fn dump(self: Parser) void {
    std.debug.print("types: {{\n", .{});
    for (self.types.items) |t| std.debug.print("\t{any}\n", .{t});

    std.debug.print("}}\n", .{});
}
pub fn parseFile(allocator: Allocator, tokenizer: Tokenizer, _: List(Parser)) Error!Parser {
    const nodes_initial_capacity = 128;
    const types_initial_capacity = 128;
    const scalar_values_initial_capacity = 32;
    const composite_values_initial_capacity = 16 * 4 * 4;
    var self: Parser = .{
        .tokenizer = tokenizer,
        .allocator = allocator,
    };
    self.nodes = try .initCapacity(allocator, nodes_initial_capacity);
    self.types = try .initCapacity(allocator, types_initial_capacity);
    self.scalar_values = try .initCapacity(allocator, scalar_values_initial_capacity);
    self.composite_values = try .initCapacity(allocator, composite_values_initial_capacity);

    self.types.appendAssumeCapacity(.type);

    self.types.appendAssumeCapacity(.{ .@"struct" = .file });
    const file_struct_node: NodeEntry = .{ .value = .{ .type = .type, .payload = 0 } };

    try self.structs.append(self.allocator, .{ .node = file_scope, .name = tokenizer.path });
    self.nodes.appendAssumeCapacity(file_struct_node);

    if (tokenizer.list.items.len == 0) return self;
    return self;
}
pub fn getType(self: *Parser, entry: TypeEntry) Error!Type {
    const l = self.types.items.len;
    try self.types.append(self.allocator, entry);
    return @enumFromInt(l);
}
pub fn parseScope(self: *Parser) void {
    _ = self;
}

// pub fn getVariableReference(
//     self: *Parser,
//     name: []const u8,
//     scope: Node,
//     until: Node,
// ) void {}

const Struct = struct {
    name: []const u8 = undefined,

    node: Node = undefined,
    parent: Node = file_scope,

    len: u32 = 0,
};
const EntryPoint = struct {
    //next node specifies additional stage info(pad if none)
    //next 'len' nodes after stage info are EP body
    node: Node = undefined,
    parent: Node = undefined,

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
};
const Value = struct {
    type: Type,
    payload: u32,
};

pub const Type = enum(u32) { type = 0, _ };
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

    @"enum": u32, //enum id
    @"struct": StructID, // struct id
    function: u32, // function id
    entry_point: hgsl.Stage, //entry point id

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

const ErrorInfo = error_message.ErrorInfo;
const Error = hgsl.Error;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
const UnaryOperator = Tokenizer.UnaryOperator;
const Token = Tokenizer.Token;
