//TODO: var refs?

//TODO: remove internal arena
//arena only makes sense for InstructionData.operands

const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const IR = @This();

arena_allocator: std.heap.ArenaAllocator,
parser: *Parser,

inst_pool: InstructionPool = .{},
operand_pool: OperandPool = .{},

entry_points: List(EntryPoint) = .empty,
current_entry_point: usize = 0,

name_mappings: List(NameMapping) = .empty,

const NameMapping = struct {
    name: []const u8,
    operand: Operand,

    scope: Parser.Scope,
    decl_node: Parser.Node,
};

pub fn dump(self: *IR) void {
    for (self.entry_points.items) |ep| {
        std.debug.print("ENTRYPOINT(len: {d}):\n", .{ep.body.items.len});
        for (ep.body.items) |id| {
            const inst = self.inst_pool.get(id).*;
            const operands = self.operand_pool.getSlice(inst.operands, inst.count);
            std.debug.print("|{d}|{s} -> [{d}]{{", .{
                id,
                @tagName(inst.op),
                operands.len,
            });
            for (operands, 0..) |operand, i|
                std.debug.print("{f}{s}", .{
                    operand,
                    if (i + 1 == operands.len) "" else ", ",
                });
            std.debug.print("}}\n", .{});
        }
    }
}

pub fn new(parser: *Parser, allocator: Allocator) Error!IR {
    const ir: IR = .{
        .parser = parser,
        .arena_allocator = .init(allocator),
    };
    return ir;
}

pub fn deinit(self: IR) void {
    self.arena_allocator.deinit();
}

pub fn lower(self: *IR) Error!void {
    const ep_count = self.parser.entry_points.items.len;
    self.entry_points = try .initCapacity(self.arena(), ep_count);
    self.entry_points.items.len = ep_count;
    @memset(self.entry_points.items, .{});

    for (self.parser.entry_points.items, 0..) |ep_info, i| {
        self.current_entry_point = i;

        const scope = self.parser.getFunctionEntry(ep_info.function).scope;
        self.parser.current_scope = scope;

        const body = self.parser.getScopeEntry(scope).body.items;

        var statement: Parser.Node = 0;
        while (statement < body.len) {
            defer statement = self.parser.nodeConsumption(statement);
            try self.lowerStatement(statement);
        }
    }
}
fn lowerStatement(self: *IR, node: Parser.Node) Error!void {
    const entry = self.parser.getNodeEntry(node).*;
    switch (entry) {
        .folded_var_decl => |var_decl| try self.lowerVariableDeclaration(
            var_decl.qualifier,
            var_decl.name,
            node,
        ),
        .assignment => try self.lowerAssignment(node + 1),

        else => std.debug.print("lstatement: {s}\n", .{@tagName(entry)}),
    }
}

fn lowerAssignment(self: *IR, target_node: Parser.Node) Error!void {
    const target_operand = try self.lowerExpression(target_node, .pointer);

    const value_node = target_node + self.parser.nodeConsumption(target_node);
    const value_operand = try self.lowerExpression(value_node, .value);
    _ = try self.addInst(.store, &.{ target_operand, value_operand });
}

fn lowerVariableDeclaration(
    self: *IR,
    qualifier: Parser.Qualifier,
    name: Tokenizer.Token,
    node: Parser.Node,
) Error!void {
    _ = .{ self, name, node };
    switch (qualifier) {
        else => {},
    }
}

const ExpressionKind = enum { value, pointer };
fn lowerExpression(self: *IR, node: Parser.Node, kind: ExpressionKind) Error!Operand {
    const entry = self.parser.getNodeEntry(node).*;
    return switch (entry) {
        .builtin => |builtin| blk: {
            const variable = switch (builtin.builtin) {
                .position => OutputBuiltin.position.toOperand(),
                .vertex_id => InputBuiltin.vertex_id.toOperand(),
                // else => unreachable,
            };
            if (kind == .pointer) break :blk variable;

            const load = try self.addInst(.load, &.{variable});
            break :blk .new(load, .inst);
        },
        .value => |value| .new(value.payload, .parser_value),
        .constructor => |constructor| blk: {
            const type_node = node + 1;
            const @"type": Parser.Type = @enumFromInt(self.parser.getValuePayload(type_node));

            const elem_node = type_node + self.parser.nodeConsumption(type_node);

            break :blk self.lowerConstructor(@"type", constructor.elem_count, elem_node);
            // try self.lowerConstructor(
        },
        .indexing => try self.lowerIndexing(node + 1, kind),
        .var_ref => try self.lowerVariableReference(),

        inline else => |_, tag| @panic(@tagName(tag)),
        // else => .fromRaw(0),
    };
}
// fn lowerVariableReference
fn lowerIndexing(self: *IR, target_node: Parser.Node, kind: ExpressionKind) Error!Operand {
    const target_operand = try self.lowerExpression(target_node, kind);

    const index_node = target_node + self.parser.nodeConsumption(target_node);
    const index_operand = try self.lowerExpression(index_node, .value);

    const access_chain = try self.addInst(
        .access_chain,
        &.{ target_operand, index_operand },
    );
    if (kind == .pointer)
        return .new(access_chain, .inst);

    const load = try self.addInst(.load, &.{.new(access_chain, .inst)});
    return .new(load, .inst);

    //pointer deref at index offset
    //composite extract
    //vector dynamic extract
    //access chain
    //access chain
}

fn lowerConstructor(self: *IR, @"type": Parser.Type, elem_count: u32, elem_node: Parser.Node) Error!Operand {
    return switch (self.parser.getTypeEntry(@"type")) {
        .vector => blk: {
            var operands: [4]Operand = undefined;
            var count: usize = 0;

            var node = elem_node;

            for (0..elem_count) |_| {
                defer node += self.parser.nodeConsumption(node);
                //TODO: can require a cast!
                const elem_type = self.parser.typeOf(node) catch unreachable;
                const elem_slots = (self.parser.constructorStructure(elem_type) catch unreachable).len;

                defer count += elem_slots;

                const elem_operand = try self.lowerExpression(node, .value);

                if (elem_slots > 1) {
                    for (0..elem_slots) |j| {
                        const extract = try self.addInst(
                            .composite_extract,
                            &.{ elem_operand, .fromRaw(j) },
                        );
                        operands[count + j] = .new(extract, .inst);
                    }
                } else operands[count] = elem_operand;
            }
            const construct = try self.addInst(.composite_construct, operands[0..count]);
            break :blk .new(construct, .inst);
        },
        else => @panic("lower non vector consturctor"),
    };
}

fn addInst(self: *IR, op: Op, operands: []const Operand) Error!u32 {
    // const id =
    const operands_id = try self.operand_pool.addSlice(self.arena(), operands);
    const inst_id = try self.inst_pool.add(self.arena(), .{
        .op = op,
        .operands = operands_id,
        .count = @truncate(operands.len),
    });
    const current_ep = &self.entry_points.items[self.current_entry_point];
    try current_ep.body.append(self.arena(), inst_id);
    return inst_id;
}

fn operandSlice(self: *IR, inst: Inst) []Operand {
    self.operand_pool.getSlice(inst.operands);
}

const EntryPoint = struct {
    body: List(u32) = .empty,
};

//every instruction has unique id
//in the pool, those ids stored in list to
//form function bodies
//value produced by instruction is
//referenced by its id
const Inst = struct {
    op: Op,

    operands: u32,
    count: u32,

    type: u32 = undefined,
};

const OutputBuiltin = enum(u32) {
    position,
    pub fn toOperand(self: @This()) Operand {
        return .new(@intFromEnum(self), .output_builtin);
    }
};
const InputBuiltin = enum(u32) {
    vertex_id,
    pub fn toOperand(self: @This()) Operand {
        return .new(@intFromEnum(self), .input_builtin);
    }
};

const Operand = packed struct(u64) {
    val: u32,
    kind: OperandKind,
    pub inline fn new(val: u32, kind: OperandKind) Operand {
        return .{ .val = val, .kind = kind };
    }

    pub fn fromRaw(raw: u64) Operand {
        return @bitCast(raw);
    }
    pub fn toRaw(operand: Operand) u64 {
        return @bitCast(operand);
    }
    pub fn format(self: Operand, writer: *std.Io.Writer) !void {
        try writer.print("{{{d}, {}}}", .{ self.val, self.kind });
    }
};

const OperandKind = enum(u32) {
    inst,
    parser_value,
    variable,
    local_variable,

    input_builtin,
    output_builtin,
    _,
};

const Op = enum(u32) {
    add, //[left][right]
    sub,
    mul,
    div,

    vector_x_scalar,
    vector_x_matrix,
    matrix_x_vector,
    matrix_x_matrix,

    dot,

    //[operand]
    transpose,

    swizzle, //[vec1][vec2][x][y][z][w]
    swizzle_literal, //[vector][mask][elem1]...[elem4]

    composite_extract,
    composite_construct,

    // device_pointer_load?, //[ptr]
    // device_pointer_store?, //[ptr]
    load, //[var]
    store, //[var][value]
    access_chain, //[var][index0..]

    load_builtin,
    store_builtin,

    @"return",
    return_value,

    initialize_local_variable, //[localvarid][id]
};
const InstructionPool = Pool(Inst, 16);
const OperandPool = Pool(Operand, 64);
fn Pool(T: type, comptime block_size: comptime_int) type {
    return struct {
        blocks: List([]T) = .empty,
        count: u32 = 0,
        const Self = @This();

        pub fn addSlice(self: *Self, allocator: Allocator, slice: []const T) !u32 {
            const len: u32 = @truncate(slice.len);
            if (len > block_size)
                @panic("cannot allocate slice bigger than block_size");

            defer self.count += len;
            const free_in_block = (block_size - (self.count % block_size)) * @intFromBool(self.count > 0);
            if (free_in_block < len) {
                self.count += free_in_block;
                const new_block = try allocator.alloc(T, block_size);
                try self.blocks.append(allocator, new_block);
            }
            const local_id = self.count % block_size;
            @memcpy(
                self.blocks.items[self.blocks.items.len - 1][local_id .. local_id + len],
                slice,
            );
            return self.count;
        }
        pub fn add(self: *Self, allocator: Allocator, inst: T) !u32 {
            const ptr = try self.alloc(allocator);
            ptr.* = inst;
            return self.count - 1;
        }
        pub fn getSlice(self: *Self, id: u32, len: usize) []T {
            return @as([*]T, @ptrCast(self.get(id)))[0..len];
        }
        pub fn get(self: *Self, id: u32) *T {
            return &self.blocks.items[id / block_size][id % block_size];
        }
        fn alloc(self: *Self, allocator: Allocator) !*T {
            const blocks_len = self.blocks.items.len;
            if (blocks_len > 0) {
                const local_id = self.count % block_size;
                if (local_id > 0) {
                    self.count += 1;
                    return &self.blocks.items[self.count / block_size][local_id];
                }
            }
            const new_block = try allocator.alloc(T, block_size);
            try self.blocks.append(allocator, new_block);
            self.count += 1;
            return @ptrCast(new_block.ptr);
        }
    };
}

inline fn arena(self: *IR) Allocator {
    return self.arena_allocator.allocator();
}
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Error = hgsl.Error;
const Token = Tokenizer.Token;
