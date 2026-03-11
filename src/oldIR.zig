//TODO: remove internal arena
//arena only makes sense for InstructionData.operands
const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const IR = @This();

arena_allocator: std.heap.ArenaAllocator,
parser: *Parser,

pool: InstructionPool = .{},

types: List(TypeEntry) = .empty,

entry_points: List(EntryPoint) = .empty,
current_entry_point: usize = 0,

name_mappings: List(NameMapping) = .empty,

const NameMapping = struct {
    name: []const u8,
    operand: Operand,
    scope: Parser.Scope,
};

const NameMapping = struct {
    name: []const u8,
    operand: Operand,
    scope: Parser.Scope,
};

pub fn dump(self: *IR) void {
    // for (self.entry_points.items) |ep|
    // std.debug.print("EP:name: {s}, kind: {}\n", .{ ep.name, ep.kind });

    for (self.entry_points.items) |ep| {
        std.debug.print("ENTRYPOINT:\n", .{});
        var body = ep.body;
        while (body != null_instruction) {
            const data = self.pool.get(body).*;
            std.debug.print("|{d}|{f}\n", .{ body, data });
            body = data.next;
        }
    }
}

pub fn new(parser: *Parser, allocator: Allocator) Error!IR {
    var ir: IR = .{
        .parser = parser,
        .arena_allocator = .init(allocator),
    };
    try ir.types.append(ir.arena(), .void);
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

    for (self.parser.entry_points.items, self.entry_points.items) |ep_info, *ep| {
        const scope = self.parser.getFunctionEntry(ep_info.function).scope;
        self.parser.current_scope = scope;

        const body = self.parser.getScopeEntry(scope).body.items;

        ep.body = self.pool.count; //next created instruction
        var tail = null_instruction;
        defer { //if none instructions were added set body to 'null'
            if (tail == null_instruction) ep.body = null_instruction;
        }

        var statement: Parser.Node = 0;
        while (statement < body.len) {
            defer statement = self.parser.nodeConsumption(statement);

            const statement_instruction = try self.lowerStatement(tail, statement);
            if (statement_instruction != null_instruction)
                tail = statement_instruction;
        }
    }
}
fn lowerStatement(self: *IR, tail: Instruction, node: Parser.Node) Error!Instruction {
    const entry = self.parser.getNodeEntry(node).*;
    return switch (entry) {
        .folded_var_decl => |var_decl| self.lowerVariableDeclaration(tail, var_decl, node),
        .assignment => blk: {
            const k = (try self.lowerExpression(tail, node + 1 + self.parser.nodeConsumption(node + 1)))[0];
            std.debug.print("K: {any}\n", .{k});
            break :blk k;
        },
        else => null_instruction,
    };
    // std.debug.print("lower statement node: {d}\n", .{node});
}
fn lowerVariableDeclaration(self: *IR, tail: Instruction, var_decl: Parser.VariableDeclaration, node: Parser.Node) Error!Instruction {
    return switch (var_decl.qualifier) {
        .@"const" => blk: {
            const value_node = node + 2 + self.parser.nodeConsumption(node + 2);
            const initializer = try self.lowerExpression(tail, value_node);
            try self.name_mappings.append(self.arena(), .{
                .name = self.parser.tokenizer.slice(var_decl.name),
                .operand = initializer,
                .scope = self.parser.current_scope,
            });
            break :blk if (initializer[1] == .instruction)
                initializer[0]
            else
                null_instruction;
        },
        .@"var" => null_instruction,
        .push, .workgroup => null_instruction,
        .env, .vertex, .fragment, .compute => return null_instruction,
    };
}
fn lowerExpression(self: *IR, tail: Instruction, node: Parser.Node) Error!Operand {
    std.debug.print("lowers exprsestion\n", .{});
    const entry = self.parser.getNodeEntry(node).*;
    return switch (entry) {
        .constructor => |constructor| try self.lowerConstructor(
            tail,
            constructor.elem_count,
            @enumFromInt(self.parser.getValuePayload(node + 1)),
            node + 1 + self.parser.nodeConsumption(node + 1),
        ),
        .value => |value| .{ value.payload, .parser_constant },

        else => |e| {
            std.debug.print("else: {s}\n", .{@tagName(e)});
            return null_operand;
        },
        // else => null_instruction,
    };
}
fn lowerConstructor(self: *IR, tail: Instruction, elem_count: u32, @"type": Parser.Type, elem_node: Parser.Node) Error!Operand {
    return switch (self.parser.getTypeEntry(@"type")) {
        .vector => blk: {
            var operands: [4]Operand = undefined;
            var count: usize = 0;

            var node = elem_node;
            var current = tail;

            for (0..elem_count) |_| {
                defer node += self.parser.nodeConsumption(node);

                //TODO: can require a cast!
                const elem_type = self.parser.typeOf(node) catch unreachable;
                const elem_slots = (self.parser.constructorStructure(elem_type) catch unreachable).len;

                defer count += elem_slots;

                const elem_inst = try self.lowerExpression(current, node);
                self.advanceOperand(&current, elem_inst);

                if (elem_slots > 1) {
                    for (0..elem_slots) |j| {
                        const extract = try self.addInstruction(.{
                            .op = .composite_extract,
                            .operands = &.{ elem_inst, .{ @truncate(j), .literal } },
                        });
                        operands[count + j] = .{ extract, .instruction };
                        self.advance(&current, extract);
                    }
                } else operands[count] = elem_inst;
            }
            const construct = try self.addInstruction(.{
                .op = .composite_construct,
                .operands = operands[0..count],
            });
            self.advance(&current, construct);
            break :blk .{ construct, .instruction };
        },
        else => @panic("lower non vector consturctor"),
    };
}

fn addInstruction(self: *IR, data: struct {
    op: OpCode,
    operands: []const Operand = &.{},
    next: Instruction = null_instruction,
    type: Type = .void,
}) Error!Instruction {
    return self.pool.add(self.arena(), .{
        .op = data.op,
        .operands = if (data.operands.len > 0)
            (try self.arena().dupe(Operand, data.operands)).ptr
        else
            &.{},
        .operand_count = @truncate(data.operands.len),
        .next = data.next,
        .type = data.type,
    });
}

fn attach(self: *IR, head: Instruction, tail: Instruction) void {
    if (head != null_instruction) self.pool.get(head).next = tail;
}
fn advance(self: *IR, head: *Instruction, tail: Instruction) void {
    self.attach(head.*, tail);
    head.* = tail;
}
fn advanceOperand(self: *IR, head: *Instruction, operand: Operand) void {
    if (operand[1] == .instruction) self.advance(head, operand[0]);
}

const EntryPoint = struct {
    body: Instruction = null_instruction,
    // //input variables []Variable
    // //output variables []Variable
    // local_variables: List(VariableEntry) = .empty,
};

const GlobalVariable = struct {
    storage_class: StorageClass,
    type: Type, //not a pointer type
    decl_node: Parser.Node,

    extra: u32 = 0, //binding, offset
};
const OutputBuiltin = enum { position };
const InputBuiltin = enum { vertex_id };

const LocalVariable = struct {
    type: Type,
    decl_node: Parser.Node, //??
};
const StorageClass = enum { general, input, output, push, workgroup };

const Operand = @Tuple(&.{ u32, OperandKind });
const OperandKind = enum(u32) {
    //WHERE STORED
    local_variable,
    //entry_point.local_variables
    global_variable,
    //entry_point.global_variables??

    parser_constant,
    //parser.values
    ir_constant,
    //parser.values

    instruction,
    //instruction pool
    literal,
};

const Type = enum(u32) { void = 0, _ };
const TypeEntry = union(enum) {
    void,
    bool,

    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    array: Array,
    runtime_array: Type,
    device_pointer: DevicePointer,
    logical_pointer: LogicalPointer,

    storage_image,
    sampled_image,
    //sampler, image

    // @"struct": Struct,
    const Scalar = Parser.TypeEntry.Scalar;
    const Vector = Parser.TypeEntry.Vector;
    const Matrix = Parser.TypeEntry.Matrix;
    const Array = struct { len: u32, child: Type };
    const LogicalPointer = struct { child: Type, storage_class: StorageClass };
    const DevicePointer = struct { child: Type, alignment: Alignment };
    const Alignment = enum(u32) { size = 0, _ };
};

const OpCode = enum(u32) {
    function,

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

pub const Instruction = u32;
const null_instruction = ~@as(Instruction, 0);
const null_operand: Operand = .{ null_instruction, .instruction };

pub const InstructionData = struct {
    operands: [*]Operand = undefined,

    operand_count: u32 = 0,
    op: OpCode = undefined,
    type: Type = undefined,

    next: Instruction = null_instruction,

    pub fn format(self: InstructionData, writer: *std.Io.Writer) !void {
        try writer.print("{s}", .{@tagName(self.op)});
        if (self.next != null_instruction)
            try writer.print("(N:{d})", .{self.next});
        try writer.print(" -> [{d}]:{any}", .{
            self.operand_count,
            self.operands[0..self.operand_count],
        });
    }
};

inline fn arena(self: *IR) Allocator {
    return self.arena_allocator.allocator();
}

const InstructionPool = struct {
    blocks: List([]InstructionData) = .empty,
    count: u32 = 0,
    const block_size = 32;

    pub fn add(self: *InstructionPool, allocator: Allocator, inst: InstructionData) !Instruction {
        const ptr = try self.alloc(allocator);
        ptr.* = inst;
        return self.count - 1;
    }
    pub fn get(self: *InstructionPool, id: Instruction) *InstructionData {
        return &self.blocks.items[id / block_size][id % block_size];
    }
    fn alloc(self: *InstructionPool, allocator: Allocator) !*InstructionData {
        const blocks_len = self.blocks.items.len;
        if (blocks_len > 0) {
            const local_id = self.count % block_size;
            if (local_id > 0) {
                self.count += 1;
                return &self.blocks.items[self.count / block_size][local_id];
            }
        }
        const new_block = try allocator.alloc(InstructionData, block_size);
        try self.blocks.append(allocator, new_block);
        self.count += 1;
        return @ptrCast(new_block.ptr);
    }
};
fn dupeFunctionName(self: *IR, function: Parser.Function) Error![]const u8 {
    const token = self.parser.getFunctionEntry(function).name;
    const slice = self.parser.tokenizer.slice(token);
    return try self.arena().dupe(u8, slice);
}

const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Error = hgsl.Error;
const Token = Tokenizer.Token;
