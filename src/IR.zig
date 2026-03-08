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
const NameMapping = struct { name: []const u8, operand: Operand };

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

    for (self.parser.entry_points.items, self.entry_points.items) |ep_info, *ep| {
        ep.body = null_instruction;
        const scope = self.parser.getFunctionEntry(ep_info.function).scope;
        const body = self.parser.getScopeEntry(scope).body.items;
        var statement: Parser.Node = 0;
        var inst: Instruction = null_instruction;
        while (statement < body.len) {
            defer statement = self.parser.nodeConsumptionScope(scope, statement);

            const statement_instruction = try self.lowerStatement(scope, statement);
            if (inst != null_instruction)
                self.pool.get(inst).next = statement_instruction;
            inst = statement_instruction;
        }
    }
}
fn lowerStatement(self: *IR, scope: Parser.Scope, node: Parser.Node) Error!Instruction {
    //name -> operand mappings??
    const entry = self.parser.getNodeEntryScope(scope, node).*;
    return switch (entry) {
        .folded_var_decl => |vd| switch (vd.qualifier) {
            else => null_instruction,
        },
        else => null_instruction,
    };
    // std.debug.print("lower statement node: {d}\n", .{node});
}
// fn lowerExpression(self: *IR, scope: Parser.Scope, node: Parser.Node) Error!Instruction.ID {}

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
};
const OutputBuiltin = enum { position };
const InputBuiltin = enum { vertex_id };

const LocalVariable = struct {
    type: Type,
    decl_node: Parser.Node, //??
};
const StorageClass = enum { general, input, output, push, workgroup };

const Operand = struct {
    id: u32,
    kind: Kind,
    const Kind = enum {
        //WHERE STORED
        local_variable,
        //entry_point.local_variables
        global_variable,
        //entry_point.global_variables??
        parser_constant,
        //parser.number_values, etc
        intermediate,
        //instruction pool
    };
};

const Type = enum(u32) { void = 0, _ };
const TypeEntry = union(enum) {
    void,
    bool,

    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    array: Array,
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

pub const InstructionData = struct {
    operands: [*]u32 = undefined,

    operand_count: u32 = 0,
    op: OpCode = undefined,
    type: Type = undefined,

    next: Instruction = null_instruction,

    pub fn format(self: InstructionData, writer: *std.Io.Writer) !void {
        try writer.print("{s}({d}) -> {d}:{any}", .{
            @tagName(self.op),
            self.operand_count,
            self.next,
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

    pub fn append(self: *InstructionPool, allocator: Allocator, inst: InstructionData) !Instruction {
        const ptr = try self.alloc(allocator);
        ptr.* = inst;
        return self.count - 1;
    }
    pub fn get(self: *InstructionPool, id: Instruction) *InstructionData {
        return &self.blocks.items[id / block_size][id % block_size];
    }
    pub fn new(self: *InstructionPool, allocator: Allocator) !Instruction {
        // _ = try self.alloc(allocator);
        // return self.count - 1;
        return self.append(allocator, .{});
    }
    fn alloc(self: *InstructionPool, allocator: Allocator) !*InstructionData {
        const blocks_len = self.blocks.items.len;
        if (blocks_len > 0) {
            const local_id = self.count % blocks_len;
            if (local_id > 0) {
                self.count += 1;
                return &self.blocks.items[self.count / blocks_len][local_id];
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
