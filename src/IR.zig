const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const IR = @This();

arena_allocator: std.heap.ArenaAllocator,
parser: *Parser,

types: List(TypeEntry) = .empty,

pub fn new(parser: *Parser, allocator: Allocator) IR {
    return .{
        .parser = parser,
        .arena_allocator = .init(allocator),
    };
}

pub fn deinit(self: IR) void {
    self.arena_allocator.deinit();
}

pub fn lower(self: *IR) Error!void {
    try self.types.append(self.arena(), .void);
}

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
const StorageClass = enum { general, input, output, push, shared };

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

    device_pointer_load, //[ptr]
    load, //[var]
    store, //[var][value]
    access_chain, //[var][index0..]

    load_builtin,
    store_builtin,

    @"return",
    return_value,

    initialize_local_variable, //[localvarid][id]
};

pub const Instruction = struct {
    operands: [*]u32 = undefined,

    operand_count: u32 = 0,
    op: OpCode = undefined,
    type: Type = undefined,

    next: ID = null_id,

    const null_id = ~@as(ID, 0);
    pub const ID = u32;
    pub fn format(self: Instruction, writer: *std.Io.Writer) !void {
        try writer.print("{s}({d}) -> {d}:{any}", .{
            @tagName(self.op),
            self.operands.len,
            self.next,
            self.operands,
        });
    }
};

inline fn arena(self: *IR) Allocator {
    return self.arena_allocator.allocator();
}

const InstructionPool = struct {
    blocks: List([]Instruction) = .empty,
    count: u32 = 0,
    const block_size = 32;

    pub fn append(self: *InstructionPool, allocator: Allocator, inst: Instruction) !Instruction.ID {
        const ptr = try self.alloc(allocator);
        ptr.* = inst;
        return self.count - 1;
    }
    pub fn get(self: *InstructionPool, id: Instruction.ID) *Instruction {
        return &self.blocks.items[id / block_size][id % block_size];
    }
    pub fn new(self: *InstructionPool, allocator: Allocator) !Instruction.ID {
        // _ = try self.alloc(allocator);
        // return self.count - 1;
        return self.append(allocator, .{});
    }
    fn alloc(self: *InstructionPool, allocator: Allocator) !*Instruction {
        const blocks_len = self.blocks.items.len;
        if (blocks_len > 0) {
            const local_id = self.count % blocks_len;
            if (local_id > 0) {
                self.count += 1;
                return &self.blocks.items[self.count / blocks_len][local_id];
            }
        }
        const new_block = try allocator.alloc(Instruction, block_size);
        try self.blocks.append(allocator, new_block);
        self.count += 1;
        return @ptrCast(new_block.ptr);
    }
};
const Array = struct { len: u32, child: Type };
const LogicalPointer = struct { child: Type, storage_class: StorageClass };
const DevicePointer = struct { child: Type, alignment: Alignment };
const Alignment = enum(u32) { size = 0, _ };

const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Error = hgsl.Error;
const Token = Tokenizer.Token;
const Scalar = Parser.Scalar;
const Vector = Parser.Vector;
const Matrix = Parser.Matrix;
