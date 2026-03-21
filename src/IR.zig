//for(parser.entry_points)|ep|
// lowerFunction(ep.function)

const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const IR = @This();

parser: *Parser,
allocator: Allocator,
inst_pool: InstructionPool = .{},
operand_pool: OperandPool = .{},

functions: List(Function) = .empty,
current_function: usize = undefined,

//function[entry_point[i]]
entry_points: List(usize) = .empty,
types: List(TypeEntry) = .empty,

extension_information: ExtensionInformation = .{},

const ExtensionInformation = struct {
    spirv_glsl_std: bool = true,
};

pub fn new(parser: *Parser, allocator: Allocator) Error!IR {
    const entry_point_count = parser.entry_points.items.len;
    return .{
        .parser = parser,
        .allocator = allocator,
        .functions = try .initCapacity(allocator, entry_point_count),
        .entry_points = try .initCapacity(allocator, entry_point_count),
    };
}

pub fn deinit(self: *IR) void {
    self.functions.deinit(self.allocator);
    self.entry_points.deinit(self.allocator);
}

pub fn lower(self: *IR) Error!void {
    for (self.parser.entry_points.items) |ep| {
        const source: Function.FunctionSource = .{ .function = ep.function };
        try self.functions.append(self.allocator, .{ .source = source });
        const function_id = self.functions.items.len - 1;

        try self.lowerFunction(function_id);

        try self.entry_points.append(self.allocator, function_id);
    }
}

pub fn lowerFunction(self: *IR, id: usize) Error!void {
    self.current_function = id;
    const entry = self.functions.items[id];
    //allocate rtype, parameter types

    const scope = if (entry.source == .function)
        self.parser.getFunctionEntry(entry.source.function).scope
    else
        self.parser.getFunctionPermutationEntry(entry.source.permutation).scope;

    const body_len = self.parser.getScopeEntry(scope).body.items.len;
    var node: Parser.Node = 0;

    while (node < body_len) {
        defer node += self.parser.nodeConsumption(scope, node);
        try self.lowerStatement(scope, node);
    }
}
fn lowerStatement(self: *IR, scope: Parser.Scope, node: Parser.Node) Error!void {
    const entry = self.parser.getNodeEntry(scope, node).*;

    switch (entry) {
        else => {},
    }
}

fn addInst(self: *IR, op: Op, operands: []const Operand, @"type": Type) Error!u32 {
    const operands_id = try self.operand_pool.addSlice(self.allocator, operands);
    const inst_id = try self.inst_pool.add(self.allocator, .{
        .op = op,
        .operands = operands_id,
        .count = @truncate(operands.len),
        .type = @"type",
    });
    const current_function = &self.functions.items[self.current_function];
    try current_function.body.append(self.arena(), inst_id);
    return inst_id;
}

const LocalVariable = struct {
    //use to store local constants?
    type: Type,
    value: Operand,

    node: Parser.Node,
};
const GlobalVariable = struct {
    type: Type,
    storage_class: StorageClass,
    value: Operand,

    node: Parser.Node,
    scope: Parser.Scope,
};
const Function = struct {
    parameter_types: []Type = undefined,
    return_type: Type = undefined,

    //init with atleast the number of nodes in source body
    body: List(u32) = .empty,
    // local_variables: Variable,

    source: FunctionSource,
    const FunctionSource = union(enum) {
        function: Parser.Function,
        permutation: Parser.FunctionPermutation,
    };
};

const Type = enum(u32) { _ };
const TypeEntry = union(enum) {
    void,
    bool,
    bool_vec: Vector.Len,

    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    array: Array,
    runtime_array: Type,

    //spirv: OpPointer(child_type, storage_class = PhysicalStorageBuffer)
    device_pointer: DevicePointer,
    logical_pointer: LogicalPointer,

    storage_image: Image,
    sampled_image: Image,

    @"struct": struct { off: u32, len: u32 },
    pub const Scalar = Parser.TypeEntry.Scalar;
    pub const Vector = Parser.TypeEntry.Vector;
    pub const Matrix = Parser.TypeEntry.Matrix;
    pub const Array = struct { len: u32, child: Type };

    pub const LogicalPointer = struct { child: Type, storage_class: StorageClass };

    pub const DevicePointer = struct { child: Type, alignment: Alignment };
    pub const Alignment = enum(u32) { size = 0, _ };

    pub const Image = struct {
        texel_type: Type,
        dim: Dimensionality,
    };
    const Dimensionality = enum { d1, d2, d3, cube };
};

const StorageClass = enum {
    general,
    input,
    output,
    push,
    workgroup,
    pub fn fromQualifier(qualifier: Parser.Qualifier) StorageClass {
        return switch (qualifier) {
            .push => .push,
            .workgroup => .workgroup,
            else => .general,
        };
    }
};

const Operand = struct {
    id: u32 = 0,
    kind: Kind = .raw,
    pub const Kind = enum(u32) {
        raw,
        inst,
        parameter,

        parser_value,

        global_variable,
        local_variable,

        input_builtin,
        output_builtin,
        _,
    };
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
    ptr_access_chain, //[ptr][elem][index0...]

    load_builtin,
    store_builtin,

    @"return",
    return_value,
};
const Inst = struct {
    op: Op,

    operands: u32,
    count: u32,

    type: Type,
};
const InstructionPool = Pool(Inst, 64);
const OperandPool = Pool(Operand, 128);
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

const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Error = hgsl.Error;
const Token = Tokenizer.Token;
