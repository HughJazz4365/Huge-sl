const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const IR = @This();

arena: std.heap.ArenaAllocator,
parser: *Parser,
current_entry_point: usize = 0,

pool: InstructionPool = .{},

entry_points: List(EntryPoint) = .empty,
entry_point_aliases: void = undefined,

variables: List(VariableEntry) = .empty,
constants: List(ConstantEntry) = .empty,

//list of functions
//shuffle_literals
//extension information?

//create valid_ variations of statement NodeEntries
pub fn dump(self: *IR) void {
    // for (self.entry_points.items) |ep|
    // std.debug.print("EP:name: {s}, kind: {}\n", .{ ep.name, ep.kind });

    std.debug.print("INSTRUCTION POOL: {d}\n", .{self.pool.count});
    for (0..self.pool.count) |i| {
        const block_index = i / InstructionPool.block_size;
        const local_index = i % InstructionPool.block_size;
        std.debug.print(
            "{f}\n",
            .{self.pool.blocks.items[block_index][local_index]},
        );
    }
}

pub fn lower(self: *IR) Error!void {
    const body = self.parser.getScopeEntry(.root_source_file).body.items;
    var node: Parser.Node = 0;
    while (node < body.len) {
        defer node += self.parser.nodeConsumptionScope(.root_source_file, node);
        const node_entry = body[node];

        if (node_entry == .var_decl and node_entry.var_decl.qualifier.isEntryPoint()) {
            const function_node = node + 1 + self.parser.nodeSequenceConsumption(.root_source_file, node + 1, 2);

            const function_value = (try self.parser.getValue(function_node)).?;
            const function: Parser.Function = @enumFromInt(function_value.payload);

            const entry_point_scope = self.parser.getFunctionEntry(function).scope;

            self.current_entry_point = self.entry_points.items.len;
            try self.entry_points.append(self.arena.allocator(), .{
                .parser_id = function,
                .body = try self.lowerScope(entry_point_scope),
                .name = try self.dupeFunctionName(function),
                .kind = switch (node_entry.var_decl.qualifier) {
                    .vertex => .vertex,
                    .fragment => .fragment,
                    else => .compute,
                },
                .compute_workgroup_size = 0, //??
            });
        }
    }
}

pub fn new(parser: *Parser, allocator: std.mem.Allocator) IR {
    return .{
        .parser = parser,
        .arena = .init(allocator),
    };
}
pub fn deinit(self: *IR) void {
    self.arena.deinit();
}

pub fn lowerScope(self: *IR, scope: Parser.Scope) Error!Instruction.ID {
    const len = self.parser.getScopeEntry(scope).body.items.len;
    if (len == 0) return Instruction.null_id;
    const first_consumption = self.parser.nodeConsumptionScope(scope, 0);
    const root = try self.lowerStatement(scope, 0);
    var node: Parser.Node = first_consumption;

    var inst_id = root;
    while (len > node) {
        const consumption = self.parser.nodeConsumptionScope(scope, node);
        defer node += consumption;
        const new_inst = try self.lowerStatement(scope, node);
        if (~new_inst != 0) {
            if (~inst_id != 0) self.pool.get(inst_id).next = new_inst;
            inst_id = new_inst;
        }
    }
    return inst_id;
}
pub fn lowerStatement(self: *IR, scope: Parser.Scope, node: Parser.Node) Error!Instruction.ID {
    // const root = try self.pool.new(self.arena.allocator());
    const entry = self.parser.getNodeEntryScope(scope, node).*;
    return switch (entry) {
        .var_decl => |vd| try self.lowerVarDecl(scope, vd, node),
        else => Instruction.null_id,
    };
}

fn lowerVarDecl(self: *IR, scope: Parser.Scope, var_decl: Parser.VariableDeclaration, node: Parser.Node) Error!Instruction.ID {
    return switch (var_decl.qualifier) {
        .@"var", .shared => blk: {
            const var_kind: VariableEntry.Kind = //
                if (var_decl.qualifier == .@"var") .regular else .shared;
            const type_node = node + 2;
            const var_type: Parser.Type = @enumFromInt((try self.parser.getValue(type_node)).?.payload);

            if (self.parser.getTypeEntry(var_type).isComptime())
                break :blk Instruction.null_id;

            const initializer_node = type_node + self.parser.nodeConsumptionScope(scope, type_node);
            //put value.payload into VariableEntry.initalizer field
            if (try self.parser.getValue(initializer_node)) |iv| {
                _ = try self.addLocalVariable(.{ .kind = var_kind, .type = var_type, .initializer = iv.payload });
                break :blk Instruction.null_id;
            } else {
                //calculate variable initializer and
                //emit initializer_local_variable instruction
                //inside target scope
                const local_var_id = try self.addLocalVariable(.{ .kind = var_kind, .type = var_type });
                const initializer_id: u32 = 0;
                if (initializer_id == 0)
                    @panic("emit non comptime local variable_initializer");
                break :blk try self.pool.append(.{
                    .operands = try self.arena.allocator().dupe(u32, &.{
                        @intFromEnum(local_var_id),
                        initializer_id,
                    }),
                    .op = .initialize_local_variable,
                });
            }
        },
        .push => Instruction.null_id,
        else => Instruction.null_id,
        //if constant we skip
        //if push constant we skip for now
        //else -> add local variable
    };
}
fn addLocalVariable(self: *IR, entry: VariableEntry) Error!Variable {
    const entry_point = &self.entry_points.items[self.current_entry_point];
    try entry_point.local_variables.append(self.arena.allocator(), entry);
    return .{
        .id = @truncate(entry_point.local_variables.items.len),
        .kind = .local,
    };
}

// pub fn newInstNode(self: *IR, inst: Instruction) InstructionNode.Ptr {}
const EntryPoint = struct {
    parser_id: Parser.Function,
    body: Instruction.ID,

    name: []const u8,
    kind: Kind,
    compute_workgroup_size: u32, //??
    //some push constant info
    //input variables []Variable
    //output variables []Variable
    local_variables: List(VariableEntry) = .empty,

    const Kind = enum { vertex, fragment, compute };
};

const Variable = packed struct {
    id: u31,
    kind: Kind,
    const Kind = enum(u1) { local, global };
};
const VariableEntry = struct {
    kind: Kind,
    type: Parser.Type,
    initializer: u32 = null_initializer, //value payload
    const null_initializer = ~@as(u32, 0);
    const Kind = enum { regular, input, output, push, shared };
};

const Constant = u32;
const ConstantEntry = struct {
    id: u32,
    type: Parser.Type,
};

const Instruction = struct {
    operands: []u32 = &.{},
    op: OpCode = undefined,
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

const OpCode = enum(u32) {
    mul, //[left][right]
    add,

    //??

    swizzle, //[vec1][vec2][x][y][z][w]
    swizzle_literal, //[vector][mask][elem1]...[elem4]

    device_pointer_load, //[ptr]
    load, //[var]
    store, //[var][value]

    @"return",

    initialize_local_variable, //[localvarid][id]
};

const InstructionPool = struct {
    blocks: List([]Instruction) = .empty,
    count: u32 = 0,
    const block_size = 16;

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

fn dupeFunctionName(self: *IR, function: Parser.Function) Error![]const u8 {
    const token = self.parser.getFunctionEntry(function).name;
    const slice = self.parser.tokenizer.slice(token);
    return try self.arena.allocator().dupe(u8, slice);
}

// vertex vert: fn():void = fn () void{
//    push vertex_buffer: *f32x3 = <null>
//    @position = f32x4{'vertex_buffer[@vertex_id], 1}

// fragment frag: fn():f32x4 = fn () f32x4{
//    return f32x4{1}

//vertex data
//@builtin variables{
//  input vertex_id: u32
//  output position : f32x4
//@variables {
//  push vertex_buffer: *f32x3
//@constants {
//@shuffle_literals : '1'

//@vertex entrypoint:{
// (load) vertex_buffer = load(vertex_buffer)
// (load) vid = load(@vertex_id)
// (fmul) elem_pointer_offset = @ptrOffset(f32x3) * vid
// (fadd) elem_pointer = vertex_buffer + elem_pointer_offset
// (load_device_ptr) vertex = load_device_ptr(elem_pointer)
// (shuffle_literal) pos = exlitsh(vertex, 'x 'y 'z 1)
// (store) @position = pos
// (return) *ommited*
//}

//fragment data
//@builtin variables{
//@variables {
//  output frag_out: f32x4
//@constants {
//  vec_of1 : f32x4 = .{1, 1, 1, 1}
//@fragment entry point:{
//  (constant vec => f32x4{1,1,1,1})
//  (store) frag_out = vec
//  (return) *ommited*
//}

const Allocator = std.mem.Allocator;
const List = std.ArrayList;
const Error = hgsl.Error;
const Token = Tokenizer.Token;
