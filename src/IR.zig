const std = @import("std");
const hgsl = @import("root.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const IR = @This();

arena: std.heap.ArenaAllocator,
parser: *Parser,

//result must have no unused constants/variables/types/functions
//IR -> struct{
//arena
//list of EntryPoints
//entry_point_aliases
//list of functions
//list of global variables
//list of constants
//shuffle_literals
//extension information?
//}

//struct FunctionEntry{
//body: List(Op)???
//argtypes: List(type)
//rtype: type
//}

//most of operation on this IR
//will disrupt the instruction order/ insert new instructions
//so Linked list?

//create valid_ variations of statement NodeEntries
//and go through vertex/fragment/compute functions
// and convert bodies to ir instructions
pub fn lower(self: *IR) Error!void {
    _ = self;
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

const EntryPoint = struct {
    body: InstructionNode.Ptr,

    name: Token,
    kind: Kind,
    compute_workgroup_size: u32, //??
    //some push constant info

    const Kind = enum { vertex, fragment, compute };
};

const VariableEntry = struct {
    kind: Kind,
    type: Parser.Type,
    const Kind = enum { regular, input, output, push, shared };
};

const ConstantEntry = struct {
    id: u32,
    type: Parser.Type,
};

const InstructionNode = struct {
    value: Instruction,
    next: Ptr,

    const Ptr = *allowzero InstructionNode;
    const nul: Ptr = @ptrFromInt(0);
};

const Instruction = struct {
    operands: []u32,
    op: OpCode,
    result: Id,
};

const Value = u32;
const Id = u32;
const Var = u32;
const Const = u32;

const OpCode = enum(u32) {
    umul, //[left][right]
    uadd,

    //??

    swizzle, //[vec1][vec2][x][y][z][w]
    swizzle_literal, //[vector][mask][elem1]...[elem4]

    device_pointer_load, //[ptr]
    load, //[var]
    store, //[var][value]

    @"return",
};

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
const Error = hgsl.Error;
const Token = Tokenizer.Token;
