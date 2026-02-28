const Parser = @import("Parser.zig");
const IR = @This();

//IR -> struct{
//list of EntryPoints
//list of functions
//list of global variables
//list of constants
//extension information?
//}

//struct EntryPoint{
//  kind: enum{vertex, fragment, compute}
//  workgroup size : [3]u32
//  body: List(Op)??

//  input, output: List(struct{name, type, interpolation})
//  push constants: ???
//}

//struct FunctionEntry{
//body: List(Op)???
//argtypes: List(type)
//rtype: type
//}

//most of operation on this IR
//will disrupt the instruction order/ insert new instructions
//so Linked list?

pub fn lower(parser: *Parser) IR {
    _ = parser;
    return .{};
}

const Instruction = enum {
    umul,
    uadd,

    extra_literal_shuffle, //??

    device_pointer_dereference,
    load,
    store,
};

// vertex vert: fn():void = fn () void{
//    push vertex_buffer: *f32x3 = <null>
//    @position = f32x4{'vertex_buffer[@vertex_id], 1}

// fragment frag: fn():f32x4 = fn () f32x4{
//    return f32x4{1}

//vertex entrypoint:{
// (load) vid = load(@vertex_id)
// (mul) elem_pointer_offset = @ptrOffset(f32x3) * vid
// (add) elem_pointer = vertex_buffer + elem_pointer_offset
// (extra_literal_shuffle) pos = exlitsh(vertex_buffer, 'x 'y 'z 1)
// (store) @position = pos
// (return) *ommited*
//}

//fragment entry point:{
//  (constant vec => f32x4{1,1,1,1})
//  (store) frag_out = vec
//  (return) *ommited*
//}
