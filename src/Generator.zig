const std = @import("std");
const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Generator = @This();

const Error = error{
    OutOfMemory,
} || Writer.Error;
allocator: Allocator,
arena: Allocator,
parser: *Parser,
result: List(u32) = .empty,

id: u32 = 0,

capabilities: Capabilities = .{}, //flag struct
extensions: Extensions = .{}, //flag struct
decorations: List(Decoration) = .empty,
instructions: List(u32) = .empty,

types: List(TypeEntry) = .empty,
constants: List(usize) = .empty,
//memory model

pub fn generate(self: *Generator) Error![]u32 {
    const magic_number: u32 = 0x07230203;
    const spirv_version_major: u8 = 1;
    const spirv_version_minor: u8 = 6;
    const version_word = @as(u32, spirv_version_major) << 16 | @as(u32, spirv_version_minor) << 8;

    const generator_magic: u32 = 0;

    try self.result.appendSlice(
        self.arena,
        &[_]u32{ magic_number, version_word, generator_magic, 0, 0 },
    );
    defer self.result.items[3] = self.id;

    for (self.parser.global_scope.body.items) |statement|
        switch (statement) {
            .var_decl => |var_decl| try self.generateVarDecl(var_decl),
            else => {},
        };
    // try self.output.print("WRITE: {d}\n", .{52});
    //algorithm:
    //go through global scope statements
    //if its a var decl of type entrypoint generate code for it

    //generate for entry point:
    //when encounter a new type add it to the used_types list
    //when encounter a new function generate an output for it

    const result = try self.allocator.alloc(u32, self.result.items.len);
    @memcpy(result, self.result.items);
    return result;
}
fn generateVarDecl(self: *Generator, var_decl: Parser.VariableDecl) Error!void {
    _ = self;
    if (var_decl.qualifier == .@"const") {

        //gen constant (easyy
        return;
    }
}
fn genOpConstant(self: *Generator, value: Parser.Value) Error!void {
    const type_id = try self.typeID(value.type);
    _ = type_id;
}
fn typeID(self: *Generator, @"type": Type) Error!u32 {
    for (self.types.items) |t|
        if (@"type".eql(t.type)) return self.instructions[t.offset];
    const new_id = self.newid();
    try self.types.append(self.arena, .{ .type = @"type", .id = new_id });
    return new_id;
}
inline fn opWord(count: u16, comptime op_code: u16) u32 {
    return @as(u32, count) << 16 | @as(u32, op_code);
}

pub inline fn newid(self: *Generator) u32 {
    defer self.id += 1;
    return self.id;
}
const EntryPoint = struct {
    id: u32,
    stage_info: ShaderStageInfo,
    io: []u32,
};

const TypeEntry = struct { type: Type, id: u32 };
const Decoration = struct {};
const Capabilities = packed struct {
    shader: bool = true,
};
const Extensions = packed struct {
    glslstd: bool = true,
};

const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const List = std.ArrayList;
// const Type = tp.Type;
const Type = union(enum) {
    bool,
    void,

    int: void,
    float: void,

    vector: void,
    matrix: void,

    array: void,

    image: void,
    //sampled_image???

    ptr: PointerType,
};
const PointerType = struct { type: *Type, storage_class: StorageClass };

const StorageClass = enum(u32) {
    function = 7,

    uniform_constant = 0,
    uniform = 2,
    push_constant = 9,

    input = 1,
    output = 3,

    workgroup = 4,
    cross_workgroup = 5,

    atomic_counter = 10,
    image = 11,
    storage_buffer = 12,
};
//need a new 'type' type
// to account for pointer stuff
const ShaderStageInfo = Parser.ShaderStageInfo;
