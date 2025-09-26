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
types: List(TypeEntry) = .empty,
decorations: List(Decoration) = .empty,
instructions: List(u32) = .empty,

constants: List(Constant) = .empty,
constants_composite: List(ConstantComposite) = .empty,
//store constants somehow
// List(u32)?

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

    for (self.parser.global_scope.body.items) |statement| {
        _ = statement;
        // try self.output.print("WRITE: {d}\n", .{52});
        //algorithm:
        //go through global scope statements
        //if its a var decl of type entrypoint generate code for it

        //generate for entry point:
        //when encounter a new type add it to the used_types list
        //when encounter a new function generate an output for it
    }

    const result = try self.allocator.alloc(u32, self.result.items.len);
    @memcpy(result, self.result.items);
    return result;
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

const Constant = struct { id: u32, value: u32, type: Type };
const ConstantComposite = struct { id: u32, components: []u32, type: Type };
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
const Type = tp.Type;
const ShaderStageInfo = Parser.ShaderStageInfo;
