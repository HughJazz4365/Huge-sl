const std = @import("std");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Generator = @This();

const Error = error{
    OutOfMemory,

    InvalidSpirvType,
} || Writer.Error;
allocator: Allocator,
arena: Allocator,
parser: *Parser,

local_id: u32 = 0,

// spirv module structure
capabilities: Capabilities = .{}, //flag struct
extensions: Extensions = .{}, //flag struct
entry_points: List(EntryPoint) = .empty,

decorations: List(Decoration) = .empty,
types: List(TypeEntry) = .empty,
global_variables: List(GlobalVar) = .empty,
instructions: List(Instruction) = .empty,

//memory model

pub fn generate(self: *Generator) Error![]u32 {
    const magic_number: u32 = 0x07230203;
    const spirv_version_major: u8 = 1;
    const spirv_version_minor: u8 = 6;
    const version_word = @as(u32, spirv_version_major) << 16 | @as(u32, spirv_version_minor) << 8;

    const generator_magic: u32 = 0;
    _ = .{ self, magic_number, version_word, generator_magic };

    for (self.parser.global_scope.body.items) |statement| {
        switch (statement) {
            .var_decl => |var_decl| std.debug.print("{s} vd: {d}\n", .{ var_decl.name, var_decl.reference_count }),
            else => @panic("einienne"),
        }
    }
    // try self.result.appendSlice(
    // self.arena,
    // &[_]u32{ magic_number, version_word, generator_magic, 0, 0 },
    // );
    // defer self.result.items[3] = self.id;

    // for (self.parser.global_scope.body.items) |statement|
    // switch (statement) {
    // .var_decl => |var_decl| try self.generateVarDecl(var_decl),
    // else => {},
    // };
    // try self.output.print("WRITE: {d}\n", .{52});
    //algorithm:
    //go through global scope statements
    //if its a var decl of type entrypoint generate code for it

    //generate for entry point:
    //when encounter a new type add it to the used_types list
    //when encounter a new function generate an output for it

    // const result = try self.allocator.alloc(u32, self.result.items.len);
    const result: []u32 = @constCast(&[0]u32{});
    // @memcpy(result, self.result.items);
    return result;
}

// fn generateFunction(self: *Generator, var_decl: Parser.VariableDecl) Error!void {}
inline fn generateVarDecl(self: *Generator, var_decl: Parser.VariableDecl) Error!void {
    if (var_decl.type == .entrypoint)
        return try self.generateEntryPoint(
            var_decl.name,
            @ptrCast(@alignCast(var_decl.value.value.payload.ptr)),
        );
    //functions and entrypoints are so different

    _ = try self.getParserType(var_decl.type);
    if (var_decl.qualifier == .@"const") {

        //gen constant (easyy
        return;
    }
}
inline fn generateEntryPoint(self: *Generator, name: []const u8, entry_point: *Parser.EntryPoint) Error!void {
    _ = .{ self, name, entry_point };
}

fn getConstantID(self: *Generator, value: Parser.Value) Error!u32 {
    const type_id = try self.getTypeID(value.type);
    _ = type_id;
}
fn getTypeID(self: *Generator, @"type": Type) Error!u32 {
    for (self.types.items) |t|
        if (@"type".eql(t.type)) return t.id;
    const new_id = self.newid();
    try self.types.append(self.arena, .{ .type = @"type", .id = new_id });
    return new_id;
}
fn getParserType(self: *Generator, ptype: Parser.Type) Error!u32 {
    const @"type": Type = switch (ptype) {
        .number => |number| switch (number.type) {
            .float => .{ .float = .{ .width = @intFromEnum(number.width) } },
            inline else => |tag| .{ .int = .{
                .width = @intFromEnum(number.width),
                .signed = tag == .int,
            } },
        },
        .vector => |vector| .{ .vector = .{
            .component_type_id = try self.getParserType(.{ .number = vector.child }),
            .len = @intFromEnum(vector.len),
        } },

        // else => return Error.InvalidSpirvType,
        else => .void,
    };
    std.debug.print("from: {f}, to: {any}\n", .{ ptype, @"type" });
    return try self.getTypeID(@"type");
}

inline fn opWord(count: u16, op_code: u16) u32 {
    return (@as(u32, count) << 16) | @as(u32, op_code);
}

pub inline fn newid(self: *Generator) u32 {
    defer self.id += 1;
    return self.id;
}

const GlobalVar = struct {
    type_id: u32,
    id: u32,
    mut: bool,
    extra: union {
        storage_class: StorageClass,
        value: union {
            single: u32,
            many: []u32,
        },
    },
};
// 1 to 1 translatable to spirv instruction
// needed to preserve id order
const Instruction = union(enum) {
    function: OpFunction,
    label: u32,
    store: OpStore,
    return_void,
    function_end,

    add_same: GenericBinOp,
    sub_same: GenericBinOp,
    mul_same: GenericBinOp,
    vec_x_scalar, //...
    mat_x_vec,
    vec_x_mat,
    mat_x_mat,
    mat_x_scalar,

    variable: void,
    // OpFunction %void None %3
    //          %5 = OpLabel
    //               OpStore %9 %11
    //               OpReturn
    //               OpFunctio
};
// const OpFunction
const OpFunction = struct {
    result_type: u32,
    result: u32,
    function_control: FunctionControl,
    function_type: u32,
};
const FunctionControl = enum(u32) {
    none = 0,
    @"inline" = 1,
    dontinline = 2,
    pure = 3,
    @"const" = 4,
};
const OpStore = struct {
    pointer: TempID,
    value: TempID,
    memory_operands: []u32 = @constCast(&.{}),
};
const GenericBinOp = struct {
    type: u32,
    result: u32,
    a: TempID,
    b: TempID,
};
const TempID = struct {
    type: TempIDType,
    id: u32,
};
const TempIDType = union(enum) { global, func };

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

const Type = union(enum) {
    bool,
    void,

    int: IntType,
    float: FloatType,

    vector: VectorType,
    matrix: MatrixType,

    array: ArrayType,

    image: void,
    //sampled_image???

    ptr: PointerType,
    function: FunctionType,
    pub fn eql(a: Type, b: Type) bool {
        return if (std.meta.activeTag(a) != std.meta.activeTag(b)) false else switch (a) {
            .int => a.int.width == b.int.width and b.int.signed == b.int.signed,
            .float => a.float.width == b.float.width,
            .vector => a.vector.component_type == b.vector.component_type and a.vector.len == b.vector.len,
            else => std.meta.eql(a, b),
        };
    }
};
const IntType = struct { width: u32, signed: bool };
const FloatType = struct { width: u32 };

const VectorType = struct { component_type: u32, len: u32 };
const MatrixType = struct { column_type: u32, count: u32 };

const ArrayType = struct { elem_type: u32, len: u32 };

const PointerType = struct { type: u32, storage_class: StorageClass };
const FunctionType = struct { rtype: u32, arg_types: []u32 };

const EntryPointInstruction = struct {
    // exec model,
    execution_model: ExecutionModel,
    id: u32,
    name: []const u8,
    interfaces: List(u32) = .empty,
};
const ExecutionModel = enum(u32) {
    vertex = 0,
    tesselation_control = 1,
    tesselation_evaluation = 2,
    geometry = 3,
    fragment = 4,
    compute = 5,
};

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
const Expression = Parser.Expression;
