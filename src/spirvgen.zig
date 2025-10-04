const std = @import("std");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Generator = @This();

const Error = error{
    OutOfMemory,

    InvalidSpirvType,

    GenError,
} || Writer.Error;
allocator: Allocator,
arena: Allocator,
parser: *Parser,

id: u32 = 0,

// spirv module structure
capabilities: Capabilities = .{}, //flag struct
extensions: Extensions = .{}, //flag struct
entry_points: List(EntryPoint) = .empty,

decorations: List(Decoration) = .empty,
types: List(TypeEntry) = .empty,
global_variables: List(GlobalVariableEntry) = .empty,
global_constants: List(GlobalConstantEntry) = .empty,
instructions: List(u32) = .empty,

global_name_mappings: List(NameMapping) = .empty,
current_name_mappings: []NameMapping = &.{},

current_buf: *List(u32) = undefined,
in_function: bool = false,

pub fn new(parser: *Parser) Generator {
    return .{
        .parser = parser,
        .allocator = parser.allocator,
        .arena = parser.arena.allocator(),
    };
}
pub fn generate(self: *Generator) Error![]u32 {
    const magic_number: u32 = 0x07230203;
    const spirv_version_major: u8 = 1;
    const spirv_version_minor: u8 = 6;
    const version_word = @as(u32, spirv_version_major) << 16 | @as(u32, spirv_version_minor) << 8;

    const generator_magic: u32 = 0;
    _ = .{ self, magic_number, version_word, generator_magic };

    for (self.parser.global_scope.body.items) |statement|
        try self.generateStatement(statement);
    for (self.types.items) |t| std.debug.print("T: {any}\n", .{t});
    for (self.global_constants.items) |c| std.debug.print("C: {any}\n", .{c});

    const result: []u32 = @constCast(&[0]u32{});
    return result;
}

fn generateStatement(self: *Generator, statement: Parser.Statement) Error!void {
    std.debug.print("STATEMENT: {f}\n", .{statement});
    switch (statement) {
        .var_decl => |var_decl| try self.generateVarDecl(var_decl),
        else => @panic("cant gen that statement"),
    }
}
fn generateVarDecl(self: *Generator, var_decl: Parser.VariableDecl) Error!void {
    std.debug.print("{s} vd: {d}\n", .{ var_decl.name, var_decl.reference_count });
    if (var_decl.type == .entrypoint)
        return try self.generateEntryPoint(
            var_decl.name,
            @as(*const Parser.EntryPoint, @ptrCast(@alignCast(var_decl.value.value.payload.ptr))).*,
        );

    //skip variables of 'incomplete' types
    _ = self.castParserType(var_decl.type) catch |err| if (err == Error.InvalidSpirvType) return else return err;

    const value: ?u32 = if (!var_decl.value.isEmptyExpression())
        try self.generateExpressionID(var_decl.value)
    else
        null;
    return switch (var_decl.qualifier) {
        .@"const" => value.?,
        .out => ,
        else => @panic("cant gen var with this qualifier"),
    };

    // std.debug.print("exprid: {any}\n", .{value});
}
fn generateExpressionID(self: *Generator, expr: Expression) Error!u32 {
    return switch (expr) {
        .value => |value| try self.generateValueID(value),
        else => Error.GenError,
    };
}
fn generateValueID(self: *Generator, value: Parser.Value) Error!u32 {
    const @"type" = self.castParserType(value.type) catch unreachable;
    const type_id = try self.getTypeID(@"type");
    return switch (value.type) {
        .number => |number| switch (number.width) {
            inline else => |width| switch (number.type) {
                inline else => |nt| try self.getGlobalConstID(.{
                    .type = type_id,
                    .value = blk: {
                        const compt: Parser.Type = .{ .number = .{ .type = nt, .width = width } };
                        const T = compt.ToZig();
                        break :blk if (width == .long) .{
                            //reorder words in that slice
                            .many = @ptrCast(@alignCast(
                                @as(*T, @ptrCast(@alignCast(@constCast(
                                    &value.payload.wide,
                                )))),
                            )),
                        } else .{ .single = util.fit(u32, util.extract(T, value.payload.wide)) };
                    },
                }),
            },
        },

        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.component.width) {
                inline else => |width| switch (vector.component.type) {
                    inline else => |nt| blk: {
                        const compt: Parser.Type = .{ .vector = .{ .len = len, .component = .{ .type = nt, .width = width } } };
                        const T = compt.ToZig();
                        const vec_len = @intFromEnum(len);

                        const slice = try self.arena.alloc(u32, vec_len);
                        inline for (0..vec_len) |i|
                            slice[i] = try self.generateValueID(.{
                                .type = .{ .number = compt.vector.component },
                                .payload = .{
                                    .wide = util.fit(u128, @as(*const T, @ptrCast(@alignCast(value.payload.ptr)))[i]),
                                },
                            });

                        break :blk try self.getGlobalConstID(.{
                            .type = type_id,
                            .value = .{ .many = slice },
                        });
                    },
                },
            },
        },

        // else => unreachable,
        else => @panic("unhandled gen value type"),
    };
}
fn getGlobalConstID(self: *Generator, global_constant: GlobalConstant) Error!u32 {
    for (self.global_constants.items) |c| {
        if (c.value.eql(global_constant, self)) return @truncate(c.id);
    }
    const new_id = self.newID();
    try self.global_constants.append(self.arena, .{ .value = global_constant, .id = new_id });
    return new_id;
}

fn generateEntryPoint(self: *Generator, name: []const u8, entry_point: Parser.EntryPoint) Error!void {
    _ = .{ self, name, entry_point };
    const rtype = try self.getTypeID(.void);
    const function_type = try self.getTypeID(.{ .function = .{ .rtype = rtype } });
    const result = self.newID();
    var buf: List(u32) = .empty;

    //op function
    try buf.appendSlice(
        self.arena,
        &.{ opWord(5, 54), rtype, result, @intFromEnum(FunctionControl.none), function_type },
    );
    const was_in_function = self.in_function;
    self.in_function = true;

    self.current_buf = &buf;
    for (entry_point.body.items) |statement| try self.generateStatement(statement);

    if (!was_in_function) self.in_function = false;

    //op function end
    try buf.append(self.arena, opWord(1, 56));
    //track entry point
    try self.entry_points.append(self.arena, .{
        .id = result,
        .name = name,
        .stage_info = entry_point.stage_info,
        .io = &.{}, //get from parsing the body
    });

    try self.instructions.appendSlice(self.arena, buf.items);
    buf.deinit(self.arena);
}

fn getConstantID(self: *Generator, value: Parser.Value) Error!u32 {
    const type_id = try self.getTypeID(value.type);
    _ = type_id;
}
fn getTypeFromID(self: *Generator, id: u32) Type {
    return for (self.types.items) |t| {
        if (t.id == id) break t.type;
    } else unreachable;
}
fn getTypeID(self: *Generator, @"type": Type) Error!u32 {
    for (self.types.items) |t|
        if (@"type".eql(t.type)) return t.id;
    const new_id = self.newID();
    try self.types.append(self.arena, .{ .type = @"type", .id = new_id });
    return new_id;
}
fn castParserType(self: *Generator, ptype: Parser.Type) Error!Type {
    return switch (ptype) {
        .number => |number| switch (number.type) {
            .float => .{ .float = .{ .width = @intFromEnum(number.width) } },
            else => |tag| .{ .int = .{
                .width = @intFromEnum(number.width),
                .signed = tag == .int,
            } },
        },
        .vector => |vector| .{ .vector = .{
            .component_type = try self.getTypeID(try self.castParserType(.{ .number = vector.component })),
            .len = @intFromEnum(vector.len),
        } },
        .void => .void,
        else => return Error.InvalidSpirvType,
    };
}

fn opWord(count: u16, op_code: u16) u32 {
    return (@as(u32, count) << 16) | @as(u32, op_code);
}

pub fn newID(self: *Generator) u32 {
    defer self.id += 1;
    return self.id;
}

const GlobalVariableEntry = struct { id: u32, value: GlobalVariable };
const GlobalVariable = struct {
    type_id: u32,
    storage_class: StorageClass,
};
const GlobalConstantEntry = struct { id: u32, value: GlobalConstant };
const GlobalConstant = struct {
    type: u32,
    value: union {
        single: u32,
        many: []u32,
    },
    pub fn eql(a: GlobalConstant, b: GlobalConstant, self: *Generator) bool {
        if (a.type != b.type) return false;
        return if (self.getTypeFromID(a.type).constructorIDCount() == 1)
            a.value.single == b.value.single
        else
            std.mem.eql(u32, a.value.many, b.value.many);
    }
};
const NameMapping = struct {
    id: u32,
    name: []const u8,
};
const FunctionControl = enum(u32) {
    none = 0,
    @"inline" = 1,
    dontinline = 2,
    pure = 3,
    @"const" = 4,
};

const EntryPoint = struct {
    id: u32,
    name: []const u8,
    stage_info: ShaderStageInfo,
    io: []u32,
};

const TypeEntry = struct { id: u32, type: Type };
const Decoration = struct {};
const Capabilities = packed struct {
    shader: bool = true,
};
const Extensions = packed struct {
    glslstd: bool = true,
};

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
    pub fn constructorIDCount(t: Type) u32 {
        return switch (t) {
            .int => |int| (int.width + 31) >> 5,
            .float => |float| (float.width + 31) >> 5,
            .vector => |vector| vector.len,

            else => 1,
        };
    }
};
const IntType = struct { width: u32, signed: bool };
const FloatType = struct { width: u32 };

const VectorType = struct { component_type: u32, len: u32 };
const MatrixType = struct { column_type: u32, count: u32 };

const ArrayType = struct { elem_type: u32, len: u32 };

const PointerType = struct { type: u32, storage_class: StorageClass };
const FunctionType = struct { rtype: u32, arg_types: []u32 = &.{} };

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
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const List = std.ArrayList;
