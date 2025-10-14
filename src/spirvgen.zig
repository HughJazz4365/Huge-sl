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

id: u32 = 1, //id of '0' is reserved

// spirv module structure
capabilities: Capabilities = .{}, //flag struct
extensions: Extensions = .{}, //flag struct
entry_points: List(EntryPoint) = .empty,

decorations: List(u32) = .empty,
types: List(TypeEntry) = .empty,

instructions: List(u32) = .empty,

global_variables: List(GlobalVariable) = .empty,
global_constants: List(GlobalConstantEntry) = .empty,
global_name_mappings: List(NameMapping) = .empty,

current_name_mappings: []NameMapping = &.{},
current_buf: *List(u32) = undefined,
current_interfaces: *List(u32) = undefined,
in_function: bool = false,

pub fn generateDissasembly() Error![]const u8 {
    //TODO:
    // dissasemble(u32) also
}
pub fn generate(parser: *Parser) Error![]u32 {
    var self: Generator = .{
        .parser = parser,
        .allocator = parser.allocator,
        .arena = parser.arena.allocator(),
    };
    //generate all the stuff by parsing global scope of parsed data
    for (self.parser.global_scope.body.items) |statement|
        try self.generateStatement(statement);
    // for (self.types.items) |t| std.debug.print("T: {any}\n", .{t});
    // for (self.global_constants.items) |c| std.debug.print("C: {any}\n", .{c});
    // for (self.global_variables.items) |gv| std.debug.print("GV: {any}\n", .{gv});
    //TODO: ENABLE DYNAMICALY
    self.capabilities.shader = true;

    //compile all the parts into one slice
    const spirv_magic: u32 = 0x07230203;
    const spirv_version_major: u8 = 1;
    const spirv_version_minor: u8 = 6;
    const version_word = @as(u32, spirv_version_major) << 16 | @as(u32, spirv_version_minor) << 8;
    const generator_magic: u32 = 0;

    var result_unmanaged: List(u32) = .empty;
    var result = result_unmanaged.toManaged(self.allocator);

    //add header
    try result.appendSlice(&.{ spirv_magic, version_word, generator_magic, self.id, 0 });
    //enable capabilities
    inline for (@typeInfo(Capabilitiy).@"enum".fields) |ef|
        if (@field(self.capabilities, ef.name))
            try result.appendSlice(&.{ opWord(2, .capability), ef.value });
    //extensions
    // //////////////
    //

    //logical addressing model(0)
    //memory model is Vulkan(3)/GLSL450(1)???
    try result.appendSlice(&.{ opWord(3, .memory_model), 0, 1 });

    //entry point
    for (self.entry_points.items) |entry_point| {
        const execution_mode: u32 = switch (entry_point.stage_info) {
            .vertex => 0,
            .fragment => 4,
            .compute => 5,
        };
        const name_len = (entry_point.name.len + 4) / 4;

        try result.appendSlice(&.{
            opWord(@truncate(3 + entry_point.io.len + name_len), .entry_point),
            execution_mode,
            entry_point.id,
        });
        const name_index = result.items.len;
        try result.appendNTimes(0, name_len);
        for (entry_point.name, 0..) |char, i|
            result.items[name_index + i / 4] |= @as(u32, char) << @intCast((i & 3) * 8);
        try result.appendSlice(entry_point.io);
    }
    //execution modes
    for (self.entry_points.items) |entry_point| {
        switch (entry_point.stage_info) {
            .fragment => try result.appendSlice(&.{
                opWord(3, .execution_mode),
                entry_point.id,
                @intFromEnum(ExecutionMode.origin_upper_left),
            }),

            else => {},
            // .vertex => 0,
            // .fragment => 4,
            // .compute => 5,
        }
    }

    //append decorations section
    try result.appendSlice(self.decorations.items);

    //add type decls
    for (self.types.items) |t| try result.appendSlice(switch (t.type) {
        .void => &.{ opWord(2, .type_void), t.id },
        .bool => &.{ opWord(2, .type_bool), t.id },
        .int => |int| &.{ opWord(4, .type_int), t.id, int.width, @intFromBool(int.signed) },
        //extra parameters for 8,16 bit floats
        .float => |float| &.{ opWord(3, .type_float), t.id, float.width },
        .vector => |vector| &.{ opWord(4, .type_vector), t.id, vector.component_type, vector.len },
        .pointer => |pointer| &.{ opWord(4, .type_pointer), t.id, @intFromEnum(pointer.storage_class), pointer.type },
        .function => |function| blk: {
            try result.appendSlice(
                &.{ opWord(@intCast(3 + function.arg_types.len), .type_function), t.id, function.rtype },
            );
            break :blk function.arg_types;
        },
        else => @panic("cant generate instruction for that type decl for now"),
    });
    //add constant decl instructions
    for (self.global_constants.items) |c| {
        // const index = result.items.len;
        const @"type" = self.getTypeFromID(c.payload.type);
        switch (@"type") {
            .bool => try result.appendSlice(&.{
                opWord(3, if (c.payload.value.single > 0) .constant_true else .constant_false),
                c.payload.type,
                c.id,
            }),

            .float, .int => {
                const width = (if (@"type" == .float)
                    @"type".float.width
                else
                    @"type".int.width);
                try result.appendSlice(&.{
                    opWord(@intCast(3 + (width >> 5)), .constant),
                    c.payload.type,
                    c.id,
                });
                if (width > 32) {
                    try result.appendSlice(c.payload.value.many);
                } else try result.append(c.payload.value.single);
            },
            .vector => |vector| {
                try result.appendSlice(&.{
                    opWord(@intCast(3 + vector.len), .constant_composite),
                    c.payload.type,
                    c.id,
                });
                try result.appendSlice(c.payload.value.many);
            },

            else => @panic("invalid constant type"),
        }
        // std.debug.print("genconst: {any}\n", .{result.items[index..]});
    }

    //add all the global variables
    for (self.global_variables.items) |gv| {
        try result.appendSlice(&.{
            opWord(@intCast(if (gv.initializer != null) @as(u16, 5) else @as(u16, 4)), .variable),
            gv.type,
            gv.id,
            @intFromEnum(gv.storage_class),
        });
        if (gv.initializer) |i| try result.append(i);
    }

    //add the rest of the module
    try result.appendSlice(self.instructions.items);

    return try result.toOwnedSlice();
}

fn generateStatement(self: *Generator, statement: Parser.Statement) Error!void {
    switch (statement) {
        .var_decl => |var_decl| try self.generateVarDecl(var_decl),
        else => @panic("cant gen that statement"),
    }
}
fn generateVarDecl(self: *Generator, var_decl: Parser.VariableDeclaration) Error!void {
    // std.debug.print("{s} vd: {d}\n", .{ var_decl.name, var_decl.reference_count });
    if (var_decl.type == .entrypoint)
        return try self.generateEntryPoint(
            var_decl.name,
            @as(*const Parser.EntryPoint, @ptrCast(@alignCast(var_decl.initializer.value.payload.ptr))).*,
        );

    //skip variables of 'incomplete' types
    const @"type" = self.convertParserType(var_decl.type) catch |err| if (err == Error.InvalidSpirvType) return else return err;
    const type_id = try self.getTypeID(@"type");

    std.debug.print("VALEXPR: {any}\n", .{var_decl.initializer});
    const value: ?u32 = if (!var_decl.initializer.isEmpty())
        try self.generateExpressionID(var_decl.initializer)
    else
        null;

    //add name mappings!
    if (var_decl.qualifier == .@"const") return;

    const storage_class: StorageClass = switch (var_decl.qualifier) {
        .@"var" => if (self.in_function) .function else .private,
        .in => .input,
        .out => .output,
        else => @panic("idk how whats the storage class of this qualifier"),
    };
    const ptr_type_id = try self.getTypeID(
        .{ .pointer = .{ .type = type_id, .storage_class = storage_class } },
    );

    const new_id = self.newID();
    if (storage_class == .function) {
        const consumed: u16 = if (value != null) 5 else 4;
        try self.current_buf.appendSlice(
            self.arena,
            &.{ opWord(consumed, .variable), ptr_type_id, new_id, @intFromEnum(storage_class) },
        );
        if (value) |v| try self.current_buf.append(self.arena, v);
    } else {
        try self.global_variables.append(self.arena, .{
            .id = new_id,
            .type = ptr_type_id,
            .storage_class = storage_class,
            .initializer = value,
            .from_function = self.in_function,
        });
        const gi = self.global_variables.items.len - 1;
        if (self.in_function) {
            if (storage_class == .input or storage_class == .output) {
                try self.current_interfaces.append(self.arena, @truncate(gi));
                try self.decorateLocationEntryPointIO(storage_class);
            }
        } else {
            if (storage_class == .input or storage_class == .output)
                try self.decorateLocationGlobalIo(storage_class);
        }
    }
}
fn decorateLocationGlobalIo(self: *Generator, storage_class: StorageClass) Error!void {
    var location: u32 = 0;
    for (self.global_variables.items[0 .. self.global_variables.items.len - 1]) |gv| {
        if (gv.storage_class != storage_class) continue;
        const location_consumption = 1;
        location += location_consumption;
    }
    try self.decorations.appendSlice(self.arena, &.{
        opWord(4, .decorate),
        self.global_variables.items[self.global_variables.items.len - 1].id,
        @intFromEnum(Decoration.location),
        location,
    });
}
//decorate last element of current_interfaces list with the correct location
fn decorateLocationEntryPointIO(self: *Generator, storage_class: StorageClass) Error!void {
    var location: u32 = 0;

    for (self.current_interfaces.items[0 .. self.current_interfaces.items.len - 1]) |i| {
        const variable = self.global_variables.items[i];
        if (variable.storage_class != storage_class) continue;

        const location_consumption = 1;
        location += location_consumption;
    }
    try self.decorations.appendSlice(self.arena, &.{
        opWord(4, .decorate),
        self.global_variables.items[self.current_interfaces.items[self.current_interfaces.items.len - 1]].id,
        @intFromEnum(Decoration.location),
        location,
    });
}
fn generateExpressionID(self: *Generator, expr: Expression) Error!u32 {
    const id = switch (expr) {
        .value => |value| try self.generateValueID(value),
        else => Error.GenError,
    };
    return id;
}
fn generateValueID(self: *Generator, value: Parser.Value) Error!u32 {
    const @"type" = self.convertParserType(value.type) catch unreachable;
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
                                    .wide = util.fit(Parser.WIDE, @as(*const T, @ptrCast(@alignCast(value.payload.ptr)))[i]),
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
        if (c.payload.eql(global_constant, self)) return @truncate(c.id);
    }
    const new_id = self.newID();
    try self.global_constants.append(self.arena, .{ .payload = global_constant, .id = new_id });
    return new_id;
}

fn generateEntryPoint(self: *Generator, name: []const u8, entry_point: Parser.EntryPoint) Error!void {
    const rtype = try self.getTypeID(.void);
    const function_type = try self.getTypeID(.{ .function = .{ .rtype = rtype } });
    const result = self.newID();
    var buf: List(u32) = .empty;
    var interfaces: List(u32) = .empty;

    for (self.global_variables.items, 0..) |gv, i|
        if (gv.storage_class == .input or gv.storage_class == .output and !gv.from_function)
            try interfaces.append(self.arena, @truncate(i));

    //op function
    try buf.appendSlice(
        self.arena,
        &.{ opWord(5, .function), rtype, result, @intFromEnum(FunctionControl.none), function_type },
    );
    //op label
    try buf.appendSlice(self.arena, &.{ opWord(2, .label), self.newID() });
    const was_in_function = self.in_function;
    self.in_function = true;
    self.current_interfaces = &interfaces;

    self.current_buf = &buf;
    for (entry_point.body.items) |statement| try self.generateStatement(statement);

    if (!was_in_function) self.in_function = false;

    const io = try self.arena.alloc(u32, interfaces.items.len);
    for (interfaces.items, 0..) |interface, i| io[i] = self.global_variables.items[interface].id;

    //op return
    try buf.append(self.arena, opWord(1, .@"return"));
    //op function end
    try buf.append(self.arena, opWord(1, .function_end));
    //track entry point
    try self.entry_points.append(self.arena, .{
        .id = result,
        .name = name,
        .stage_info = entry_point.stage_info,
        .io = io, //get from parsing the body
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
fn convertParserType(self: *Generator, ptype: Parser.Type) Error!Type {
    return switch (ptype) {
        .number => |number| switch (number.type) {
            .float => .{ .float = .{ .width = @intFromEnum(number.width) } },
            else => |tag| .{ .int = .{
                .width = @intFromEnum(number.width),
                .signed = tag == .int,
            } },
        },
        .vector => |vector| .{ .vector = .{
            .component_type = try self.getTypeID(try self.convertParserType(.{ .number = vector.component })),
            .len = @intFromEnum(vector.len),
        } },
        .void => .void,
        else => {
            std.debug.print("T in question: {f}\n", .{ptype});
            return Error.InvalidSpirvType;
        },
    };
}

fn opWord(count: u16, op: Op) u32 {
    return (@as(u32, count) << 16) | @intFromEnum(op);
}

pub fn newID(self: *Generator) u32 {
    defer self.id += 1;
    return self.id;
}
const Op = enum(u32) {
    constant_true = 41,
    constant_false = 42,
    constant = 43,
    constant_composite = 44,

    type_void = 19,
    type_bool = 20,
    type_int = 21,
    type_float = 22,
    type_vector = 23,
    type_matrix = 24,
    type_pointer = 32,
    type_function = 33,

    capability = 17,
    entry_point = 15,
    execution_mode = 16,
    memory_model = 14,
    decorate = 71,

    function = 54,
    label = 248,
    @"return" = 253,
    function_end = 56,

    variable = 59,
};
const Decoration = enum(u32) {
    location = 30,
};

const GlobalVariable = struct {
    id: u32,
    type: u32,
    storage_class: StorageClass,
    initializer: ?u32,
    from_function: bool,
};
const GlobalConstantEntry = struct { id: u32, payload: GlobalConstant };
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
const Capabilities = structFromEnum(Capabilitiy, bool);
const Capabilitiy = enum(u32) {
    matrix = 0,
    shader = 1,
    geometry = 2,
    tesselation = 3,

    float64 = 10,
    float16 = 9,
    int64 = 11,
    int16 = 22,

    atomic_storage = 21,
    int64atomics = 12,

    geometry_point_size = 24,

    storage_image_multisample = 27,
    //there is too much of them
};

const Extensions = struct {
    glslstd: ?u32 = null,
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

    pointer: PointerType,
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

const ExecutionMode = enum(u32) {
    origin_upper_left = 7,
    origin_lower_left = 8,
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
    private = 6,

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
fn structFromEnum(Enum: type, T: type) type {
    const em = @typeInfo(Enum).@"enum";
    var struct_fields: [em.fields.len]std.builtin.Type.StructField = undefined;
    const zeroes = std.mem.zeroes(T);
    inline for (em.fields, &struct_fields) |ef, *sf| {
        sf.* = .{
            .default_value_ptr = &zeroes,
            .alignment = @alignOf(T),
            .is_comptime = false,
            .name = ef.name,
            .type = T,
        };
    }
    return @Type(.{ .@"struct" = .{
        .decls = &.{},
        .fields = &struct_fields,
        .is_tuple = false,
        .layout = .auto,
    } });
}
//need a new 'type' type
// to account for pointer stuff
const ShaderStageInfo = Parser.ShaderStageInfo;
const Expression = Parser.Expression;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const List = std.ArrayList;
