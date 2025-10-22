//module structure:
// capabilities
// extension, exstension instruction imports
// memory model
// op entry points
// execution modes
// // debug
// decorations
// types, constants, global variables
// all functions
const std = @import("std");
const util = @import("util.zig");
const zigbuiltin = @import("builtin");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Generator = @This();

const WORD = u32;

const Error = error{
    OutOfMemory,
    InvalidSpirvType,
    GenError,
} || Writer.Error || Parser.Error;

const glsl_std_id: WORD = 1;
parser: *Parser,
current_id: WORD = 2, //0 - reserved,1 - glsl ext for now
arena: Allocator,

//constants
true_const: WORD = 0,
false_const: WORD = 0,
entry_points: List(EntryPoint) = .empty,
decorations: void = {},
constants: List(Constant) = .empty,
global_vars: List(GlobalVariable) = .empty,

deferred_global_initializers: List(GlobalInitializer) = .empty,

main_buffer: List(WORD) = .empty,

current_interfaces_ids: *List(WORD) = @ptrCast(@alignCast(@constCast(&0))),
current_buffer: *List(WORD) = @ptrCast(@alignCast(@constCast(&0))),
//does this break on nested scopes??? it shoulndt if tied to function
current_name_mappings: *List(NameMapping) = @ptrCast(@alignCast(@constCast(&0))),

type_decls: List(TypeDeclaration) = .empty,
pub fn generate(parser: *Parser) Error![]u32 {
    var generator: Generator = .{
        .parser = parser,
        .arena = parser.arena.allocator(),
    };
    generator.current_buffer = &generator.main_buffer;
    for (generator.parser.global_scope.body.items) |statement| try generator.generateStatment(statement);

    _ = &generator;
    return &.{};
}
fn inGlobalScope(self: *Generator) bool {
    return @intFromPtr(self.current_buffer) == @intFromPtr(&self.main_buffer);
}

fn generateStatment(self: *Generator, statement: Parser.Statement) Error!void {
    //non formal statment types
    // - function/entry point decl
    //   keep the body in separate buffer so that inner function declarations go before they used
    //   if(entry point)
    //      track all the references to global variables, op entry point
    //   else
    //      add name mapping to the returned id
    // - const decl
    //   get value_id and add name mapping
    // - variable decl
    //     global = qualifier != .mut
    //     if(global)
    //       if(in entrypoint) add interface id
    //       create global var if can add initializer id, if cant track initializer and add assignment to each affected entry point
    //     get value_id and load it into create variable
    //     add name mapping
    //     return variable id
    // - comptime only var decl
    //     skip
    // - assignment
    //     get value id
    //     load store etc
    // - noreturn function call
    //     call idk
    // - @discard
    // - @barrier
    // - ...

    return switch (statement) {
        .var_decl => |var_decl| try self.generateVariableDeclaration(var_decl),
        else => @panic("cant gen such statement"),
    };
}
fn generateVariableDeclaration(self: *Generator, var_decl: Parser.VariableDeclaration) Error!void {
    if (var_decl.type == .entrypoint)
        return try self.generateEntryPoint(@as(*Parser.EntryPoint, @ptrCast(@alignCast(@constCast(var_decl.initializer.value.payload.ptr)))));
    if (var_decl.type.isComptimeOnly()) return;

    const storage_class: StorageClass = switch (var_decl.qualifier) {
        .mut => if (self.inGlobalScope()) .private else .function,
        .push => .push_constant,
        .in => .input,
        .out => .output,
        .@"const" => {
            const val = try self.generateExpression(var_decl.initializer);
            try self.current_name_mappings.append(self.arena, .{
                .type_id = try self.convertTypeID(var_decl.type),
                .id = val,
                .name = var_decl.name,
                .load = val,
            });
            return;
        },
        //uniform, shared
        else => @panic("idk storage class of this qualifier"),
    };
    const type_id = try self.convertTypeID(var_decl.type);
    //generate pointer type
    _ = try self.typeID(.{ .pointer = .{
        .pointed_id = type_id,
        .storage_class = storage_class,
    } });
    const var_id = self.newID();
    var initializer: WORD = 0;
    var load: WORD = 0;

    if (!var_decl.initializer.isEmpty()) switch (var_decl.initializer) {
        .identifier => |identifier| initializer = for (self.global_vars.items) |gv| (if (util.strEql(gv.name, identifier)) break gv.id) else 0,
        .value => |value| {
            if (!var_decl.initializer.isEmpty()) initializer = try self.generateValue(value);
        },
        else => if (self.inGlobalScope()) {
            try self.deferred_global_initializers.append(self.arena, .{
                .index = @truncate(self.global_vars.items.len),
                .expr = &var_decl.initializer,
            });
        } else {
            load = try self.generateExpression(var_decl.initializer);
            try self.addWords(&.{ opWord(.store, 3), var_id, load });
        },
    };
    // if (initializer != 0) load = initializer;

    //track the variable
    if (storage_class == .function) {
        try self.current_name_mappings.append(self.arena, .{ .type_id = type_id, .id = var_id, .name = var_decl.name, .load = load });
    } else try self.global_vars.append(self.arena, .{
        .name = var_decl.name,
        .type_id = type_id,
        .id = var_id,
        .storage_class = storage_class,

        .initializer = initializer,
        .load = 0,
    });

    // if (!self.inGlobalScope()) for (self.current_name_mappings.items) |nm| std.debug.print("NM: {s}, id: {d} \n", .{ nm.name, nm.id });
}

fn generateEntryPoint(self: *Generator, entry_point: *Parser.EntryPoint) Error!void {
    var buffer: List(WORD) = .empty;
    var interfaces_ids: List(WORD) = .empty;
    var name_mappings: List(NameMapping) = .empty;

    self.parser.current_scope = &entry_point.scope;
    defer self.parser.current_scope = entry_point.scope.parent;

    self.current_buffer = &buffer;
    self.current_interfaces_ids = &interfaces_ids;
    self.current_name_mappings = &name_mappings;
    defer self.current_buffer = &self.main_buffer;

    const entry_point_id = self.newID();

    try self.addWords(&.{
        opWord(.function, 5),
        try self.typeID(.void),
        entry_point_id,
        @intFromEnum(FunctionControl.none),
        try self.typeID(.{ .function = .{ .rtype_id = try self.typeID(.void) } }),
    });
    try self.addWords(&.{ opWord(.label, 2), self.newID() });

    for (entry_point.body.items) |statement| try self.generateStatment(statement);
    std.debug.print("entry point Buffer.len: {d}\n", .{buffer.items.len});

    try self.entry_points.append(self.arena, .{
        .id = entry_point_id,
        .exec_model_info = entry_point.exec_model_info,
        .interface_ids = try interfaces_ids.toOwnedSlice(self.arena),
    });
    try self.addWords(&.{ opWord(.@"return", 1), opWord(.function_end, 1) });
}
fn generateExpression(self: *Generator, expr: Expression) Error!WORD {
    const result_type_id = try self.convertTypeID(try self.parser.typeOf(expr));

    const result = switch (expr) {
        .value => |value| try self.generateValue(value),
        .bin_op => |bin_op| try self.generateBinOp(bin_op, result_type_id),
        .u_op => |u_op| try self.generateUOp(u_op, result_type_id),
        .identifier => |identifier| try self.generateVariableLoad(identifier),
        .call => |call| try self.generateCall(call, result_type_id),
        .constructor => |constructor| try self.generateConstructor(constructor.components, result_type_id),
        else => {
            std.debug.print("Cannot gen expr: {f}\n", .{expr});
            @panic("idk how to gen that expr");
        },
    };
    std.debug.print("id: {d}, Expr: {f}\n", .{ result, expr });
    return result;
}
fn generateConstructor(self: *Generator, components: []Expression, result_type_id: WORD) Error!WORD {
    const id = self.newID();
    try self.addWords(&.{ opWord(.constant_composite, @truncate(3 + components.len)), result_type_id, id });
    for (components) |c| try self.addWord(try self.generateExpression(c));
    return id;
}
fn generateCall(self: *Generator, call: Parser.Call, result_type_id: WORD) Error!WORD {
    return switch (call.callee.*) {
        .builtin => |builtin| switch (builtin.function) {
            .reflect => try self.addWordsAndReturn2(&.{
                opWord(.ext_inst, 7),
                result_type_id,
                self.newID(),
                glsl_std_id,
                @intFromEnum(GlslStdExtOp.reflect),
                try self.generateExpression(call.args[0]),
                try self.generateExpression(call.args[1]),
            }),
            else => @panic("idk how to gen that builtin call"),
        },
        else => @panic("idk how to gen that call"),
    };
}
fn generateValue(self: *Generator, value: Parser.Value) Error!WORD {
    const result = switch (value.type) {
        .bool => blk: {
            const bool_val = util.extract(bool, value.payload.wide);
            const ptr = if (bool_val) &self.true_const else &self.false_const;
            if (ptr.* == 0) ptr.* = self.newID();
            break :blk ptr.*;
        },
        //we can directly bitcast stuff and offset pointers without the |allVectorTypes loop|
        .scalar => try self.addConstant(
            try self.convertTypeID(value.type),
            .{ @truncate(value.payload.wide), @truncate(value.payload.wide >> 32), 0, 0 },
        ),
        // try self.generateNumberConstant(value.payload.wide, try self.convertTypeID(value.type)),
        .vector => |vector| blk: {
            var words: ConstantValue = @splat(0);
            //elem[i] is at offset of component_bytes
            //elem[i] should go at
            const component_type_id = try self.convertTypeID(.{ .scalar = vector.component });
            for (0..@intFromEnum(vector.len)) |i| {
                var dword: u64 = 0;
                switch (vector.component.width) {
                    inline else => |width| dword = (@as(
                        [*]const @Type(.{ .int = .{ .bits = @intFromEnum(width), .signedness = .unsigned } }),
                        @ptrCast(@alignCast(value.payload.ptr)),
                    ))[i],
                }
                words[i] = try self.addConstant(component_type_id, .{ @truncate(dword), @truncate(dword >> 32), 0, 0 });
            }
            break :blk try self.addConstant(try self.convertTypeID(value.type), words);
        },
        // enum
        // matrix
        // array
        else => {
            std.debug.print("Cannot gen value of type: {f}\n", .{value.type});
            @panic("cant gen value :(");
        },
    };
    return result;
}
fn addConstant(self: *Generator, type_id: WORD, value: ConstantValue) Error!WORD {
    for (self.constants.items) |c| if (c.type_id == type_id and constantValueEql(c.value, value)) return c.id;
    const id = self.newID();
    try self.constants.append(self.arena, .{ .id = id, .type_id = type_id, .value = value });
    return id;
}
fn constantValueEql(a: ConstantValue, b: ConstantValue) bool {
    return a[0] == b[0] and a[1] == b[1] and a[2] == b[2] and a[3] == b[3];
}
fn generateBinOp(self: *Generator, bin_op: Parser.BinOp, result_type_id: WORD) Error!WORD {
    const left = try self.generateExpression(bin_op.left.*);
    const right = try self.generateExpression(bin_op.right.*);
    return switch (bin_op.op) {
        .@"^" => try self.addWordsAndReturn2(&.{ opWord(.ext_inst, 7), result_type_id, self.newID(), glsl_std_id, @intFromEnum(GlslStdExtOp.pow), left, right }),
        .@"+" => blk: {
            const @"type" = self.typeFromID(result_type_id);
            const op: Op = if (@"type" == .float or (@"type" == .vector and self.typeFromID(@"type".vector.component_id) == .float)) .fadd else .iadd;
            break :blk try self.addWordsAndReturn2(&.{ opWord(op, 5), result_type_id, self.newID(), left, right });
        },
        .@"-" => blk: {
            //matrices??
            const @"type" = self.typeFromID(result_type_id);
            const op: Op = if (@"type" == .float or (@"type" == .vector and self.typeFromID(@"type".vector.component_id) == .float)) .fsub else .isub;
            break :blk try self.addWordsAndReturn2(&.{ opWord(op, 5), result_type_id, self.newID(), left, right });
        },
        .@"***", .@"**" => blk: {
            //zero value for dotClamped
            // const ptype = self.parser.typeOf(bin_op.left.*) catch unreachable;
            // const zero_value: WORD = try self.generateValue(.{ .type = (ptype).vector.component, .payload = .{ .wide = 0 } });
            const id = self.newID();
            const op: Op = switch (self.typeFromID(result_type_id)) {
                .float => .dot,
                .int => |int| if (int.signed) .sdot else .udot,
                else => unreachable,
            };

            try self.addWords(&.{ opWord(op, 5), result_type_id, id, left, right });
            break :blk id;
        },
        else => @panic("idk how to gen that bin op"),
    };
}
fn generateUOp(self: *Generator, u_op: Parser.UOp, result_type_id: WORD) Error!WORD {
    const target = try self.generateExpression(u_op.target.*);
    return switch (u_op.op) {
        .@";" => try self.addWordsAndReturn2(&.{ opWord(.ext_inst, 6), result_type_id, self.newID(), glsl_std_id, @intFromEnum(GlslStdExtOp.normalize), target }),
        else => @panic("idk how to gen that u op"),
    };
}
fn generateVariableLoad(self: *Generator, name: []const u8) Error!WORD {
    const name_info = self.nameInfo(name);
    if (name_info.load.* != 0) return name_info.load.*;
    const id = self.newID();
    name_info.load.* = id;
    return try self.addWordsAndReturn2(&.{ opWord(.load, 4), name_info.type_id, id, name_info.id }); //load memory operands??
}
fn nameInfo(self: *Generator, name: []const u8) NameInfo {
    if (!self.inGlobalScope()) for (self.current_name_mappings.items) |*nm|
        if (util.strEql(name, nm.name)) return .{ .type_id = nm.type_id, .id = nm.id, .load = &nm.load };

    return for (self.global_vars.items) |*gv| {
        if (util.strEql(name, gv.name)) break .{ .type_id = gv.type_id, .id = gv.id, .load = &gv.load };
    } else @panic("couldnt find variable by name somehow?");
    // } else unreachable;
}
const NameInfo = struct { id: WORD, load: *WORD, type_id: WORD };
fn generateConstantFromScalar(self: *Generator, scalar: anytype, type_id: WORD) Error!WORD {
    const U = @Type(.{ .int = .{ .bits = @sizeOf(@TypeOf(scalar)) * 8, .signedness = .unsigned } });
    const uval: U = @bitCast(scalar);
    return try self.addConstant(type_id, if (@sizeOf(@TypeOf(uval)) > 4)
        .{ .many = .{ @truncate(uval), @truncate(uval >> 32), 0, 0 } }
    else
        .{ .single = @truncate(uval) });
}

fn addWordsAndReturn2(self: *Generator, words: []const WORD) Error!WORD {
    try self.current_buffer.appendSlice(self.arena, words);
    return words[2];
}
fn addWordsAndReturnIth(self: *Generator, words: []const WORD, i: usize) Error!WORD {
    try self.current_buffer.appendSlice(self.arena, words);
    return words[i];
}
fn addWords(self: *Generator, words: []const WORD) Error!void {
    try self.current_buffer.appendSlice(self.arena, words);
}
fn addWord(self: *Generator, word: WORD) Error!void {
    try self.current_buffer.append(self.arena, word);
}

const EntryPoint = struct {
    id: WORD,
    exec_model_info: ExecutionModelInfo,
    interface_ids: []WORD = &.{},
};
const GlobalInitializer = struct {
    index: u32,
    expr: *const Expression,
};
const GlobalVariable = struct {
    name: []const u8,

    type_id: WORD,
    id: WORD,
    storage_class: StorageClass,
    initializer: WORD,

    load: WORD = 0,
};
const NameMapping = struct { name: []const u8, type_id: WORD, id: WORD, load: WORD = 0 };

const Constant = struct {
    id: WORD,
    type_id: WORD,
    value: ConstantValue,
};
const ConstantValue = [4]WORD;

fn convertTypeID(self: *Generator, from: Parser.Type) Error!WORD {
    return try self.typeID(try self.convertType(from));
}
fn convertType(self: *Generator, from: Parser.Type) Error!Type {
    return switch (from) {
        .bool => .bool,
        .void => .void,
        .scalar => |scalar| if (scalar.type == .float)
            .{ .float = scalar.width }
        else
            .{ .int = .{ .width = scalar.width, .signed = scalar.type == .int } },
        .vector => |vector| .{ .vector = .{
            .len = vector.len,
            .component_id = try self.convertTypeID(.{ .scalar = vector.component }),
        } },
        .entrypoint => .{ .function = .{ .rtype_id = try self.typeID(.void) } },
        .function => |function| .{ .function = .{
            .rtype_id = try self.convertTypeID(function.rtype.*),
            .arg_type_ids = try self.convertTypeSliceToIDS(function.arg_types),
        } },

        else => {
            std.debug.print("TYPE: {f}\n", .{from});
            @panic("cant convert type");
        },
    };
}
fn convertTypeSliceToIDS(self: *Generator, types: []const Parser.Type) Error![]WORD {
    const slice = try self.arena.alloc(WORD, types.len);
    for (slice, types) |*to, from| to.* = try self.convertTypeID(from);
    return slice;
}
fn typeFromID(self: *Generator, id: WORD) Type {
    return for (self.type_decls.items) |td| (if (td.id == id) break td.type) else unreachable;
}
fn typeID(self: *Generator, @"type": Type) Error!WORD {
    return for (self.type_decls.items) |td| {
        if (Type.eql(td.type, @"type")) break td.id;
    } else blk: {
        const id = self.newID();
        try self.type_decls.append(self.arena, .{ .type = @"type", .id = id });
        break :blk id;
    };
}
fn newID(self: *Generator) WORD {
    self.current_id += 1;
    return self.current_id - 1;
}

const Type = union(enum) {
    void,
    bool,

    float: BitWidth,
    int: IntType,
    vector: VectorType,
    matrix: MatrixType,

    function: FunctionType,
    pointer: PointerType,

    pub fn eql(a: Type, b: Type) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .void, .bool => true,
            .float => |float| float == b.float,
            .int => |int| int.width == b.int.width and int.signed == b.int.signed,
            .vector => |vector| vector.len == b.vector.len and vector.component_id == b.vector.component_id,
            .function => |function| function.rtype_id == b.function.rtype_id and
                if (function.arg_type_ids.len == b.function.arg_type_ids.len)
                    (for (function.arg_type_ids, b.function.arg_type_ids) |a_arg, b_arg| (if (a_arg != b_arg) break false) else true)
                else
                    false,
            .pointer => |pointer| pointer.pointed_id == b.pointer.pointed_id and pointer.storage_class == b.pointer.storage_class,
            else => @panic("idk how to compare that type"),
        };
    }
    pub fn valueWordConsumption(self: Type) u32 {
        return switch (self) {
            .float => |float| @as(WORD, if (float == .long) 2 else 1),
            .int => |int| @as(WORD, if (int.width == .long) 2 else 1),
            .vector => |vector| @intFromEnum(vector.len),
            .matrix => |matrix| @intFromEnum(matrix.column_count),
            else => 1,
        };
    }
};
fn opWord(op: Op, count: u16) u32 {
    return (@as(u32, count) << 16) | @intFromEnum(op);
}
const Op = enum(WORD) {
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

    load = 61,
    store = 62,

    iadd = 128,
    fadd = 129,
    isub = 130,
    fsub = 131,

    dot = 148,
    sdot = 4450,
    udot = 4451,

    ext_inst = 12,
};

const GlslStdExtOp = enum(WORD) {
    normalize = 69,
    reflect = 71,
    pow = 26,
};
const Decoration = enum(WORD) {
    location = 30,
};
const StorageClass = enum(WORD) {
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
const Capabilitiy = enum(WORD) {
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

const FunctionControl = enum(WORD) {
    none = 0,
    @"inline" = 1,
    dontinline = 2,
    pure = 3,
    @"const" = 4,
};
const PointerType = struct { pointed_id: WORD, storage_class: StorageClass };
const FunctionType = struct { rtype_id: WORD, arg_type_ids: []WORD = &.{} };

const MatrixType = struct { column_count: VectorLen, column_type_id: WORD };
const VectorType = struct { len: VectorLen, component_id: WORD };
const IntType = struct { width: BitWidth, signed: bool };
const BitWidth = Parser.tp.BitWidth;
const VectorLen = Parser.tp.VectorLen;
const TypeDeclaration = struct { type: Type, id: WORD };

const ExecutionModelInfo = Parser.ExecutionModelInfo;
const Expression = Parser.Expression;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const List = std.ArrayList;
