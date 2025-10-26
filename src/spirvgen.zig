//module structure:
// boilerplate
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
const bi = @import("builtin.zig");
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
const glsl_ext_literal_words = blk: {
    const str = "GLSL.std.450";
    const num_words = (str.len + 4) / 4;
    var words: [num_words]WORD = @splat(0);
    for (str, 0..) |c, i| words[i / 4] |= @as(WORD, c) << ((i & 3) * 8);

    break :blk words;
};
const magic_number: WORD = 0x07230203;
const generator_number: WORD = 0;
const IOEntry = struct { id: WORD, type_id: WORD, properties: packed struct {
    global: bool,
    qual: u2,
    index: u29,
} };
const IOType = enum(u2) { in, out, uniform, push };

parser: *Parser,
current_id: WORD = 2, //0 - reserved,1 - glsl ext for now
arena: Allocator,

capabilities: Capabilities = .{ .shader = true }, //flag struct
io_order: List(IOEntry) = .empty,
decorations: List(u32) = .empty,
entry_points: List(EntryPoint) = .empty,

type_decls: List(TypeDeclaration) = .empty,
true_const: WORD = 0,
false_const: WORD = 0,
constants: List(Constant) = .empty,
global_vars: List(GlobalVariable) = .empty,

deferred_global_initializers: List(GlobalInitializer) = .empty,
builtin_decoration_ids: BuiltinDecorationIDS = .{},

main_buffer: List(WORD) = .empty,

current_interface_ids: *List(WORD) = @ptrCast(@alignCast(@constCast(&0))),
current_buffer: *List(WORD) = @ptrCast(@alignCast(@constCast(&0))),
current_variable_buffer: *List(WORD) = @ptrCast(@alignCast(@constCast(&0))),
//does this break on nested scopes??? it shoulndt if tied to function
current_name_mappings: *List(NameMapping) = @ptrCast(@alignCast(@constCast(&0))),

pub fn generate(parser: *Parser) Error![]WORD {
    var self: Generator = .{
        .parser = parser,
        .arena = parser.arena.allocator(),
    };
    var result: std.array_list.Managed(WORD) = .init(self.parser.allocator);
    self.current_buffer = &self.main_buffer;
    for (self.parser.global_scope.body.items) |statement| try self.generateStatment(statement);
    self.current_buffer = &self.main_buffer;

    try result.appendSlice(&.{ //spirv boilerplate
        magic_number,
        versionWord(1, 6),
        generator_number,
        self.current_id,
        0, //reserved
    });
    inline for (@typeInfo(Capability).@"enum".fields) |ef| // capabilities
        if (@field(self.capabilities, ef.name))
            try result.appendSlice(&.{ opWord(.capability, 2), ef.value });

    try result.appendSlice(&.{ opWord(.ext_inst_import, 2 + glsl_ext_literal_words.len), glsl_std_id });
    try result.appendSlice(&glsl_ext_literal_words); // exstension instruction imports

    //logical addressing model(0)
    //memory model is Vulkan(3)/GLSL450(1)???
    try result.appendSlice(&.{ opWord(.memory_model, 3), 0, 1 }); // memory model

    for (self.entry_points.items) |ep| { // op entry points
        // std.de
        const execution_model: WORD = switch (ep.exec_model_info) {
            .vertex => 0,
            .fragment => 4,
            .compute => 5,
        };
        try result.appendSlice(&.{ opWord(
            .entry_point,
            @truncate(3 + ((ep.name.len + 4) / 4) + ep.interface_ids.len),
        ), execution_model, ep.id });

        for (0..(ep.name.len + 4) / 4) |wi| {
            var word: WORD = 0;
            for (0..4) |i| {
                const ci = wi * 4 + i;
                if (ci < ep.name.len) word |= @as(WORD, ep.name[ci]) << @truncate(i * 8);
            }
            try result.append(word);
        }
        try result.appendSlice(ep.interface_ids);
    }

    for (self.entry_points.items) |ep| { // execution modes
        switch (ep.exec_model_info) {
            .fragment => try result.appendSlice(&.{
                opWord(.execution_mode, 3),
                ep.id,
                @intFromEnum(ExecutionMode.origin_upper_left),
            }),

            else => {},
            // .vertex => 0,
            // .fragment => 4,
            // .compute => 5,
        }
    }

    // decorations
    var global_offsets: [4]WORD = @splat(0);
    var local_offsets: [4]WORD = @splat(0);
    for (self.io_order.items) |io| {
        // const @"type" = self.typeFromID(io.type_id);
        const ptr: *WORD = &(if (io.properties.global) &global_offsets else &local_offsets)[io.properties.qual];
        //different kinds of decorates
        try result.appendSlice(&.{
            opWord(.decorate, 4),
            io.id,
            @intFromEnum(if (io.properties.qual < 2) Decoration.location else Decoration.binding),
            ptr.* + if (!io.properties.global) global_offsets[io.properties.qual] else 0,
        });
        ptr.* += 1;
    }
    //locations, bindings, deskriptor_sets
    for (self.type_decls.items) |t| try result.appendSlice(switch (t.type) { //types
        .bool => &.{ opWord(.type_bool, 2), t.id },
        .void => &.{ opWord(.type_void, 2), t.id },
        .int => |int| &.{ opWord(.type_int, 4), t.id, @intFromEnum(int.width), @intFromBool(int.signed) },
        .float => |width| &.{ opWord(.type_float, 3), t.id, @intFromEnum(width) },
        .vector => |vector| &.{ opWord(.type_vector, 4), t.id, vector.component_id, @intFromEnum(vector.len) },
        .pointer => |pointer| &.{ opWord(.type_pointer, 4), t.id, @intFromEnum(pointer.storage_class), pointer.pointed_id },
        .function => |function| blk: {
            try result.appendSlice(&.{
                opWord(.type_function, @truncate(3 + function.arg_type_ids.len)),
                t.id,
                function.rtype_id,
            });
            break :blk function.arg_type_ids;
        },

        else => unreachable,
    });
    for (self.constants.items) |c| { //constants
        const @"type" = self.typeFromID(c.type_id);
        const word_count = @"type".valueWordConsumption();
        try result.appendSlice(&.{ opWord(
            if (@"type" == .int or @"type" == .float) .constant else .constant_composite,
            @truncate(3 + word_count),
        ), c.type_id, c.id });
        try result.appendSlice(if (word_count <= 4) c.value.quad[0..word_count] else c.value.ptr[0..word_count]);
    }
    if (self.false_const != 0) try result.appendSlice(&.{ opWord(.constant_false, 3), try self.typeID(.bool), self.false_const });
    if (self.true_const != 0) try result.appendSlice(&.{ opWord(.constant_true, 3), try self.typeID(.bool), self.true_const });

    for (self.global_vars.items) |gv| {
        try result.appendSlice(&.{
            opWord(.variable, if (gv.initializer == 0) 4 else 5),
            try self.typeID(.{ .pointer = .{ .pointed_id = gv.type_id, .storage_class = gv.storage_class } }),
            gv.id,
            @intFromEnum(gv.storage_class),
        });
        if (gv.initializer != 0) try result.append(gv.initializer);
    }

    // std.debug.print("IOORDER: {any}\n", .{self.io_order.items});
    try result.appendSlice(self.main_buffer.items);
    return try result.toOwnedSlice();
}
fn versionWord(major: u8, minor: u8) WORD {
    return @as(WORD, minor) << 8 | @as(WORD, major) << 16;
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
        return try self.generateEntryPoint(var_decl.name, @as(*Parser.EntryPoint, @ptrCast(@alignCast(@constCast(var_decl.initializer.value.payload.ptr)))));
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
    const pointer_type_id = try self.typeID(.{ .pointer = .{
        .pointed_id = type_id,
        .storage_class = storage_class,
    } });
    const var_id = self.newID();
    var initializer: WORD = 0;
    var load: WORD = 0;
    io: { //check if we should add io
        try self.io_order.append(self.arena, .{
            .id = var_id,
            .type_id = type_id,
            .properties = .{
                .global = self.inGlobalScope(),
                .qual = @intFromEnum(@as(IOType, switch (var_decl.qualifier) {
                    .in => .in,
                    .out => .out,
                    .push => .push,
                    .uniform => .uniform,
                    else => break :io,
                })),
                .index = blk: {
                    if (!self.inGlobalScope())
                        for (self.entry_points.items[self.entry_points.items.len -| 1].val_ptr.io, 0..) |li, i|
                            (if (util.strEql(var_decl.name, li)) break :blk @truncate(i));
                    break :blk for (self.parser.global_io.items, 0..) |gi, i| (if (util.strEql(var_decl.name, gi)) break @truncate(i)) else unreachable;
                },
            },
        });
    }

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
        },
    };
    // if (initializer != 0) load = initializer;

    if (storage_class == .function) {
        try self.current_variable_buffer.appendSlice(self.arena, &.{ opWord(.variable, if (initializer == 0) 4 else 5), pointer_type_id, var_id, @intFromEnum(StorageClass.function) });
        if (initializer != 0) try self.addWord(initializer);
        try self.current_name_mappings.append(self.arena, .{ .type_id = type_id, .id = var_id, .name = var_decl.name, .load = load });
    } else {
        if (!self.inGlobalScope()) try self.addInterfaceID(var_id);
        try self.global_vars.append(self.arena, .{
            .name = var_decl.name,
            .type_id = type_id,
            .id = var_id,
            .storage_class = storage_class,

            .initializer = initializer,
            .load = 0,
        });
    }

    if (!self.inGlobalScope() and initializer == 0 and load != 0)
        try self.addWords(&.{ opWord(.store, 3), var_id, load });
}

fn generateEntryPoint(self: *Generator, name: []const u8, entry_point: *Parser.EntryPoint) Error!void {
    var buffer: List(WORD) = .empty;
    var variable_buffer: List(WORD) = .empty;
    var interface_ids: List(WORD) = .empty;
    var name_mappings: List(NameMapping) = .empty;

    self.parser.current_scope = &entry_point.scope;
    defer self.parser.current_scope = entry_point.scope.parent;

    self.current_buffer = &buffer;
    self.current_variable_buffer = &variable_buffer;
    self.current_interface_ids = &interface_ids;
    self.current_name_mappings = &name_mappings;
    defer self.current_buffer = &self.main_buffer;

    const entry_point_id = self.newID();

    const index = self.entry_points.items.len;
    try self.entry_points.append(self.arena, .{
        .val_ptr = entry_point,
        .name = name,

        .id = entry_point_id,
        .exec_model_info = entry_point.exec_model_info,
    });

    for (entry_point.body.items) |statement| try self.generateStatment(statement);

    self.entry_points.items[index].interface_ids = try interface_ids.toOwnedSlice(self.arena);

    try self.addWords(&.{ opWord(.@"return", 1), opWord(.function_end, 1) });

    try self.main_buffer.appendSlice(self.arena, &.{
        opWord(.function, 5),
        try self.typeID(.void),
        entry_point_id,
        @intFromEnum(FunctionControl.none),
        try self.typeID(.{ .function = .{ .rtype_id = try self.typeID(.void) } }),
    });
    try self.main_buffer.appendSlice(self.arena, &.{ opWord(.label, 2), self.newID() });
    try self.main_buffer.appendSlice(self.arena, variable_buffer.items);
    try self.main_buffer.appendSlice(self.arena, buffer.items);
    buffer.deinit(self.arena);
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
        .indexing => |indexing| try self.generateIndexing(indexing),
        .builtin => |builtin| if (builtin == .variable) try self.generateBuiltinVariable(builtin.variable) else @panic("gen builtin function"),
        else => {
            std.debug.print("Cannot gen expr: {f}\n", .{expr});
            @panic("idk how to gen that expr");
        },
    };
    // std.debug.print("id: {d}, Expr: {f}\n", .{ result, expr });
    return result;
}
fn generateBuiltinVariable(self: *Generator, bv: bi.BuiltinVariable) Error!WORD {
    return switch (bv) {
        .vertex_id => blk: {
            if (self.builtin_decoration_ids.vertex_id != 0)
                break :blk self.builtin_decoration_ids.vertex_id;
            const var_id = self.newID();
            const type_id = try self.typeID(.{ .int = .{ .width = .word, .signed = true } });
            try self.global_vars.append(self.arena, .{
                .type_id = try self.typeID(.{ .pointer = .{
                    .pointed_id = type_id,
                    .storage_class = .input,
                } }),
                .id = var_id,
                .storage_class = .input,
            });
            try self.decorations.appendSlice(self.arena, &.{
                opWord(.decorate, 4),
                var_id,
                @intFromEnum(Decoration.builtin),
                @intFromEnum(BuiltinDecoration.vertex_id),
            });
            break :blk try self.generateVariableLoadID(
                type_id,
                self.newID(),
                var_id,
            );
        },
        else => @panic("unknown builtin variable"),
    };
}
fn generateIndexing(self: *Generator, indexing: Parser.Indexing) Error!WORD {
    const target = try self.generateExpression(indexing.target.*);
    const index = try self.generateExpression(indexing.index.*);
    std.debug.print("t: {d}, i:{d}\n", .{ target, index });
    @panic("gen indexing");
}

fn generateConstructor(self: *Generator, components: []Expression, result_type_id: WORD) Error!WORD {
    const id = self.newID();
    try self.addWords(&.{ opWord(.composite_construct, @truncate(3 + components.len)), result_type_id, id });
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
            .{ .quad = .{ @truncate(value.payload.wide), @truncate(value.payload.wide >> 32), 0, 0 } },
        ),
        // try self.generateNumberConstant(value.payload.wide, try self.convertTypeID(value.type)),
        .vector => |vector| blk: {
            var words: [4]WORD = @splat(0);
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
                words[i] = try self.addConstant(component_type_id, .{ .quad = .{ @truncate(dword), @truncate(dword >> 32), 0, 0 } });
            }
            break :blk try self.addConstant(try self.convertTypeID(value.type), .{ .quad = words });
        },
        .array => |array| blk: {
            var word_array: [4]WORD = @splat(0);
            const slice = if (array.len <= 4) &word_array else try self.arena.alloc(WORD, array.len);
            for (slice, @as([*]const Parser.ValuePayload, @ptrCast(@alignCast(value.payload.ptr)))[0..array.len]) |*w, p| //
                w.* = try self.generateValue(.{ .type = array.component.*, .payload = p });
            break :blk try self.addConstant(try self.convertTypeID(value.type), if (array.len <= 4)
                .{ .quad = word_array }
            else
                .{ .ptr = slice.ptr });
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
    for (self.constants.items) |c| if (c.type_id == type_id and constantValueEql(c.value, value, self.typeFromID(type_id).valueWordConsumption())) return c.id;
    const id = self.newID();
    try self.constants.append(self.arena, .{ .id = id, .type_id = type_id, .value = value });
    return id;
}
fn constantValueEql(a: ConstantValue, b: ConstantValue, word_count: WORD) bool {
    return if (word_count <= 4)
        std.mem.eql(WORD, a.quad[0..word_count], b.quad[0..word_count])
    else
        std.mem.eql(WORD, a.ptr[0..word_count], b.ptr[0..word_count]);
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
    const name_info: NameInfo = blk: {
        if (!self.inGlobalScope()) for (self.current_name_mappings.items) |*nm|
            if (util.strEql(name, nm.name)) break :blk .{ .type_id = nm.type_id, .id = nm.id, .load = &nm.load };

        break :blk for (self.global_vars.items) |*gv| {
            if (util.strEql(name, gv.name)) {
                try self.addInterfaceID(gv.id);
                break .{ .type_id = gv.type_id, .id = gv.id, .load = &gv.load };
            }
        } else @panic("couldnt find variable by name somehow?");
    };
    if (name_info.load.* != 0) return name_info.load.*;
    const id = self.newID();
    name_info.load.* = id;
    return try self.generateVariableLoadID(name_info.type_id, id, name_info.id);
}
fn generateVariableLoadID(self: *Generator, type_id: WORD, id: WORD, var_id: WORD) Error!WORD {
    return try self.addWordsAndReturn2(&.{ opWord(.load, 4), type_id, id, var_id });
}
fn addInterfaceID(self: *Generator, id: WORD) Error!void {
    if (for (self.current_interface_ids.items) |ii| (if (ii == id) break false) else true)
        try self.current_interface_ids.append(self.arena, id);
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
    val_ptr: *Parser.EntryPoint,
    name: []const u8,

    id: WORD,
    exec_model_info: ExecutionModelInfo,
    interface_ids: []WORD = &.{},
};
const GlobalInitializer = struct {
    index: u32,
    expr: *const Expression,
};
const GlobalVariable = struct {
    name: []const u8 = "\x00",

    type_id: WORD,
    id: WORD,
    storage_class: StorageClass,
    initializer: WORD = 0,

    load: WORD = 0,
};
const NameMapping = struct { name: []const u8, type_id: WORD, id: WORD, load: WORD = 0 };

const Constant = struct {
    id: WORD,
    type_id: WORD,
    value: ConstantValue,
};
const ConstantValue = union {
    quad: [4]WORD,
    ptr: [*]WORD,
};

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
        .array => |array| .{ .array = .{ .len = array.len, .component_id = try self.convertTypeID(array.component.*) } },

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

    array: ArrayType,

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
            .array => |array| array.component_id == b.array.component_id and array.len == b.array.len,
            else => @panic("idk how to compare that type"),
        };
    }
    pub fn valueWordConsumption(self: Type) WORD {
        return switch (self) {
            .float => |float| @as(WORD, if (float == .long) 2 else 1),
            .int => |int| @as(WORD, if (int.width == .long) 2 else 1),
            .vector => |vector| @intFromEnum(vector.len),
            .matrix => |matrix| @intFromEnum(matrix.column_count),
            .array => |array| array.len,
            else => 1,
        };
    }
};
const PointerType = struct { pointed_id: WORD, storage_class: StorageClass };
const FunctionType = struct { rtype_id: WORD, arg_type_ids: []WORD = &.{} };

const MatrixType = struct { column_count: VectorLen, column_type_id: WORD };
const VectorType = struct { len: VectorLen, component_id: WORD };
const IntType = struct { width: BitWidth, signed: bool };
const BitWidth = Parser.tp.BitWidth;
const VectorLen = Parser.tp.VectorLen;
const ArrayType = struct { len: WORD, component_id: WORD };
const TypeDeclaration = struct { type: Type, id: WORD };
fn opWord(op: Op, count: u16) WORD {
    return (@as(WORD, count) << 16) | @intFromEnum(op);
}
const Op = enum(WORD) {
    constant_true = 41,
    constant_false = 42,
    constant = 43,
    constant_composite = 44,

    composite_construct = 80,

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

    ext_inst_import = 11,
    ext_inst = 12,
};

const GlslStdExtOp = enum(WORD) {
    normalize = 69,
    reflect = 71,
    pow = 26,
};
const BuiltinDecorationIDS = util.StructFromEnum(BuiltinDecoration, WORD, 0);
const BuiltinDecoration = enum(WORD) {
    position = 0,
    point_size = 1,
    clip_distance = 3,
    cull_distance = 4,

    vertex_id = 5,
    instance_id = 6,
    primitive_id = 7,
    invocation_id = 8,
};
const Decoration = enum(WORD) {
    location = 30,
    binding = 33,
    descriptor_set = 34,

    uniform = 26,
    component = 31,
    index = 32,

    builtin = 11,
    block = 2,
    glsl_shared = 8,
    glsl_packed = 9,

    offset = 35,
    array_stride = 6,
    matrix_stride = 7,

    no_perspective = 13,
    flat = 14,
    non_writable = 24,
    non_readable = 25,

    restrict = 19,
    aliased = 20,
    constant = 22, //not mutated variale

    relaxed_precision = 0,
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
const Capabilities = util.FlagStructFromEnum(Capability, false);
const Capability = enum(WORD) {
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

const ExecutionMode = enum(WORD) {
    origin_upper_left = 7,
    origin_lower_left = 8,
};
const FunctionControl = enum(WORD) {
    none = 0,
    @"inline" = 1,
    dontinline = 2,
    pure = 3,
    @"const" = 4,
};

const ExecutionModelInfo = Parser.ExecutionModelInfo;
const Expression = Parser.Expression;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const List = std.ArrayList;
