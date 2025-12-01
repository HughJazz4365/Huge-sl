const std = @import("std");
const util = @import("util.zig");
const bi = @import("builtin.zig");
const tp = @import("type.zig");
const zigbuiltin = @import("builtin");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Generator = @This();
const hgsl = @import("root.zig");
const Settings = hgsl.Settings;

const WORD = u32;

const Error = error{
    OutOfMemory,
    InvalidSpirvType,
    InvalidIOType,
    GenError,
    PushConstantBlockTooBig,
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
const IOEntry = struct {
    id: WORD,
    type_id: WORD,
    io_type: IOType = undefined,

    descriptor_set: WORD = 0,
};
const IOType = enum(u2) {
    in = 0,
    out = 1,
    uniform = 2,
    push = 3,
};
decorated_global_io_count: usize = 0,
global_io: []IOEntry,
global_push_constants: List(PushConstant) = .empty,

parser: *Parser,
alignment: Alignment = .scalar,
current_id: WORD = 2, //0 - reserved,1 - glsl ext for now
arena: Allocator,

capabilities: Capabilities = .{ .shader = true },
decorations: List(u32) = .empty,
entry_points: List(EntryPoint) = .empty,

type_decls: List(TypeDeclaration) = .empty,
array_length_constants: List(struct { id: WORD, val: WORD, generated: bool = false }) = .empty,

true_const: WORD = 0,
false_const: WORD = 0,
constants: List(Constant) = .empty,
global_vars: List(GlobalVariable) = .empty,

deferred_global_initializers: List(GlobalInitializer) = .empty,
builtin_pointer_infos: BuiltinIDInfos = .{},

main_buffer: List(WORD) = .empty,

current_interface_ids: *List(WORD) = @ptrCast(@alignCast(@constCast(&0))),
current_buffer: *List(WORD) = @ptrCast(@alignCast(@constCast(&0))),
current_variable_buffer: *List(WORD) = @ptrCast(@alignCast(@constCast(&0))),
//does this break on nested scopes??? it shoulndt if tied to function
current_name_mappings: *List(NameMapping) = @ptrCast(@alignCast(@constCast(&0))),
pub fn printInstructions(slice: []WORD) void {
    var copy = slice;
    while (copy.len > 0) {
        const op_word = copy[0];
        const code: u16 = @truncate(op_word);
        const count: usize = (op_word >> 16);
        if (count == 0) return;
        const tag_name = inline for (@typeInfo(Op).@"enum".fields) |ef| (if (ef.value == code) break ef.name) else "UNKNOWN";
        std.debug.print("[{s}] {any}\n", .{ tag_name, copy[0..count] });
        copy = copy[count..];
    }
}

pub fn generate(parser: *Parser, result_allocator: Allocator) Error!hgsl.Result {
    var self: Generator = .{
        .parser = parser,
        .arena = parser.arena.allocator(),
        .global_io = try parser.arena.allocator().alloc(IOEntry, parser.global_scope.global_io.items.len),
    };

    var result: std.array_list.Managed(WORD) = .init(result_allocator);
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

    for (self.entry_points.items, 0..) |ep, i| { // op entry points
        if (ep.push_constant_struct_type_id != 0) {
            const slice = try self.arena.alloc(WORD, ep.push_constants.items.len);
            for (ep.push_constants.items, slice) |*pc, *c| c.* = pc.type_id;

            const @"type": *Type = for (self.type_decls.items) |*td|
                (if (ep.push_constant_struct_type_id == td.id) break &td.type)
            else
                unreachable;
            @"type".* = .{ .@"struct" = slice };
            if (self.sizeOf(self.typeFromID(ep.push_constant_struct_type_id)) > self.parser.settings.max_push_constant_buffer_size) return Error.PushConstantBlockTooBig;

            try self.decorateStructLayout(
                ep.push_constant_struct_type_id,
                self.typeFromID(ep.push_constant_struct_type_id).@"struct",
                false,
                .scalar,
            );

            const ptr_type_id = try self.typeID(.{ .pointer = .{
                .pointed_id = ep.push_constant_struct_type_id,
                .storage_class = .push_constant,
            } });
            try self.global_vars.append(self.arena, .{
                .type_id = ptr_type_id,
                .id = ep.push_constant_struct_id,
                .storage_class = .push_constant,
                .entry_point_index = @truncate(i),
            });
        }

        const execution_model: WORD = switch (ep.stage_info) {
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
            for (0..4) |j| {
                const ci = wi * 4 + j;
                if (ci < ep.name.len) word |= @as(WORD, ep.name[ci]) << @truncate(j * 8);
            }
            try result.append(word);
        }
        try result.appendSlice(ep.interface_ids);
    }

    for (self.entry_points.items) |ep| { // execution modes
        switch (ep.stage_info) {
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
    //debug
    if (self.parser.settings.optimize == .none) for (self.global_vars.items) |gv| {
        if (gv.name.len == 0) continue;
        try result.appendSlice(&.{ opWord(.name, @truncate(2 + (gv.name.len + 4) / 4)), gv.id });
        for (0..(gv.name.len + 4) / 4) |wi| {
            var word: WORD = 0;
            for (0..4) |i| {
                const ci = wi * 4 + i;
                if (ci < gv.name.len) word |= @as(WORD, gv.name[ci]) << @truncate(i * 8);
            }
            try result.append(word);
        }
    };
    //add all the decoration for buffer types before emitting decoration instructions
    for (self.type_decls.items) |td| {
        if (td.type != .buffer) continue;
        const id, const fields = .{ td.id, self.typeFromID(td.type.buffer.struct_id).@"struct" };
        try self.decorations.appendSlice(self.arena, &.{ opWord(.decorate, 3), id, @intFromEnum(Decoration.block) });
        try self.decorateStructLayout(id, fields, false, .base);
    }

    //decorations
    var global_offsets: [3]WORD = @splat(0);
    for (self.entry_points.items) |ep| {
        var local_offsets: [3]WORD = @splat(0);
        const gc = ep.val_ptr.global_io_count;
        for (self.global_io[@min(gc, self.decorated_global_io_count)..gc]) |g| {
            if (g.io_type == .push) continue;
            try result.appendSlice(&.{
                opWord(.decorate, 4),
                g.id,
                @intFromEnum(if (g.io_type == .uniform) Decoration.binding else Decoration.location),
                global_offsets[@intFromEnum(g.io_type)],
            });
            global_offsets[@intFromEnum(g.io_type)] += 1;
            if (g.io_type == .uniform) try self.decorations.appendSlice(self.arena, &.{
                opWord(.decorate, 4),
                g.id,
                @intFromEnum(Decoration.descriptor_set),
                g.descriptor_set,
            });
        }
        self.decorated_global_io_count = ep.val_ptr.global_io_count;

        for (ep.local_io) |l| {
            if (l.io_type == .push) continue;
            try result.appendSlice(&.{
                opWord(.decorate, 4),
                l.id,
                @intFromEnum(if (l.io_type == .uniform) Decoration.binding else Decoration.location),
                local_offsets[@intFromEnum(l.io_type)] + global_offsets[@intFromEnum(l.io_type)],
            });
            local_offsets[@intFromEnum(l.io_type)] += 1;
            if (l.io_type == .uniform) try self.decorations.appendSlice(self.arena, &.{
                opWord(.decorate, 4),
                l.id,
                @intFromEnum(Decoration.descriptor_set),
                l.descriptor_set,
            });
        }
    }
    try result.appendSlice(self.decorations.items);

    //locations, bindings, descriptor_sets
    for (self.type_decls.items) |t| try result.appendSlice(switch (t.type) { //types
        .bool => &.{ opWord(.type_bool, 2), t.id },
        .void => &.{ opWord(.type_void, 2), t.id },
        .int => |int| &.{ opWord(.type_int, 4), t.id, @intFromEnum(int.width), @intFromBool(int.signed) },
        .float => |width| &.{ opWord(.type_float, 3), t.id, @intFromEnum(width) },
        .vector => |vector| &.{ opWord(.type_vector, 4), t.id, vector.component_id, @intFromEnum(vector.len) },
        .array => |array| &.{
            opWord(.type_array, 4),
            t.id,
            array.component_id,
            for (self.array_length_constants.items) |l| (if (l.val == array.len) {
                if (!l.generated) try result.appendSlice(&.{
                    opWord(.constant, 4),
                    try self.typeID(.{ .int = .{ .width = .word, .signed = false } }),
                    l.id,
                    array.len,
                });
                break l.id;
            }) else unreachable,
        },
        .pointer => |pointer| &.{ opWord(.type_pointer, 4), t.id, @intFromEnum(pointer.storage_class), pointer.pointed_id },
        .function => |function| blk: {
            try result.appendSlice(&.{
                opWord(.type_function, @truncate(3 + function.arg_type_ids.len)),
                t.id,
                function.rtype_id,
            });
            break :blk function.arg_type_ids;
        },
        .@"struct" => |@"struct"| blk: {
            try result.appendSlice(&.{ opWord(.type_struct, @truncate(2 + @"struct".len)), t.id });
            break :blk @"struct";
        },
        .buffer => |buffer| blk: {
            const s = self.typeFromID(buffer.struct_id).@"struct";
            try result.appendSlice(&.{ opWord(.type_struct, @truncate(2 + s.len)), t.id });
            break :blk s;
        },
        .matrix => |matrix| &.{ opWord(.type_matrix, 4), t.id, matrix.column_type_id, @intFromEnum(matrix.column_count) },
        .image => |image| &.{
            opWord(.type_image, 9),
            t.id,
            image.sampled_type_id,
            @intFromEnum(image.dim),
            image.depth,
            @intFromBool(image.arrayed),
            @intFromBool(image.multi_sampled),
            @intFromBool(image.sampled),
            0,
        },
        .sampled_image => |sampled_image| &.{ opWord(.type_sampled_image, 3), t.id, sampled_image.image_id },

        // else => unreachable,
    });
    for (self.constants.items) |c| { //constants
        const @"type" = self.typeFromID(c.type_id);
        const word_count = self.typeWordConsumption(@"type");
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
            gv.type_id,
            gv.id,
            @intFromEnum(gv.storage_class),
        });
        if (gv.initializer != 0) try result.append(gv.initializer);
    }

    try result.appendSlice(self.main_buffer.items);
    // printInstructions(result.items[5..]);

    const mappings = try result_allocator.alloc(hgsl.EntryPointInfo, self.entry_points.items.len);
    for (mappings, self.entry_points.items) |*m, ep| {
        const push_constant_mappings = try result_allocator.alloc(
            hgsl.PushConstantMapping,
            ep.push_constants.items.len,
        );
        var offset: WORD = 0;
        for (push_constant_mappings, ep.push_constants.items) |*pcm, pc| {
            const pc_type = self.typeFromID(pc.type_id);
            const size = self.sizeOf(pc_type);
            offset = util.wrut(offset, self.alignOf(pc_type, .scalar)); //???
            pcm.* = .{
                .name = try result_allocator.dupe(u8, pc.name),
                .offset = offset,
                .size = size,
            };
            offset = util.wrut(offset + size, self.alignOf(pc_type, .scalar));
        }

        var input_count: u32 = 0;
        var output_count: u32 = 0;
        var uniform_count: usize = 0;
        for (&[2][]IOEntry{ self.global_io[0..ep.val_ptr.global_io_count], ep.local_io }) |slice|
            for (slice) |io| switch (io.io_type) {
                .in => input_count += 1,
                .out => output_count += 1,
                .uniform => uniform_count += 1,
                else => {},
            };

        const opaque_uniform_mappings = try result_allocator.alloc(
            hgsl.OpaqueUniformMapping,
            uniform_count,
        );
        uniform_count = 0;
        //[input][output] construct result
        const io_mappings = try result_allocator.alloc(hgsl.IOMapping, input_count + output_count);
        var input_index: u32 = 0;
        var output_index: u32 = 0;
        for (&[2][]IOEntry{ self.global_io[0..ep.val_ptr.global_io_count], ep.local_io }) |slice|
            for (slice) |io| switch (io.io_type) {
                .uniform => {
                    opaque_uniform_mappings[uniform_count] = .{
                        .name = for (self.global_vars.items) |gv| (if (io.id == gv.id) break gv.name) else unreachable,
                        .type = switch (self.typeFromID(io.type_id)) {
                            .buffer => |buffer| if (buffer.storage_class == .storage_buffer)
                                .ssbo
                            else
                                .ubo,
                            .sampled_image => .sampled_texture,
                            .image => .texture,
                            else => unreachable,
                        },
                        .binding = @intCast(uniform_count),
                        .descriptor_set = io.descriptor_set,
                    };
                    uniform_count += 1;
                },
                .in, .out => {
                    const name = try result_allocator.dupe(u8, for (self.global_vars.items) |gv| {
                        if (io.id == gv.id) break gv.name;
                    } else unreachable);
                    const @"type" = self.typeFromID(io.type_id);
                    const size = self.sizeOf(@"type");

                    const index_ptr = if (io.io_type == .in) &input_index else &output_index;
                    io_mappings[if (io.io_type == .in) input_index else input_count + output_index] = .{
                        .location = index_ptr.*,
                        .name = name,
                        .size = size,
                        .type = self.toIOType(@"type"),
                    };
                    index_ptr.* += 1;
                },
                .push => {},
            };

        const name_copy = try result_allocator.alloc(u8, ep.name.len + 1);
        name_copy[ep.name.len] = 0;
        @memcpy(name_copy[0..ep.name.len], ep.name);
        m.* = .{
            .name = name_copy[0..ep.name.len :0],
            .stage_info = ep.stage_info,

            .push_constant_mappings = push_constant_mappings,
            .opaque_uniform_mappings = opaque_uniform_mappings,

            .io_mappings_ptr = io_mappings.ptr,
            .input_count = input_count,
            .output_count = output_count,
        };
    }

    return .{
        .bytes = @ptrCast(@alignCast(try result.toOwnedSlice())),
        .entry_point_infos = mappings,
    };
}
fn versionWord(major: u8, minor: u8) WORD {
    return @as(WORD, minor) << 8 | @as(WORD, major) << 16;
}
fn inGlobalScope(self: *Generator) bool {
    return @intFromPtr(self.current_buffer) == @intFromPtr(&self.main_buffer);
}
fn decorateStructLayout(self: *Generator, type_id: WORD, fields: []const WORD, reorder: bool, alignment: Alignment) Error!void {
    if (reorder) {
        unreachable;
    }
    var offset: WORD = 0;
    for (fields, 0..) |f, i| {
        const field_type = self.typeFromID(f);
        const field_size = self.sizeOf(field_type);
        switch (field_type) {
            .matrix => |matrix| {
                const column_vector_type = self.typeFromID(matrix.column_type_id);
                try self.decorations.appendSlice(self.arena, &.{
                    opWord(.member_decorate, 4),
                    type_id,
                    @truncate(i),
                    @intFromEnum(Decoration.col_major),
                });
                try self.decorations.appendSlice(self.arena, &.{
                    opWord(.member_decorate, 5),
                    type_id,
                    @truncate(i),
                    @intFromEnum(Decoration.matrix_stride),
                    util.wrut(self.sizeOf(column_vector_type), self.alignOf(column_vector_type, alignment)),
                });
            },
            else => {},
        }
        offset = util.wrut(offset, self.alignOf(field_type, .scalar)); //???
        try self.decorations.appendSlice(self.arena, &.{
            opWord(.member_decorate, 5),
            type_id,
            @truncate(i),
            @intFromEnum(Decoration.offset),
            offset,
        });
        offset = util.wrut(offset + field_size, self.alignOf(field_type, alignment));
    }
}
fn alignOf(self: *Generator, @"type": Type, alignment: Alignment) WORD {
    return switch (@"type") {
        .float => |width| @intFromEnum(width) / 8,
        .int => |int| @intFromEnum(int.width) / 8,
        .vector => |vector| self.alignOf(self.typeFromID(vector.component_id), alignment) * //
            @as(WORD, if (alignment == .scalar) 1 else if (vector.len == ._2) 2 else 4),
        .array => |array| util.wrut(self.alignOf(self.typeFromID(array.component_id), alignment), if (alignment == .extended) 16 else 1),
        .matrix => |matrix| util.wrut(self.alignOf(self.typeFromID(matrix.column_type_id), alignment), if (alignment == .extended) 16 else 1),
        else => @panic("idk alignment of this type"),
    };
}
fn sizeOf(self: *Generator, @"type": Type) WORD {
    return switch (@"type") {
        .int => |int| @intFromEnum(int.width) / 8,
        .float => |width| @intFromEnum(width) / 8,
        .vector => |vector| @intFromEnum(vector.len) * self.sizeOf(self.typeFromID(vector.component_id)),
        .matrix => |matrix| @intFromEnum(matrix.column_count) * self.sizeOf(self.typeFromID(matrix.column_type_id)),
        else => 0,
    };
}
const Alignment = enum { scalar, base, extended };

fn generateStatment(self: *Generator, statement: Parser.Statement) Error!void {
    return switch (statement) {
        .var_decl => |var_decl| try self.generateVariableDeclaration(var_decl),
        .assignment => |assignment| try self.generateAssignment(assignment),
        else => @panic("cant gen such statement"),
    };
}
fn generateAssignment(self: *Generator, assignment: Parser.Assignment) Error!void {
    const ptr = try self.generatePointer(assignment.target);
    const value = try self.generateExpression(assignment.value);
    try self.addWords(&.{ opWord(.store, 3), ptr.id, value });
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
            if (!self.inGlobalScope()) try self.current_name_mappings.append(self.arena, .{
                .type_id = try self.convertTypeID(var_decl.type),
                .id = 0,
                .name = var_decl.name,
                .load = try self.generateExpression(var_decl.initializer),
            }) else @panic("TODO: global scope constant");
            return;
        },
        .shared => .workgroup,
        .uniform => if (var_decl.type == .buffer)
            (if (var_decl.type.buffer.type == .ssbo) .storage_buffer else .uniform)
        else
            .uniform_constant,
        .member => unreachable,
    };
    var initializer: WORD = 0;
    var load: WORD = 0;
    const type_id = try self.convertTypeID(var_decl.type);
    const var_id = if (var_decl.qualifier != .push) self.newID() else 0;
    io: { //check if we should add io
        const io_type: IOType = switch (var_decl.qualifier) {
            .in => .in,
            .out => .out,
            .uniform => .uniform,
            .push => .push,

            else => break :io,
        };
        const io_entry: IOEntry = .{
            .id = var_id,
            .type_id = type_id,
            .io_type = io_type,
            .descriptor_set = if (var_decl.qualifier == .uniform) var_decl.qualifier.uniform else 0,
        };

        if (self.inGlobalScope()) {
            for (self.parser.global_scope.global_io.items, 0..) |g, i| {
                if (util.strEql(g, var_decl.name)) {
                    self.global_io[i] = io_entry;
                    break;
                }
            } else unreachable;
        } else {
            const entry_point = self.entry_points.items[self.entry_points.items.len - 1];
            for (entry_point.val_ptr.io.items, 0..) |l, i| {
                if (util.strEql(l, var_decl.name)) {
                    entry_point.local_io[i] = io_entry;
                    break;
                }
            } else unreachable;
        }
    }

    if (!var_decl.initializer.isEmpty()) switch (var_decl.initializer) {
        .value => |value| {
            if (!var_decl.initializer.isEmpty()) initializer = try self.generateValue(value);
        },
        else => {
            if (self.inGlobalScope()) {
                try self.deferred_global_initializers.append(self.arena, .{
                    .index = @truncate(self.global_vars.items.len),
                    .expr = &var_decl.initializer,
                });
            } else {
                load = try self.generateExpression(var_decl.initializer);
                try self.addWords(&.{ opWord(.store, 3), var_id, load });
            }
        },
    };
    if (var_decl.qualifier == .push) {
        if (self.inGlobalScope())
            try self.global_push_constants.append(self.arena, .{
                .name = var_decl.name,
                .load = load,
                .type_id = type_id,
            })
        else {
            try self.addPushConstant(var_decl.name, type_id, initializer, load);
        }
    }
    const ptr_type_id = try self.typeID(.{ .pointer = .{
        .pointed_id = type_id,
        .storage_class = storage_class,
    } });
    switch (var_decl.qualifier) {
        .in, .out => {
            const interpolation = if (var_decl.qualifier == .in) var_decl.qualifier.in else var_decl.qualifier.out;
            if (interpolation != .smooth)
                try self.decorations.appendSlice(self.arena, &.{
                    opWord(.decorate, 3),
                    var_id,
                    @intFromEnum(@as(Decoration, switch (interpolation) {
                        .noperspective => .no_perspective,
                        .flat => .flat,
                        .smooth => unreachable,
                    })),
                });
        },
        else => {},
    }
    if (storage_class == .push_constant) return;

    if (storage_class == .function) {
        try self.current_variable_buffer.appendSlice(self.arena, &.{ opWord(.variable, if (initializer == 0) 4 else 5), ptr_type_id, var_id, @intFromEnum(StorageClass.function) });
        if (initializer != 0) try self.addWord(initializer);
        try self.current_name_mappings.append(self.arena, .{ .type_id = type_id, .id = var_id, .name = var_decl.name, .load = load });
    } else {
        if (!self.inGlobalScope()) try self.addInterfaceID(var_id);
        try self.global_vars.append(self.arena, .{
            .name = var_decl.name,
            .type_id = ptr_type_id,
            .id = var_id,
            .storage_class = storage_class,

            .initializer = initializer,
            .load = 0,
            .entry_point_index = self.currentEntryPointIndex(),
        });
    }
}
fn addPushConstant(self: *Generator, name: []const u8, type_id: WORD, initializer: WORD, load: WORD) Error!void {
    if (self.inGlobalScope()) return; //push constant in global scope

    const entry_point = &self.entry_points.items[self.entry_points.items.len - 1];

    if (entry_point.push_constant_struct_type_id == 0) {
        entry_point.push_constant_struct_type_id = self.newID();
        try self.decorations.appendSlice(self.arena, &.{ //add block decoration to the structure type
            opWord(.decorate, 3),
            entry_point.push_constant_struct_type_id,
            @intFromEnum(Decoration.block),
        });
        try self.type_decls.append(self.arena, .{
            .id = entry_point.push_constant_struct_type_id,
            .type = .{ .@"struct" = &.{} },
        });
        _ = try self.typeID(.{
            .pointer = .{ //add pointer type
                .pointed_id = entry_point.push_constant_struct_type_id,
                .storage_class = .push_constant,
            },
        });
    }

    if (entry_point.push_constant_struct_id == 0)
        entry_point.push_constant_struct_id = self.newID();

    try entry_point.push_constants.append(self.arena, .{
        .name = name,
        .load = load,
        .initializer = initializer,
        .type_id = type_id,
    });
}

fn checkEntryPointIndex(self: *Generator, entry_point_index: u32) bool {
    return ~entry_point_index == 0 or self.currentEntryPointIndex() == entry_point_index;
}
fn currentEntryPointIndex(self: *Generator) u32 {
    return if (!self.inGlobalScope()) @truncate(self.entry_points.items.len - 1) else ~@as(u32, 0);
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
    const label_id = self.newID();

    const index = self.entry_points.items.len;
    try self.entry_points.append(self.arena, .{
        .val_ptr = entry_point,
        .name = name,

        .id = entry_point_id,
        .stage_info = entry_point.shader_stage_info,
        .local_io = try self.arena.alloc(IOEntry, entry_point.io.items.len),
    });

    _ = try self.typeID(.void); //so that ids are assigned before function body
    _ = try self.typeID(.{ .function = .{ .rtype_id = try self.typeID(.void) } });

    for (self.global_push_constants.items) |pc| //add global push constants
        try self.addPushConstant(pc.name, pc.type_id, pc.initializer, 0);

    for (entry_point.body.items) |statement| //the meat
        try self.generateStatment(statement);

    if (self.entry_points.items[index].push_constant_struct_id != 0)
        try interface_ids.append(self.arena, self.entry_points.items[index].push_constant_struct_id);

    self.entry_points.items[index].interface_ids = try interface_ids.toOwnedSlice(self.arena);

    try self.addWords(&.{ opWord(.@"return", 1), opWord(.function_end, 1) });

    try self.main_buffer.appendSlice(self.arena, &.{
        opWord(.function, 5),
        try self.typeID(.void),
        entry_point_id,
        @intFromEnum(FunctionControl.none),
        try self.typeID(.{ .function = .{ .rtype_id = try self.typeID(.void) } }),
    });
    try self.main_buffer.appendSlice(self.arena, &.{ opWord(.label, 2), label_id });
    try self.main_buffer.appendSlice(self.arena, variable_buffer.items);
    try self.main_buffer.appendSlice(self.arena, buffer.items);
    buffer.deinit(self.arena);
    variable_buffer.deinit(self.arena);
}
fn generateExpression(self: *Generator, expr: Expression) Error!WORD {
    const type_id = try self.convertTypeID(try self.parser.typeOf(expr));

    const result = switch (expr) {
        .value => |value| try self.generateValue(value),
        .bin_op => |bin_op| try self.generateBinOp(bin_op, type_id),
        .u_op => |u_op| try self.generateUOp(u_op, type_id),
        .call => |call| try self.generateCall(call, type_id),
        .constructor => |constructor| try self.generateConstructor(constructor.components, type_id),
        .identifier => |identifier| try self.generateIdentifier(identifier),
        .indexing, .member_access => try self.generateAccessChain(expr),
        .builtin => |builtin| if (builtin == .variable) try self.generatePointerLoad(try self.generateBuiltinVariableIDInfo(builtin.variable)) else @panic("gen builtin function"),
        .cast => |cast| try self.generateCast(cast),
        else => {
            std.debug.print("Cannot gen expr: {f}\n", .{expr});
            @panic("idk how to gen that expr");
        },
    };
    return result;
}

fn generateCast(self: *Generator, cast: Parser.Cast) Error!WORD {
    const result_type_id = try self.convertTypeID(cast.type);

    const from_type = try self.parser.typeOf(cast.expr.*);
    if (cast.type == .matrix) {
        if (from_type.isNumber()) @panic("mat from scalar");
        if (from_type == .matrix) {
            //load matrix so that generateIndexing uses the loaded value
            _ = try self.generateExpression(cast.expr.*);

            const n: WORD = @intFromEnum(cast.type.matrix.n);
            const m: WORD = @intFromEnum(cast.type.matrix.m);
            const v01 = if (m > @intFromEnum(from_type.matrix.m) or n > @intFromEnum(from_type.matrix.n)) switch (cast.type.matrix.width) {
                inline else => |width| blk: {
                    const V = @Vector(2, @Type(.{ .float = .{ .bits = @intFromEnum(width) } }));
                    const vec: V = .{ 0, 1 };
                    break :blk try self.generateValue(.{
                        .type = .{ .vector = .{ .len = ._2, .component = .{ .type = .float, .width = width } } },
                        .payload = .{ .ptr = @ptrCast(@alignCast(@constCast(&vec))) },
                    });
                },
            } else 0;
            var column_ids: [4]WORD = @splat(0);
            for (0..n) |x| {
                if (x < @intFromEnum(from_type.matrix.n)) {
                    const column = try self.generateAccessChain(.{ .indexing = .{ .target = cast.expr, .index = @constCast(&Parser.Expression{ .value = .{
                        .type = tp.u32_type,
                        .payload = .{ .wide = x },
                    } }) } });
                    column_ids[x] = try self.addWordsReturnResult(&.{
                        opWord(.vector_shuffle, @truncate(5 + m)),
                        try self.convertTypeID(.{ .vector = cast.type.matrix.columnVector() }),
                        self.newID(),
                        column,
                        if (v01 == 0) column else v01,
                    });
                    for (0..m) |index| try self.addWord(if (index < @intFromEnum(from_type.matrix.m)) @truncate(index) else if (index == x) 1 else 0);
                } else {
                    column_ids[x] = try self.addWordsReturnResult(&.{
                        opWord(.vector_shuffle, @truncate(5 + m)),
                        try self.convertTypeID(.{ .vector = cast.type.matrix.columnVector() }),
                        self.newID(),
                        v01,
                        v01,
                    });
                    for (0..m) |index| try self.addWord(if (index == x) 1 else 0);
                }
            }
            const result = try self.addWordsReturnResult(&.{
                opWord(.composite_construct, @truncate(3 + n)),
                result_type_id,
                self.newID(),
            });
            try self.addWords(column_ids[0..n]);
            return result;
        } else unreachable;
    }
    @panic("idk how to gen that cast");
}
fn generateIdentifier(self: *Generator, identifier: []const u8) Error!WORD {
    const id_info = try self.getNameInfo(identifier);
    if (id_info.load.* != 0) return id_info.load.*;
    const load = try self.addWordsReturnResult(&.{
        opWord(.load, 4),
        self.typeFromID(id_info.type_id).pointer.pointed_id,
        self.newID(),
        id_info.id,
    });
    id_info.load.* = load;
    return load;
}

fn generateAccessChain(self: *Generator, expr: Expression) Error!WORD {
    const access_type_id = try self.convertTypeID(try self.parser.typeOf(expr));

    var access_chain: List(WORD) = .empty;
    defer access_chain.deinit(self.arena);

    var const_mask: u64 = 0;
    var target_name_info = try self.generateAccessChainTargetIDInfoRecursive(expr, &access_chain, &const_mask);
    for (0..access_chain.items.len / 2) |i| //reverse access chain
        std.mem.swap(WORD, &access_chain.items[i], &access_chain.items[access_chain.items.len - i - 1]);

    const is_target_vector = if (target_name_info.id != 0)
        self.typeFromID(self.typeFromID(target_name_info.type_id).pointer.pointed_id) == .vector
    else
        self.typeFromID(target_name_info.type_id) == .vector;

    //convert all literals to ids if not all indices are compime known
    if (const_mask != 0) for (access_chain.items, 0..) |*ac, i| {
        if (((const_mask >> @truncate(i)) & 1) == 0)
            ac.* = try self.generateValue(.{ .type = tp.u32_type, .payload = .{ .wide = ac.* } });
    };

    const l = access_chain.items.len;
    if (target_name_info.load.* == 0 and const_mask == 0 and is_target_vector) {
        target_name_info.load.* = try self.addWordsReturnResult(&.{
            opWord(.load, 4),
            self.typeFromID(target_name_info.type_id).pointer.pointed_id,
            self.newID(),
            target_name_info.id,
        });
    }
    if (target_name_info.load.* != 0) {
        if (const_mask == 0) {
            const id = self.newID();
            try self.addWords(&.{
                opWord(.composite_extract, @truncate(4 + l)),
                access_type_id,
                id,
                target_name_info.load.*,
            });
            try self.addWords(access_chain.items);
            return id;
        } else if (is_target_vector and l == 1) return try self.addWordsReturnResult(&.{
            opWord(.vector_extract_dynamic, 5),
            access_type_id,
            self.newID(),
            target_name_info.load.*,
            access_chain.items[0],
        });

        //create variable
        if (target_name_info.id == 0) {
            const initialize = for (self.constants.items) |constant| (if (constant.id == target_name_info.load.*) break true) else false;
            const id = self.newID();
            const ptr_type_id = try self.typeID(.{ .pointer = .{ .pointed_id = target_name_info.type_id, .storage_class = .function } });
            try self.current_variable_buffer.appendSlice(self.arena, &.{
                opWord(.variable, if (initialize) 5 else 4),
                ptr_type_id,
                id,
                @intFromEnum(StorageClass.function),
            });
            if (initialize) try self.current_variable_buffer.append(self.arena, target_name_info.load.*);

            for (self.current_name_mappings.items) |*nm| if (nm.load == target_name_info.load.*) {
                nm.type_id = ptr_type_id;
                nm.id = id;
            };
            target_name_info.id = id;
        }
    }
    for (access_chain.items, 0..) |*ac, i| { //turn all the index literals into ids anyway
        if (((const_mask >> @truncate(i)) & 1) == 0)
            ac.* = try self.generateValue(.{ .type = tp.u32_type, .payload = .{ .wide = ac.* } });
    }
    const access_ptr_type_id = try self.typeID(.{ .pointer = .{ .pointed_id = access_type_id, .storage_class = target_name_info.storage_class } });
    const access_ptr = try self.addWordsReturnResult(&.{
        opWord(.access_chain, @truncate(4 + l)),
        access_ptr_type_id,
        self.newID(),
        target_name_info.id,
    });
    try self.addWords(access_chain.items);

    const load = try self.addWordsReturnResult(&.{
        opWord(.load, 4),
        access_type_id,
        self.newID(),
        access_ptr,
    });
    return load;
}
fn generateAccessChainTargetIDInfoRecursive(
    self: *Generator,
    expr: Expression,
    access_chain: *List(WORD),
    const_mask: *u64,
) Error!IDInfo {
    const index_word: WORD, //
    const is_index_comptime: bool, //
    const target: Expression = switch (expr) {
        .indexing => |indexing| .{
            if (indexing.index.* == .value)
                @truncate(indexing.index.value.payload.wide)
            else
                try self.generateExpression(indexing.index.*),
            indexing.index.* == .value,
            indexing.target.*,
        },
        .member_access => |member_access| .{
            switch (try self.parser.typeOf(member_access.target.*)) {
                .@"struct" => |struct_id| self.parser.getStructFromID(struct_id).memberIndex(member_access.member_name).?,
                .buffer => |buffer| self.parser.getStructFromID(buffer.struct_id).memberIndex(member_access.member_name).?,
                else => unreachable,
            },
            true,
            member_access.target.*,
        },
        else => unreachable,
    };

    const_mask.* = const_mask.* << 1;
    if (!is_index_comptime) const_mask.* |= 1;
    if (access_chain.items.len >= 64) return Error.GenError;

    try access_chain.append(self.arena, index_word);

    return switch (target) {
        .indexing, .member_access => try self.generateAccessChainTargetIDInfoRecursive(target, access_chain, const_mask),
        else => try self.generatePointer(target),
    };
}

fn getNameInfo(self: *Generator, name: []const u8) Error!IDInfo {
    if (!self.inGlobalScope()) {
        const entry_point = &self.entry_points.items[self.entry_points.items.len - 1];
        for (entry_point.push_constants.items, 0..) |*pc, i| {
            if (util.strEql(name, pc.name)) {
                const ptr_type_id = try self.typeID(.{ .pointer = .{ .pointed_id = pc.type_id, .storage_class = .push_constant } });

                const access = if (pc.ptr != 0) pc.ptr else try self.addWordsReturnResult(&.{
                    opWord(.access_chain, 5),
                    try self.typeID(.{ .pointer = .{ .pointed_id = pc.type_id, .storage_class = .push_constant } }),
                    self.newID(),
                    entry_point.push_constant_struct_id,
                    try self.generateValue(.{ .type = tp.u32_type, .payload = .{ .wide = i } }),
                });
                pc.ptr = access;
                return .{
                    .type_id = ptr_type_id,
                    .id = access,
                    .load = &pc.load,
                    .global = false,
                    .storage_class = .push_constant,
                };
            }
        }
        for (self.current_name_mappings.items) |*nm|
            if (util.strEql(name, nm.name))
                return .{ .type_id = nm.type_id, .id = nm.id, .load = &nm.load, .global = false };
    }
    return for (self.global_vars.items) |*gv| {
        if (util.strEql(name, gv.name) and self.checkEntryPointIndex(gv.entry_point_index)) {
            if (!self.inGlobalScope()) try self.addInterfaceID(gv.id);
            break .{ .type_id = gv.type_id, .id = gv.id, .load = &gv.load, .global = true, .storage_class = gv.storage_class };
        }
    } else @panic("couldnt find variable by name somehow?");
}
const IDInfo = struct {
    type_id: WORD = 0,
    id: WORD = 0,
    load: *WORD = @constCast(&@as(u32, 0)),
    global: bool = false,
    storage_class: StorageClass = .function,
};
// const NameMapping = struct { name: []const u8, type_id: WORD, id: WORD, load: WORD = 0 };
// fn generateIndexingVariable(self: *Generator, indexing: Parser.Indexing, access_type_id: WORD) Error!WORD {
// }

fn generatePointerLoad(self: *Generator, ptr_info: IDInfo) Error!WORD {
    if (ptr_info.load.* != 0) return ptr_info.load.*;

    const type_id = self.typeFromID(ptr_info.type_id).pointer.pointed_id;
    return try self.addWordsReturnResult(&.{ opWord(.load, 4), type_id, self.newID(), ptr_info.id });
}
fn generatePointer(self: *Generator, expr: Expression) Error!IDInfo {
    return switch (expr) {
        .identifier => |identifier| try self.generateIdentifierPointer(identifier),
        .builtin => |builtin| if (builtin == .variable)
            try self.generateBuiltinVariableIDInfo(builtin.variable)
        else
            unreachable,
        else => unreachable,
    };
}

fn generateIdentifierPointer(self: *Generator, identifier: []const u8) Error!IDInfo {
    if (!self.inGlobalScope()) for (self.current_name_mappings.items) |*nm|
        if (util.strEql(identifier, nm.name)) {
            const var_id = self.newID();
            const ptr_type_id =
                if (nm.id == 0)
                    try self.typeID(.{ .pointer = .{ .pointed_id = nm.type_id, .storage_class = .function } })
                else
                    nm.type_id;

            const initialize = for (self.constants.items) |c| {
                if (c.id == nm.load) break true;
            } else false;

            try self.current_variable_buffer.appendSlice(self.arena, &.{
                opWord(.variable, if (initialize) 5 else 4),
                ptr_type_id,
                var_id,
                @intFromEnum(StorageClass.function),
            });

            if (initialize) {
                try self.current_variable_buffer.append(self.arena, nm.load);
            } else try self.addWords(&.{ opWord(.store, 3), var_id, nm.load });

            nm.id = var_id;
            nm.type_id = ptr_type_id;

            return .{ .type_id = nm.type_id, .id = nm.id, .load = &nm.load };
        };

    return self.getNameInfo(identifier);
}

fn generateBuiltinVariableIDInfo(self: *Generator, bv: bi.BuiltinVariable) Error!IDInfo {
    return switch (bv) {
        .position, .point_size, .cull_distance => blk: {
            const member: WORD = if (bv == .position) 0 else if (bv == .point_size) 1 else 2;
            const member_value: Parser.Value = .{ .type = tp.u32_type, .payload = .{ .wide = member } };
            const ptr_info = &self.builtin_pointer_infos.per_vertex;
            const load: *WORD = ([3]*WORD{
                &ptr_info.position_load,
                &ptr_info.point_size_load,
                &ptr_info.cull_distance_load,
            })[member];
            const storage_class: StorageClass = .output;
            const ptr_type_id = try self.typeID(.{ .pointer = .{
                .pointed_id = try self.convertTypeID(bv.typeOf()),
                .storage_class = storage_class,
            } });

            if (ptr_info.ptr.id != 0) break :blk .{ .type_id = ptr_type_id, .load = load };

            const float_type_id = try self.typeID(.{ .float = .word });
            self.builtin_pointer_infos.per_vertex.type_id_storage[0] = try self.typeID(.{ .vector = .{ .len = ._4, .component_id = float_type_id } });
            self.builtin_pointer_infos.per_vertex.type_id_storage[1] = float_type_id;
            self.builtin_pointer_infos.per_vertex.type_id_storage[2] = try self.typeID(.{ .array = .{ .len = 1, .component_id = float_type_id } });
            const per_vertex_type_id = try self.typeID(.{ .@"struct" = &self.builtin_pointer_infos.per_vertex.type_id_storage });

            const per_vertex_ptr_type_id = try self.typeID(.{ .pointer = .{ .pointed_id = per_vertex_type_id, .storage_class = storage_class } });
            const var_id = self.newID();
            ptr_info.ptr = .{ .id = var_id, .type_id = per_vertex_ptr_type_id };
            try self.global_vars.append(self.arena, .{
                .type_id = per_vertex_ptr_type_id,
                .id = var_id,
                .storage_class = storage_class,
            });
            try self.decorations.appendSlice(self.arena, &.{ opWord(.decorate, 3), per_vertex_type_id, @intFromEnum(Decoration.block) });
            try self.decorations.appendSlice(self.arena, &.{ opWord(.member_decorate, 5), per_vertex_type_id, 0, @intFromEnum(Decoration.builtin), @intFromEnum(BuiltinDecoration.position) });
            try self.decorations.appendSlice(self.arena, &.{ opWord(.member_decorate, 5), per_vertex_type_id, 1, @intFromEnum(Decoration.builtin), @intFromEnum(BuiltinDecoration.point_size) });
            try self.decorations.appendSlice(self.arena, &.{ opWord(.member_decorate, 5), per_vertex_type_id, 2, @intFromEnum(Decoration.builtin), @intFromEnum(BuiltinDecoration.cull_distance) });

            try self.current_interface_ids.append(self.arena, var_id);

            const id = try self.addWordsReturnResult(&.{
                opWord(.access_chain, 5),
                ptr_type_id,
                self.newID(),
                var_id,
                try self.generateValue(member_value),
            });
            break :blk .{ .id = id, .type_id = ptr_type_id, .load = load };
        },
        .vertex_id => blk: {
            const id_info = &self.builtin_pointer_infos.vertex_id;
            if (id_info.id != 0) break :blk id_info.*;

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
                @intFromEnum(BuiltinDecoration.vertex_index),
            });
            try self.current_interface_ids.append(self.arena, var_id);
            id_info.* = .{ .id = var_id, .type_id = try self.typeID(.{ .pointer = .{ .pointed_id = type_id, .storage_class = .input } }) };
            return id_info.*;
        },
        .frag_coord => blk: {
            const id_info = &self.builtin_pointer_infos.frag_coord;
            if (id_info.id != 0) break :blk id_info.*;

            const var_id = self.newID();
            const type_id = try self.typeID(.{ .vector = .{
                .len = ._4,
                .component_id = try self.typeID(.{ .float = .word }),
            } });
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
                @intFromEnum(BuiltinDecoration.frag_coord),
            });
            try self.current_interface_ids.append(self.arena, var_id);
            id_info.* = .{ .id = var_id, .type_id = try self.typeID(.{ .pointer = .{ .pointed_id = type_id, .storage_class = .input } }) };
            break :blk id_info.*;
        },
        inline else => |b| @panic("idk how to gen that builtin - " ++ @tagName(b)),
    };
}

fn generateConstructor(self: *Generator, components: []Expression, result_type_id: WORD) Error!WORD {
    const id = self.newID();
    var quad: [4]WORD = @splat(0);
    const slice = if (components.len <= 4) quad[0..components.len] else try self.arena.alloc(WORD, components.len);
    defer if (components.len > 4) self.arena.free(slice);

    for (slice, components) |*s, c| s.* = try self.generateExpression(c);

    try self.addWords(&.{ opWord(.composite_construct, @truncate(3 + components.len)), result_type_id, id });
    for (slice) |s| try self.addWord(s);
    return id;
}
fn generateCall(self: *Generator, call: Parser.Call, result_type_id: WORD) Error!WORD {
    return switch (call.callee.*) {
        .value => |value| blk: {
            const function: *const Parser.Function = @ptrCast(@alignCast(value.payload.ptr));
            break :blk if (function.builtin_handle) |bh|
                switch (bh) {
                    .sample => //generate sample instruction
                    self.addWordsReturnResult(&.{
                        opWord(.image_sample_implicit_lod, 5),
                        try self.convertTypeID(tp.vec4_type),
                        self.newID(),
                        try self.generateExpression(call.args[0]),
                        try self.generateSampleCoordinateID(
                            call.args[1],
                            if (call.args.len > 2) call.args[2] else null,
                        ),
                    }),

                    else => unreachable,
                }
            else
                @panic("generate function");
        },
        .builtin => |builtin| switch (builtin.function) {
            .reflect => try self.addWordsReturnResult(&.{
                opWord(.ext_inst, 7),
                result_type_id,
                self.newID(),
                glsl_std_id,
                @intFromEnum(GlslStdExtOp.reflect),
                try self.generateExpression(call.args[0]),
                try self.generateExpression(call.args[1]),
            }),
            .inverse => try self.addWordsReturnResult(&.{
                opWord(.ext_inst, 6),
                result_type_id,
                self.newID(),
                glsl_std_id,
                @intFromEnum(GlslStdExtOp.matrix_inverse),
                try self.generateExpression(call.args[0]),
            }),
            .transpose => try self.addWordsReturnResult(&.{
                opWord(.transpose, 4),
                result_type_id,
                self.newID(),
                try self.generateExpression(call.args[0]),
            }),
            else => @panic("idk how to gen that builtin call"),
        },
        else => @panic("idk how to gen that call"),
    };
}
fn generateSampleCoordinateID(self: *Generator, coord: Expression, array_layer: ?Expression) Error!WORD {
    const array_layer_expr = if (array_layer) |al| al else return try self.generateExpression(coord);

    _ = array_layer_expr;
    @panic("arrayd sample");
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
            const slice = if (array.len <= 4) word_array[0..array.len] else try self.arena.alloc(WORD, array.len);
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
    for (self.constants.items) |c| if (c.type_id == type_id and constantValueEql(c.value, value, self.typeWordConsumption(self.typeFromID(type_id)))) return c.id;
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
        .@"^" => try self.addWordsReturnResult(&.{ opWord(.ext_inst, 7), result_type_id, self.newID(), glsl_std_id, @intFromEnum(GlslStdExtOp.pow), left, right }),
        .@"+" => blk: {
            const @"type" = self.typeFromID(result_type_id);
            const op: Op = if (@"type" == .float or (@"type" == .vector and self.typeFromID(@"type".vector.component_id) == .float)) .fadd else .iadd;
            break :blk try self.addWordsReturnResult(&.{ opWord(op, 5), result_type_id, self.newID(), left, right });
        },
        .@"-" => blk: {
            //matrices??
            const @"type" = self.typeFromID(result_type_id);
            const op: Op = if (@"type" == .float or (@"type" == .vector and self.typeFromID(@"type".vector.component_id) == .float)) .fsub else .isub;
            break :blk try self.addWordsReturnResult(&.{ opWord(op, 5), result_type_id, self.newID(), left, right });
        },
        .@"***", .@"**" => blk: {
            //zero value for dotClamped
            // const ptype = self.parser.typeOf(bin_op.left.*) catch unreachable;
            // const zero_value: WORD = try self.generateValue(.{ .type = (ptype).vector.component, .payload = .{ .wide = 0 } });
            const rt = self.typeFromID(result_type_id);
            const op: Op = switch (rt) {
                .float => .dot,
                .int => |int| if (int.signed) .sdot else .udot,
                else => unreachable,
            };

            const dot = try self.addWordsReturnResult(&.{ opWord(op, 5), result_type_id, self.newID(), left, right });
            if (bin_op.op == .@"**") break :blk dot;

            const max_op: GlslStdExtOp = switch (rt) {
                .float => .fmax,
                .int => |int| if (int.signed) .smax else .umax,
                else => unreachable,
            };

            const zero_const = try self.generateValue(
                .{ .type = try self.parser.typeOf(.{ .bin_op = bin_op }), .payload = .{ .wide = 0 } },
            );
            const clamped = try self.addWordsReturnResult(&.{
                opWord(.ext_inst, 7),
                result_type_id,
                self.newID(),
                glsl_std_id,
                @intFromEnum(max_op),
                dot,
                zero_const,
            });
            break :blk clamped;
        },
        .@"*" => blk: {
            const left_type = try self.parser.typeOf(bin_op.left.*);
            const right_type = try self.parser.typeOf(bin_op.right.*);
            if (!Parser.Type.eql(left_type, right_type)) {
                var deep, var shallow, var deep_type, var shallow_type = .{ left, right, left_type, right_type };
                if (deep_type.depth() < shallow_type.depth())
                    deep, shallow, deep_type, shallow_type = .{ shallow, deep, shallow_type, deep_type };

                if (deep_type == .matrix) break :blk self.addWordsReturnResult(&.{
                    opWord(if (deep == left) .matrix_times_vector else .vector_times_matrix, 5),
                    try self.convertTypeID(shallow_type),
                    self.newID(),
                    left,
                    right,
                });
                //asymmetric ones:
                //scalar x vector (both ways)
                //matrix x vector (both ways)
                //matrix x scalar (both ways)

                @panic("TODO: different type multipilcation");
            }
            //mat x mat

            const scalar = left_type.scalarPrimitive().?;
            const op: Op = if (left_type == .matrix) .matrix_times_matrix else if (scalar.type == .float) .fmul else .imul;
            break :blk try self.addWordsReturnResult(&.{ opWord(op, 5), result_type_id, self.newID(), left, right });
        },
        else => @panic("idk how to gen that bin op"),
    };
}
fn generateUOp(self: *Generator, u_op: Parser.UOp, result_type_id: WORD) Error!WORD {
    const target = try self.generateExpression(u_op.target.*);
    return switch (u_op.op) {
        .@"|" => try self.addWordsReturnResult(&.{ opWord(.ext_inst, 6), result_type_id, self.newID(), glsl_std_id, @intFromEnum(GlslStdExtOp.normalize), target }),
        else => @panic("idk how to gen that u op"),
    };
}
fn addInterfaceID(self: *Generator, id: WORD) Error!void {
    if (for (self.current_interface_ids.items) |ii| (if (ii == id) break false) else true)
        try self.current_interface_ids.append(self.arena, id);
}
fn generateConstantFromScalar(self: *Generator, scalar: anytype, type_id: WORD) Error!WORD {
    const U = @Type(.{ .int = .{ .bits = @sizeOf(@TypeOf(scalar)) * 8, .signedness = .unsigned } });
    const uval: U = @bitCast(scalar);
    return try self.addConstant(type_id, if (@sizeOf(@TypeOf(uval)) > 4)
        .{ .many = .{ @truncate(uval), @truncate(uval >> 32), 0, 0 } }
    else
        .{ .single = @truncate(uval) });
}

fn addWordsReturnResult(self: *Generator, words: []const WORD) Error!WORD {
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
    stage_info: StageInfo,
    interface_ids: []WORD = &.{},
    local_io: []IOEntry = &.{},

    push_constants: List(PushConstant) = .empty,
    push_constant_struct_type_id: WORD = 0,
    push_constant_struct_id: WORD = 0,
};
const PushConstant = struct {
    name: []const u8,

    load: WORD = 0,
    ptr: WORD = 0,
    type_id: WORD = 0,

    initializer: WORD = 0,
};
const GlobalInitializer = struct {
    index: u32,
    expr: *const Expression,
};
const GlobalVariable = struct {
    name: []const u8 = "",
    type_id: WORD,
    id: WORD,
    storage_class: StorageClass,
    initializer: WORD = 0,
    load: WORD = 0,
    entry_point_index: u32 = ~@as(u32, 0),
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
fn toIOType(self: *Generator, @"type": Type) hgsl.IOType {
    return switch (@"type") {
        .float => |width| .{ .scalar = if (width == .word)
            .f32
        else if (width == .long)
            .f64
        else
            unreachable },
        .int => |int| .{ .scalar = if (int.width == .word)
            if (int.signed) .i32 else .u32
        else if (int.width == .long)
            if (int.signed) .i64 else .u64
        else
            unreachable },
        .vector => |vector| .{ .vector = .{
            .len = vector.len,
            .component = self.toIOType(self.typeFromID(vector.component_id)).scalar,
        } },
        else => unreachable,
    };
}

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
        .array => |array| .{ .array = .{
            .len = array.len,
            .component_id = try self.convertTypeID(array.component.*),
        } },
        .matrix => |matrix| .{ .matrix = .{
            .column_count = matrix.n,
            .column_type_id = try self.typeID(.{ .vector = .{ .len = matrix.m, .component_id = try self.typeID(.{ .float = matrix.width }) } }),
        } },
        .@"struct" => |struct_id| blk: {
            const s = self.parser.getStructFromID(struct_id);
            const slice = try self.arena.alloc(WORD, s.members.items.len);
            for (slice, s.members.items) |*id, m| id.* = try self.convertTypeID(m.type);
            break :blk .{ .@"struct" = slice };
        },
        .buffer => |buffer| .{ .buffer = .{
            .struct_id = try self.convertTypeID(.{ .@"struct" = buffer.struct_id }),
            .storage_class = .uniform,
        } },
        .texture => |texture| {
            const dim: Dim, const arrayed: bool = switch (texture.type) {
                ._1d => .{ .@"1d", false },
                ._2d => .{ .@"2d", false },
                ._3d => .{ .@"3d", false },
                .cube => .{ .cube, false },
                ._1d_array => .{ .@"1d", true },
                ._2d_array => .{ .@"2d", true },
                .cube_array => .{ .cube, true },
            };
            const image_type: Image = .{
                .sampled_type_id = try self.convertTypeID(.{ .scalar = texture.texel_primitive }),
                .dim = dim,
                .depth = 0,
                .arrayed = arrayed,
                .multi_sampled = false,
                .sampled = texture.sampled,
            };
            return if (texture.sampled)
                .{ .sampled_image = .{ .image_id = try self.typeID(.{ .image = image_type }) } }
            else
                .{ .image = image_type };
        },

        else => {
            std.debug.print("TYPE: {f}, {s}\n", .{ from, @tagName(from) });
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
        if (@"type" == .array) {
            _ = try self.typeID(.{ .int = .{ .width = .word, .signed = false } });
            try self.array_length_constants.append(self.arena, .{ .id = self.newID(), .val = @"type".array.len });
        }
        const id = self.newID();

        try self.type_decls.append(self.arena, .{ .type = @"type", .id = id });
        break :blk id;
    };
}
fn newID(self: *Generator) WORD {
    self.current_id += 1;
    return self.current_id - 1;
}
fn createValue(self: *Generator, val: anytype) Error!*@TypeOf(val) {
    const ptr = try self.arena.create(@TypeOf(val));
    ptr.* = val;
    return ptr;
}

const Type = union(enum) {
    void,
    bool,

    float: BitWidth,
    int: Int,
    vector: Vector,
    matrix: Matrix,

    array: Array,

    function: Function,
    pointer: Pointer,

    @"struct": []const WORD,
    buffer: Buffer, //struct id

    image: Image,
    sampled_image: SampledImage,

    pub fn eql(a: Type, b: Type) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .void, .bool => true,
            .float => |float| float == b.float,
            .int => |int| int.width == b.int.width and int.signed == b.int.signed,
            .vector => |vector| vector.len == b.vector.len and vector.component_id == b.vector.component_id,
            .matrix => |matrix| matrix.column_count == b.matrix.column_count and matrix.column_type_id == b.matrix.column_type_id,
            .function => |function| function.rtype_id == b.function.rtype_id and
                if (function.arg_type_ids.len == b.function.arg_type_ids.len)
                    (for (function.arg_type_ids, b.function.arg_type_ids) |a_arg, b_arg| (if (a_arg != b_arg) break false) else true)
                else
                    false,
            .pointer => |pointer| pointer.pointed_id == b.pointer.pointed_id and pointer.storage_class == b.pointer.storage_class,
            .array => |array| array.component_id == b.array.component_id and array.len == b.array.len,
            .@"struct" => |st| std.mem.eql(WORD, st, b.@"struct"),
            .buffer => |buffer| buffer.struct_id == b.buffer.struct_id and buffer.storage_class == b.buffer.storage_class,
            .image => |image| std.meta.eql(image, b.image),
            .sampled_image => |sampled_image| sampled_image.image_id == b.sampled_image.image_id,
            // else => @panic("idk how to compare that type"),
        };
    }
};
pub fn typeWordConsumption(self: *Generator, @"type": Type) WORD {
    _ = self;
    return switch (@"type") {
        .float => |float| @as(WORD, if (float == .long) 2 else 1),
        .int => |int| @as(WORD, if (int.width == .long) 2 else 1),
        .vector => |vector| @intFromEnum(vector.len),
        .matrix => |matrix| @intFromEnum(matrix.column_count),
        .array => |array| array.len,
        .@"struct" => |st| @truncate(st.len),
        else => 1,
    };
}
const SampledImage = struct { image_id: WORD };
const Image = struct {
    sampled_type_id: WORD,
    dim: Dim,
    depth: WORD,
    arrayed: bool,
    multi_sampled: bool,
    sampled: bool,
};
const Dim = enum(u32) { @"1d" = 0, @"2d" = 1, @"3d" = 2, cube = 3 };
const Buffer = struct { struct_id: WORD, storage_class: StorageClass };
const Pointer = struct { pointed_id: WORD, storage_class: StorageClass };
const Function = struct { rtype_id: WORD, arg_type_ids: []WORD = &.{} };

const Matrix = struct { column_count: VectorLen, column_type_id: WORD };
const Vector = struct { len: VectorLen, component_id: WORD };
const Int = struct { width: BitWidth, signed: bool };
const BitWidth = Parser.tp.BitWidth;
const VectorLen = Parser.tp.VectorLen;
const Array = struct { len: WORD, component_id: WORD };
const TypeDeclaration = struct { type: Type, id: WORD };
fn opWord(op: Op, count: u16) WORD {
    return (@as(WORD, count) << 16) | @intFromEnum(op);
}
const Op = enum(WORD) {
    name = 5,
    constant_true = 41,
    constant_false = 42,
    constant = 43,
    constant_composite = 44,

    vector_extract_dynamic = 77, //when index is not comptime known only for vectors
    vector_insert_dynamic = 78, //vector[runtime_known] = x
    vector_shuffle = 79,
    composite_construct = 80,
    composite_extract = 81, //when index is comptime known
    composite_insert = 82,

    type_void = 19,
    type_bool = 20,
    type_int = 21,
    type_float = 22,
    type_vector = 23,
    type_matrix = 24,
    type_array = 28,
    type_pointer = 32,
    type_function = 33,
    type_struct = 30,
    type_image = 25,
    type_sampler = 26,
    type_sampled_image = 27,

    image_sample_implicit_lod = 87,

    capability = 17,
    entry_point = 15,
    execution_mode = 16,
    memory_model = 14,
    decorate = 71,
    member_decorate = 72,

    function = 54,
    label = 248,
    @"return" = 253,
    function_end = 56,

    variable = 59,

    load = 61,
    store = 62,
    access_chain = 65,

    iadd = 128,
    fadd = 129,
    isub = 130,
    fsub = 131,

    imul = 132,
    fmul = 133,
    vector_times_matrix = 144,
    matrix_times_vector = 145,
    matrix_times_matrix = 146,

    dot = 148,
    sdot = 4450,
    udot = 4451,

    transpose = 84,

    ext_inst_import = 11,
    ext_inst = 12,
};

const GlslStdExtOp = enum(WORD) {
    normalize = 69,
    reflect = 71,
    pow = 26,
    matrix_inverse = 34,

    fmax = 40,
    umax = 41,
    smax = 42,
};
const BuiltinIDInfos = struct {
    per_vertex: struct {
        ptr: IDInfo = .{},
        position_load: WORD = 0,
        point_size_load: WORD = 0,
        cull_distance_load: WORD = 0,

        type_id_storage: [3]WORD = @splat(0),
    } = .{},
    vertex_id: IDInfo = .{},
    frag_coord: IDInfo = .{},
};
const BuiltinDecoration = enum(WORD) {
    position = 0,
    point_size = 1,
    clip_distance = 3,
    cull_distance = 4,

    // vertex_id = 5,
    // instance_id = 6,
    vertex_index = 42,
    instance_index = 43,
    primitive_id = 7,
    invocation_id = 8,

    frag_coord = 15,
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

    offset = 35,
    array_stride = 6,
    matrix_stride = 7,
    col_major = 5,

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

const StageInfo = Parser.StageInfo;
const Expression = Parser.Expression;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const List = std.ArrayList;
