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
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
const Generator = @This();

const WORD = u32;

const Error = error{
    OutOfMemory,
    InvalidSpirvType,
    GenError,
} || Writer.Error;

parser: *Parser,
current_id: WORD = 1, //0 - reserved
arena: Allocator,

//constants
true_const: WORD = 0,
false_const: WORD = 0,
constants: List(Constant) = .empty,

// function_buff : List(u32) orsmth

type_decls: List(TypeDeclaration) = .empty,
pub fn generate(parser: *Parser) Error![]u32 {
    var generator: Generator = .{
        .parser = parser,
        .arena = parser.arena.allocator(),
    };
    for (generator.parser.global_scope.body.items) |statement| {
        if (statement == .var_decl) {
            std.debug.print("PT: {f}, id: {d}\n", .{ statement.var_decl.type, try generator.convertTypeID(statement.var_decl.type) });
            if (statement.var_decl.initializer == .value) {
                std.debug.print("VAL: {f}, ID: {d}\n", .{
                    statement.var_decl.initializer.value,
                    try generator.generateValue(statement.var_decl.initializer.value),
                });
            }
            if (statement.var_decl.type == .entrypoint) {
                const ep_ptr: *Parser.EntryPoint = @ptrCast(@alignCast(@constCast(statement.var_decl.initializer.value.payload.ptr)));
                for (ep_ptr.body.items) |epi| {
                    if (epi == .var_decl)
                        std.debug.print("PT: {f}, id: {d}\n", .{ epi.var_decl.type, try generator.convertTypeID(epi.var_decl.type) });
                }
            }
        }
    }
    _ = &generator;
    return &.{};
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

    _ = statement;
    _ = self;
}
fn generateExpression(self: *Generator, expr: Expression) Error!WORD {
    return switch (expr) {
        .value => |value| try self.generateValue(value),
        else => {
            std.debug.print("Cannot gen expr: {f}\n", .{expr});
            @panic("idk how to gen that expr");
        },
    };
}
fn generateValue(self: *Generator, value: Parser.Value) Error!WORD {
    return switch (value.type) {
        .bool => blk: {
            const bool_val = util.extract(bool, value.payload.wide);
            const ptr = if (bool_val) &self.true_const else &self.false_const;
            if (ptr.* == 0) ptr.* = self.newID();
            break :blk ptr.*;
        },
        .scalar, .vector => inline for (Parser.tp.Vector.allVectorTypes) |vector|
            (if (Parser.Type.eql(value.type, .{ .scalar = vector.component }))
                self.generateConstantFromScalar(@as(
                    if (vector.component.width == .long) u64 else WORD,
                    @truncate(value.payload.wide),
                ), try self.convertTypeID(.{ .scalar = vector.component }))
            else if (Parser.Type.eql(value.type, .{ .vector = vector }))
                try self.addConstant(try self.convertTypeID(.{ .vector = vector }), .{
                    .many = blk: {
                        var ids: [4]WORD = @splat(0);
                        const T = (Parser.Type{ .vector = vector }).ToZig();
                        const vec_info = @typeInfo(T).vector;

                        const ptr: *const T = @ptrCast(@alignCast(value.payload.ptr));
                        const component_type_id = try self.convertTypeID(.{ .scalar = vector.component });
                        inline for (0..vec_info.len) |i|
                            ids[i] = try self.generateConstantFromScalar(ptr[i], component_type_id);
                        break :blk ids;
                    },
                }))
        else
            unreachable,
        else => @panic("cant gen value :("),
    };
    // enum
    // matrix
    // array

}
fn generateConstantFromScalar(self: *Generator, scalar: anytype, type_id: WORD) Error!WORD {
    const U = @Type(.{ .int = .{ .bits = @sizeOf(scalar) * 8, .signedness = false } });
    const uval: U = @bitCast(scalar);
    return try self.addConstant(type_id, if (@sizeOf(@Type(uval)) > 4)
        .{ .many = .{ @truncate(uval), @truncate(uval >> 32), 0, 0 } }
    else
        .{ .single = @truncate(uval) });
}
fn addConstant(self: *Generator, type_id: WORD, value: ConstantValue) Error!WORD {
    const wc = self.typeFromID(type_id).valueWordConsumption();

    return for (self.constants.items) |c| {
        if (c.type_id == type_id and std.mem.eql(c.value.many[0..wc], value.many[0..wc])) break c.id;
    } else blk: {
        const id = self.newID();
        self.constants.append(self.arena, .{
            .id = id,
            .type_id = type_id,
            .value = value,
        });
        break :blk id;
    };
}

const Constant = struct {
    id: WORD,
    type_id: WORD,
    value: ConstantValue,
};
const ConstantValue = union {
    single: WORD,
    many: [4]WORD,
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
            .component_type_id = try self.convertTypeID(.{ .scalar = vector.component }),
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
    return for (self.type_decls) |td| (if (td.id == id) break td.type) else unreachable;
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

    pub fn eql(a: Type, b: Type) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .float => |float| float == b.float,
            .int => |int| int.width == b.int.width and int.signed == b.int.signed,
            .vector => |vector| vector.len == b.vector.len and vector.component_type_id == b.vector.component_type_id,
            .function => |function| function.rtype_id == b.function.rtype_id and
                if (function.arg_type_ids.len == b.function.arg_type_ids.len)
                    (for (function.arg_type_ids, b.function.arg_type_ids) |a_arg, b_arg| (if (a_arg != b_arg) break false) else true)
                else
                    false,
            else => @panic("idk how to compare that type"),
        };
    }
    pub fn valueWordConsumption(self: Type) u32 {
        switch (self) {
            .float => |float| if (float == .long) 2 else 1,
            .int => |int| if (int.width == .long) 2 else 1,
            .vector => |vector| @intFromEnum(vector.len),
            .matrix => |matrix| @intFromEnum(matrix.column_count),
            else => 1,
        }
    }
};
const FunctionType = struct { rtype_id: WORD, arg_type_ids: []WORD = &.{} };

const MatrixType = struct { column_count: VectorLen, column_type_id: WORD };
const VectorType = struct { len: VectorLen, component_type_id: WORD };
const IntType = struct { width: BitWidth, signed: bool };
const BitWidth = Parser.tp.BitWidth;
const VectorLen = Parser.tp.VectorLen;
const TypeDeclaration = struct { type: Type, id: WORD };

const ExecutionModelInfo = Parser.ExecutionModelInfo;
const Expression = Parser.Expression;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;
const List = std.ArrayList;
