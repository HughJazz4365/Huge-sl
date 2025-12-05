const std = @import("std");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
const hgsl = @import("root.zig");
const bi = @import("builtin.zig");

pub fn typeOf(self: *Parser, expr: Expression) Error!Type {
    return switch (expr) {
        .value => |value| value.type,
        .named_value => |named_value| named_value.value.type,
        .constructor => |constructor| constructor.type,
        .struct_constructor => |struct_constructor| struct_constructor.type,
        .cast => |cast| cast.type,
        .identifier => |identifier| (try self.current_scope.getVariableReference(self, identifier)).type,
        .member_access => |member_access| switch (try self.typeOf(member_access.target.*)) {
            .@"struct" => |s| try self.getStructFromID(s).getMemberType(member_access.member_name),
            .buffer => |buffer| try self.getStructFromID(buffer.struct_id).getMemberType(member_access.member_name),
            .type => @panic("member access type on 'type'"),
            .texture => |texture| if (util.strEql(member_access.member_name, Texture.sample_function_name))
                .{ .function = texture.createSampleFunctionType(null) catch unreachable }
            else
                return Error.InvalidMemberAccess,

            else => return self.errorOut(Error.InvalidMemberAccess),
        },
        .builtin => |builtin| try bi.typeOfBuiltin(self, builtin),
        .bin_op => |bin_op| blk: {
            const left_type = try self.typeOf(bin_op.left.*);
            const right_type = try self.typeOf(bin_op.right.*);

            if (left_type == .unknown or right_type == .unknown) return .unknownempty;
            break :blk switch (bin_op.op) {
                .@"+", .@"-", .@"^", .@"|", .@"&", .@">>", .@"/" => left_type,
                .@"*" => clk: {
                    if (left_type == .matrix and right_type == .matrix)
                        break :clk .{ .matrix = .{ .m = left_type.matrix.m, .n = right_type.matrix.n, .width = left_type.matrix.width } }
                    else if (left_type == .matrix and right_type == .vector)
                        break :clk .{ .vector = left_type.matrix.columnVector() }
                    else if (left_type == .vector and right_type == .matrix)
                        break :clk .{ .vector = .{ .len = right_type.matrix.n, .component = left_type.vector.component } };

                    var deep = left_type;
                    var shallow = right_type;
                    if (shallow.depth() > deep.depth()) std.mem.swap(Type, &deep, &shallow);

                    const result: Type = if ((deep == .matrix and shallow == .scalar) or
                        (deep == .vector and shallow == .scalar) or
                        Type.eql(shallow, deep))
                        deep
                    else if (deep == .matrix and shallow == .vector) shallow else .unknownempty;

                    break :clk result;
                },
                .@"***", .@"**" => if (left_type == .vector and Type.eql(left_type, right_type)) left_type.constructorStructure().component else .unknownempty,

                else => @panic("unknown bin op type"),
            };
        },
        .u_op => |u_op| switch (u_op.op) {
            else => try self.typeOf(u_op.target.*),
        },
        .indexing => |indexing| (try self.typeOf(indexing.target.*)).constructorStructure().component,
        .call => |call| blk: {
            if (call.callee.* == .builtin)
                break :blk try bi.typeOfBuiltInCall(self, call.callee.builtin.function, call.args);
            break :blk switch (try self.typeOf(call.callee.*)) {
                .function => |function| function.rtype.*,
                .unknown => .unknownempty,
                else => return Error.InvalidCall,
            };
        },
        else => .{ .unknown = try self.createVal(expr) },
    };
}
pub const Type = union(enum) {
    void,
    type,
    bool,

    unknown: *const Expression,

    compint,
    compfloat,

    scalar: Scalar,
    vector: Vector,
    array: Array,
    matrix: Matrix,

    @"struct": Parser.StructID, //value.payload - []Expression
    buffer: Buffer,
    texture: Texture,

    image,

    @"enum": Enum,

    function: FunctionType,
    entrypoint: Parser.Stage,

    pub const _type: Type = .{ .type = {} };
    pub const _void: Type = .{ .void = {} };
    pub const _bool: Type = .{ .bool = {} };

    pub const unknownempty: Type = .{ .unknown = &Expression.empty };
    pub const format = @import("debug.zig").formatType;

    pub fn refine(self: Type, parser: *Parser) Error!Type {
        return if (self == .unknown) try parser.asType(self.unknown) else self;
    }
    pub fn isIndexable(self: Type) bool {
        return self.constructorStructure().len > 0 or self == .array;
    }
    pub fn isBindable(self: Type) bool {
        return switch (self) {
            .array => |array| array.component.isDescriptor(),
            else => self.isDescriptor(),
        };
    }
    pub fn isDescriptor(self: Type) bool {
        return self == .buffer or self == .texture;
    }

    pub fn valuePayloadType(self: Type) enum { ptr, wide, type } {
        return switch (self) {
            .type => .type,
            .scalar, .compint, .compfloat, .bool => .wide,
            else => .ptr,
        };
    }
    pub fn isComptimeOnly(self: Type) bool {
        return switch (self) {
            .compint, .compfloat, .void, .type, .unknown, .function, .entrypoint => true,
            else => false,
        };
    }
    pub fn isEmpty(self: Type) bool {
        return self == .unknown and self.unknown.isEmpty();
    }
    pub fn eql(a: Type, b: Type) bool {
        return std.meta.eql(a, b);
    }
    pub fn ToZig(comptime @"type": Type) type {
        return switch (@"type") {
            inline else => |value| if (@hasDecl(@TypeOf(value), "ToZig")) value.ToZig() else @compileError("cant convert type to zig"),
        };
    }
    pub fn isNumber(self: Type) bool {
        return self == .compfloat or self == .compint or self == .scalar;
    }
    pub fn numberRestrictiveness(self: Type) u3 {
        return switch (self) {
            .compint => 1,
            .compfloat => 2,
            .scalar => 3,
            else => 0,
        };
    }
    pub fn depth(self: Type) u32 {
        return switch (self) {
            .vector => 1,
            .matrix => 2,
            else => 0,
        };
    }
    pub fn scalarPrimitive(self: Type) ?Scalar {
        return switch (self) {
            .scalar => |scalar| scalar,
            .vector => |vector| vector.component,
            .matrix => |matrix| .{ .type = .float, .width = matrix.width },
            .array => |array| array.component.scalarPrimitive(),
            else => null,
        };
    }
    pub fn shouldReferenceCopyPayload(self: Type) bool {
        return switch (self) {
            .@"struct" => false,
            else => true,
        };
    }
    pub fn valuePayloadPtrSize(self: Type) usize {
        return switch (self) {
            .@"struct" => @sizeOf([*]Expression),
            else => self.size(),
        };
    }
    pub fn size(self: Type) usize {
        return switch (self) {
            .scalar => |number| @intFromEnum(number.width) >> 3,
            .vector => |vector| util.rut(usize, Type.size(.{ .scalar = vector.component }) * @intFromEnum(vector.len), 16),
            .array => |array| array.component.size() * array.len,
            .matrix => |matrix| Type.size(.{ .vector = matrix.columnVector() }) * @intFromEnum(matrix.n),
            .bool => 1,
            else => 0,
        };
    }
    pub fn constructorStructure(self: Type) ConstructorStructure {
        return switch (self) {
            .vector => |vector| .{ .component = .{ .scalar = vector.component }, .len = @intFromEnum(vector.len) },
            .array => |array| .{ .component = array.component.*, .len = array.len },
            .matrix => |matrix| .{ .component = .{ .vector = matrix.columnVector() }, .len = @intFromEnum(matrix.n) },
            .unknown => .{ .component = .unknownempty, .len = 1 },
            else => .{ .component = self },
        };
    }
    pub fn asExpr(self: Type) Expression {
        return .{ .value = .{ .type = .type, .payload = .{ .type = self } } };
    }
};
pub const Texture = struct {
    type: TextureType = ._2d,
    texel_primitive: Scalar,
    sampled: bool,
    pub const arg_names: []const []const u8 = &.{
        "sampler",
        "coord",
        "array_index",
    };

    pub const sample_function_name: []const u8 = "sample";
    pub fn getFunction(function_type: FunctionType) Parser.Function {
        return .{
            .arg_names = arg_names[0..function_type.arg_types.len],
            .type = function_type,
            .builtin_handle = .sample,
        };
    }

    pub fn createSampleFunctionType(self: Texture, allocator: ?std.mem.Allocator) error{OutOfMemory}!FunctionType {
        const array_index_type: Type = .{ .scalar = .{ .type = .uint, .width = .word } };
        const rtype: Type = .{ .vector = .{ .len = ._4, .component = self.texel_primitive } };
        const arg_types = ([3]Type{
            .{ .texture = self },
            switch (self.type) {
                .cube, .cube_array, ._3d => .{ .vector = .{ .len = ._3, .component = .{ .type = .float, .width = .word } } },
                ._2d, ._2d_array => .{ .vector = .{ .len = ._2, .component = .{ .type = .float, .width = .word } } },
                ._1d, ._1d_array => .{ .scalar = .{ .type = .float, .width = .word } },
            },
            array_index_type,
        })[0..switch (self.type) {
            .cube_array, ._1d_array, ._2d_array => 3,
            else => 2,
        }];
        return .{
            .rtype = if (allocator) |a| blk: {
                const alloc = try a.create(Type);
                alloc.* = rtype;
                break :blk alloc;
            } else &rtype,
            .arg_types = if (allocator) |a| try a.dupe(Type, arg_types) else arg_types,
        };
    }
};
pub const TextureType = enum {
    _1d,
    _2d,
    _3d,
    cube,
    _1d_array,
    _2d_array,
    cube_array,
};

pub const Buffer = struct { struct_id: Parser.StructID, type: BufferType };
pub const BufferType = enum { ubo, ssbo };

pub const Enum = struct {
    tag_type: EnumTag = .{ .width = .word, .signed = false },
    fields: []const EnumField,
    pub fn fromZig(EnumT: type) Enum {
        const tinfo = @typeInfo(EnumT).@"enum";
        var fields: []const EnumField = &.{};
        inline for (tinfo.fields) |ef| {
            fields = fields ++ &[1]EnumField{.{ .name = ef.name, .value = ef.value }};
        }
        return .{ .fields = fields };
    }
};
pub const EnumTag = struct { width: BitWidth, signed: bool };
pub const EnumField = struct { name: []const u8, value: u64 };

pub const ConstructorStructure = struct {
    component: Type,
    len: u32 = 1,
};

pub const FunctionType = struct {
    rtype: *const Type,
    arg_types: []const Type,
};

pub const Matrix = struct {
    m: VectorLen,
    n: VectorLen,
    width: BitWidth,
    pub fn columnVector(self: Matrix) Vector {
        return .{ .len = self.m, .component = .{ .type = .float, .width = self.width } };
    }
    pub const allMatrixTypes = blk: {
        var slice: []const Matrix = &.{};
        for (allVectorLengths) |m|
            for (allVectorLengths) |n|
                for (@typeInfo(BitWidth).@"enum".fields) |w| {
                    const width: BitWidth = @enumFromInt(w.value);
                    if (width != .word) continue;
                    slice = slice ++ &[1]Matrix{.{ .m = m, .n = n, .width = width }};
                };
        break :blk slice;
    };
    pub fn toLiteral(comptime mat: Matrix) []const u8 {
        if (mat.width != .word) @compileError(std.fmt.comptimePrint(
            "no builtin literal for matrix with component bitwidth of {d}",
            .{mat.width},
        ));

        return "mat" ++ .{'0' + @intFromEnum(mat.m)} ++ "x" ++ .{'0' + @intFromEnum(mat.n)};
    }
};
pub const ArrayValue = [*]Parser.ValuePayload;
pub const Array = struct {
    component: *const Type,
    len: u32,
    pub fn ToZig(comptime arr: Array) type {
        return [arr.len](arr.component.*).ToZig();
    }
};

pub const Vector = struct {
    component: Scalar,
    len: VectorLen,
    pub fn ToZig(comptime vec: Vector) type {
        return @Vector(@intCast(@intFromEnum(vec.len)), vec.component.ToZig());
    }
    pub const allVectorTypes = blk: {
        var slice: []const Vector = &.{};
        for (Scalar.allScalarTypes) |component| {
            if (component.width != .word) continue;
            for (allVectorLengths) |len|
                slice = slice ++ &[1]Vector{.{ .component = component, .len = len }};
        }
        break :blk slice;
    };
    pub fn toLiteral(comptime vec: Vector) []const u8 {
        if (vec.component.width != .word) @compileError(std.fmt.comptimePrint(
            "no builtin literal for vector with component bitwidth of {d}",
            .{vec.component.width},
        ));
        return switch (vec.component.type) {
            .float => "",
            .int => "i",
            .uint => "u",
        } ++ "vec" ++ .{'0' + @intFromEnum(vec.len)};
    }
};
pub const VectorLen = hgsl.VectorLen;
pub const allVectorLengths = blk: {
    var slice: []const VectorLen = &.{};
    for (@typeInfo(VectorLen).@"enum".fields) |ef|
        slice = slice ++ &[1]VectorLen{@enumFromInt(ef.value)};
    break :blk slice;
};

pub const Scalar = struct {
    type: ScalarType,
    width: BitWidth,

    pub fn ToZig(comptime num: Scalar) type {
        const width = @intFromEnum(num.width);
        return if (num.type == .float)
            std.meta.Float(width)
        else
            @Int(if (num.type == .int) .signed else .unsigned, width);
    }
    pub const allScalarTypes = blk: {
        var slice: []const Scalar = &.{};
        for (@typeInfo(ScalarType).@"enum".fields) |nef| {
            for (@typeInfo(BitWidth).@"enum".fields) |wef| {
                const n: Scalar = .{ .type = @enumFromInt(nef.value), .width = @enumFromInt(wef.value) };
                slice = slice ++ &[1]Scalar{n};
            }
        }
        break :blk slice;
    };
    pub fn toLiteral(comptime num: Scalar) []const u8 {
        comptime var literal: []const u8 = &.{};
        literal = literal ++ comptime num.type.prefix() ++ switch (num.width) {
            .short => "16",
            .word => "32",
            .long => "64",
        };
        return literal;
    }
    pub fn eql(a: Scalar, b: Scalar) bool {
        return a.width == b.width and a.type == b.type;
    }
};
pub const ScalarType = enum {
    float,
    int,
    uint,
    pub fn prefix(num_type: ScalarType) []const u8 {
        return switch (num_type) {
            .float => "f",
            .int => "i",
            .uint => "u",
        };
    }
};
pub const u32_type: Type = .{ .scalar = .{ .type = .uint, .width = .word } };
pub const f32_type: Type = .{ .scalar = .{ .type = .float, .width = .word } };
pub const vec3_type: Type = .{ .vector = .{ .len = ._3, .component = .{ .type = .float, .width = .word } } };
pub const vec4_type: Type = .{ .vector = .{ .len = ._4, .component = .{ .type = .float, .width = .word } } };

pub const BitWidth = enum(u32) { short = 16, word = 32, long = 64 };

const Expression = Parser.Expression;
const Error = Parser.Error;
