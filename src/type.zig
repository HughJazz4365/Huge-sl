const std = @import("std");
const Parser = @import("Parser.zig");
const bi = @import("builtin.zig");

pub fn typeOf(self: *Parser, expr: Expression) Error!Type {
    const result: Type = switch (expr) {
        .value => |value| value.type,
        .constructor => |constructor| constructor.type,
        .cast => |cast| cast.type,
        .identifier => |identifier| (try self.current_scope.getVariableReference(self, identifier)).type,
        .builtin => |builtin| try bi.typeOfBuiltin(self, builtin),
        .bin_op => |bin_op| blk: {
            const left_type = try self.typeOf(bin_op.left.*);
            const right_type = try self.typeOf(bin_op.right.*);
            if (left_type == .unknown or right_type == .unknown) return .unknownempty;
            break :blk switch (bin_op.op) {
                .@"+", .@"-", .@"^", .@"|", .@"&", .@">>" => left_type,
                .@"*" => clk: {
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
    // std.debug.print("T: {f}, E: {f}\n", .{ result, expr });
    return result;
}
pub const Type = union(enum) {
    //dont change the order!
    void,
    type,
    bool,

    //intermediate compile time types
    unknown: *const Expression,

    compint,
    compfloat,

    scalar: Scalar,
    vector: Vector,
    array: Array,
    matrix: Matrix,

    @"struct",
    buffer,
    image,

    @"enum",

    function: FunctionType,
    entrypoint: ExecutionModel,

    pub const unknownempty: Type = .{ .unknown = &Expression.empty };
    pub const format = @import("debug.zig").formatType;

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
            else => null,
        };
    }
    pub fn size(self: Type) usize {
        return switch (self) {
            .scalar => |number| @intFromEnum(number.width) >> 3,
            .vector => |vector| (Type.size(.{ .scalar = vector.component }) * @intFromEnum(vector.len) + 15) / 16 * 16,
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
pub const ConstructorStructure = struct {
    component: Type,
    len: u32 = 1,
};

pub const FunctionType = struct {
    rtype: *const Type,
    arg_types: []const Type,
};

pub const ExecutionModel = enum { vertex, fragment, compute };

pub const Matrix = struct {
    m: VectorLen,
    n: VectorLen,
    width: BitWidth,
    pub fn columnVector(self: Matrix) Vector {
        return .{ .len = self.m, .component = .{ .type = .float, .width = self.width } };
    }
};
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
            for (VectorLen.allVectorLengths) |len|
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
pub const VectorLen = enum(u8) {
    _2 = 2,
    _3 = 3,
    _4 = 4,
    pub const allVectorLengths = blk: {
        var slice: []const VectorLen = &.{};
        for (@typeInfo(VectorLen).@"enum".fields) |ef|
            slice = slice ++ &[1]VectorLen{@enumFromInt(ef.value)};
        break :blk slice;
    };
};

pub const Scalar = struct {
    type: ScalarType,
    width: BitWidth,

    pub fn ToZig(comptime num: Scalar) type {
        const width = @intFromEnum(num.width);
        return @Type(
            if (num.type == .float)
                .{ .float = .{ .bits = width } }
            else
                .{ .int = .{ .bits = width, .signedness = if (num.type == .int) .signed else .unsigned } },
        );
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
pub const BitWidth = enum(u8) { short = 16, word = 32, long = 64 };

const Expression = Parser.Expression;
const Error = Parser.Error;
