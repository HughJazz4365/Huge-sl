const std = @import("std");
const tp = @import("type.zig");
const util = @import("util.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub var p: *Parser = undefined;

pub fn formatStatement(statement: Statement, writer: *std.Io.Writer) !void {
    switch (statement) {
        .var_decl => |var_decl| {
            try writer.print("[RC:{d}]{s}", .{ var_decl.reference_count, @tagName(var_decl.qualifier) });
            switch (var_decl.qualifier) {
                .in, .out => try writer.print("({})", .{if (var_decl.qualifier == .in) var_decl.qualifier.in else var_decl.qualifier.out}),
                else => {},
            }
            try writer.print(" {s} : {f} = {f}", .{ var_decl.name, var_decl.type, var_decl.initializer });
        },
        .assignment => |ass| try writer.print("{f} = {f}", .{
            ass.target,
            ass.value,
        }),

        .@"return" => |returned| {
            try writer.print("return", .{});
            if (!returned.isEmpty()) try writer.print(" {f}", .{returned});
        },
        else => try writer.print("[{s}]", .{@tagName(statement)}),
    }
}

pub fn formatType(t: tp.Type, writer: *std.Io.Writer) !void {
    switch (t) {
        .entrypoint => |ep| try writer.print("entrypoint(.{s})", .{@tagName(ep)}),
        .scalar => |n| try writer.print("{s}{d}", .{ n.type.prefix(), @intFromEnum(n.width) }),
        .vector => |v| try writer.print("{s}vec{d}", .{
            if (v.component.type == .float) "" else v.component.type.prefix(),
            @intFromEnum(v.len),
        }),
        .unknown => |unknown| if (!unknown.isEmpty()) {
            try writer.print("{f}", .{unknown.*});
        } else try writer.print("@UnknownType", .{}),
        .function => |f| {
            try writer.print("fn(", .{});
            for (f.arg_types, 0..) |at, i| try writer.print("{f}{s}", .{ at, if (i + 1 < f.arg_types.len) ", " else "" });
            try writer.print("){f}", .{f.rtype.*});
        },
        .array => |array| try writer.print("[{d}]{f}", .{ array.len, array.component }),
        .matrix => |matrix| try writer.print("mat{d}x{d}", .{ matrix.m, matrix.n }),
        .@"struct" => |struct_id| _ = try writer.write(p.getStructFromID(struct_id).name),
        .buffer => |buffer| try writer.print("[BufferType{}]({f})", .{ buffer.type, Parser.Type{ .@"struct" = buffer.struct_id } }),

        .texture => |texture| try writer.print("[{s}TextureType]({f}, {})", .{
            if (texture.sampled) "Sampled" else "",
            Parser.Type{ .scalar = texture.texel_primitive },
            texture.type,
        }),
        else => try writer.print("{s}", .{@tagName(t)}),
    }
}

pub fn formatExpression(expr: Expression, writer: *std.Io.Writer) !void {
    switch (expr) {
        .enum_literal => |enum_literal| try writer.print(".{s}", .{enum_literal}),
        .identifier => |id| try writer.print("{s}", .{id}),
        .builtin => |builtin| switch (builtin) {
            inline else => |val| try writer.print("@{s}", .{@tagName(val)}),
        },
        .named_value => |named_value| _ = try writer.write(named_value.name),
        .value => |v| try writer.print("{f}", .{v}),
        .bin_op => |bin_op| try writer.print("({f} {s} {f})", .{ bin_op.left, @tagName(bin_op.op), bin_op.right }),
        .u_op => |u_op| try writer.print("{s}{f}", .{ @tagName(u_op.op), u_op.target }),
        .constructor => |constructor| {
            try writer.print("{f}{{ ", .{constructor.type});
            for (constructor.components, 0..) |component, i| {
                try writer.print("{f}{s}", .{
                    component,
                    if (i + 1 >= constructor.components.len) "" else ", ",
                });
            }
            try writer.print(" }}", .{});
        },
        .struct_constructor => |struct_constructor| {
            if (struct_constructor.type == .@"struct") {
                const s = p.getStructFromID(struct_constructor.type.@"struct");
                try writer.print("{s}{{\n", .{s.name});
            } else try writer.print("{f}{{\n", .{struct_constructor.type});
            for (struct_constructor.members) |elem| {
                try writer.print("\t.{s} = {f},\n", .{ elem.name, elem.expr });
            }
            try writer.print("}}", .{});
        },
        .cast => |cast| try writer.print("{f}{{ {f} }}", .{ cast.type, cast.expr.* }),
        .indexing => |indexing| try writer.print("{f}[{f}]", .{ indexing.target, indexing.index }),
        .call => |call| {
            try writer.print("{f}(", .{call.callee.*});
            for (call.args, 0..) |arg, i| try writer.print("{f}{s}", .{ arg, if (i + 1 < call.args.len) ", " else ")" });
        },
        .member_access => |member_access| try writer.print("{f}.{s}", .{ member_access.target.*, member_access.member_name }),
        inline else => |_, tag| try writer.print("{s}", .{@tagName(tag)}),
    }
}
pub fn formatValue(value: Parser.Value, writer: *std.Io.Writer) !void {
    switch (value.type) {
        .bool => try writer.print("{}", .{util.extract(bool, value.payload.wide)}),
        .compint => try writer.print("{d}", .{util.extract(Parser.CI, value.payload.wide)}),
        .compfloat => try writer.print("{d}f", .{util.extract(Parser.CF, value.payload.wide)}),
        .entrypoint => {
            const entry_point: *const Parser.EntryPoint = @ptrCast(@alignCast(value.payload.ptr));
            try writer.print("{f}{{\n", .{value.type});

            try writer.print("[local interfaces: ", .{});
            for (entry_point.io.items) |i| try writer.print("{s} ", .{i});
            try writer.print("]\n[global ios: {d}, local ios: {d}]\n", .{
                entry_point.global_io_count,
                entry_point.io.items.len,
            });

            for (entry_point.body.items) |statement| try writer.print("{f}\n", .{statement});
            try writer.print("}}\n", .{});
        },
        .scalar => |scalar| switch (scalar.type) {
            inline else => |st| switch (scalar.width) {
                inline else => |width| try writer.print("[{f}]|{d}|", .{
                    value.type,
                    util.extract((tp.Scalar{ .type = st, .width = width }).ToZig(), value.payload.wide),
                }),
            },
        },
        .@"struct" => |struct_id| {
            // try writer.print("STRUCT", .{});
            // if (true) return;
            const s = p.getStructFromID(struct_id);
            const members_ptr = @as([*]const Expression, @ptrCast(@alignCast(value.payload.ptr)));

            try writer.print("|{s}|{{\n", .{s.name});
            for (members_ptr[0..s.members.items.len], s.members.items) |v, m| {
                try writer.print("\t.{s} = {f},\n", .{ m.name, v });
            }
            try writer.print("}}", .{});
        },
        .vector => |vector| switch (vector.len) {
            inline else => |len| switch (vector.component.width) {
                inline else => |width| switch (vector.component.type) {
                    inline else => |num_type| try writer.print("|{f}|{d}", .{
                        value.type,
                        @as(*const (tp.Vector{ .len = len, .component = .{ .width = width, .type = num_type } }).ToZig(), @ptrCast(@alignCast(value.payload.ptr))).*,
                    }),
                },
            },
        },
        .@"enum" => |@"enum"| try writer.print("[enum].{s}", .{for (@"enum".fields) |ef| (if (util.extract(u64, value.payload.wide) == ef.value) break ef.name) else "?"}),
        .type => if (value.payload.type == .unknown)
            try writer.print("[EMPTY]", .{})
        else if (value.payload.type == .@"struct") {
            const s = p.getStructFromID(value.payload.type.@"struct");

            try writer.print("struct[{s}]{{\n", .{s.name});
            for (s.members.items) |m| try writer.print("[.{s} : {f} = {f}]\n", .{ m.name, m.type, m.default_value });
            if (s.body.items.len > 0) _ = try writer.write("\n");
            for (s.body.items) |statement| try writer.print("{f}\n", .{statement});
            try writer.print("}}\n", .{});
        } else try writer.print("{f}", .{value.payload.type}),
        .function => {
            const func: *const Parser.Function = @ptrCast(@alignCast(value.payload.ptr));
            if (func.builtin_handle) |bh| {
                try writer.print("FN[@{s}]", .{@tagName(bh)});
                return;
            }
            try writer.print("{f}{{\n", .{value.type});

            for (func.body.items, 0..) |statement, i|
                if (i < 2) (try writer.print("{f}\n", .{statement})) else {
                    try writer.print("...\n", .{});
                    break;
                };
            try writer.print("}}\n", .{});
        },
        .array => |array| {
            try writer.print("|{f}|{{", .{value.type});
            const payloads: [*]const Parser.ValuePayload = @ptrCast(@alignCast(value.payload.ptr));
            for (0..array.len) |i| try writer.print("{f}{s}", .{
                Parser.Value{ .type = array.component.*, .payload = payloads[i] },
                if (i + 1 == array.len) "" else ", ",
            });
            try writer.print("}}", .{});
        },
        .unknown, .void => try writer.print("[EMPTY]", .{}),

        else => try writer.print("[{s}]", .{@tagName(value.type)}),
    }
}
pub fn formatToken(token: Token, writer: *std.Io.Writer) !void {
    switch (token) {
        .identifier => |id| try writer.print("[id]: {s}", .{id}),
        .type_literal => |tl| try writer.print("[type_literal]: {f}", .{tl}),
        .builtin => |b| try writer.print("[builtin]: @{s}", .{b}),

        inline else => |value, tag| try writer.print("[{s}]: {any}", .{ @tagName(tag), value }),
    }
}
const Expression = Parser.Expression;
const Statement = Parser.Statement;
const Token = Parser.Token;
