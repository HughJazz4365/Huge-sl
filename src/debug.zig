const std = @import("std");
const tp = @import("type.zig");
const util = @import("util.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub var p: *Parser = undefined;

pub fn formatToken(token: Token, writer: *Writer) !void {
    switch (token) {
        .identifier => |id| try writer.print("id[{s}]", .{id}),
        .type_literal => |tl| try writer.print("Type[{any}]", .{tl}),
        .int_literal => |il| try writer.print("int[{d}]", .{il}),
        .float_literal => |fl| try writer.print("float[{d}]", .{fl}),
        // .builtin => |b| try writer.print("'builtin': @{s}", .{b}),

        inline else => |value, tag| try writer.print("{s}[{any}]", .{ @tagName(tag), value }),
    }
}
pub fn formatStatement(statement: Statement, writer: *Writer) !void {
    switch (statement) {
        .var_decl => |var_decl| {
            try writer.print("{s}", .{@tagName(var_decl.qualifier)});
            if (var_decl.qualifier == .in or var_decl.qualifier == .out) {
                const interpolation = if (var_decl.qualifier == .in)
                    var_decl.qualifier.in
                else
                    var_decl.qualifier.out;
                try writer.print("({})", .{interpolation});
            }
            try writer.print(" {s}: {f}", .{ var_decl.name, var_decl.type });
            if (var_decl.initializer != .null)
                try writer.print(" = {f}", .{var_decl.initializer});
        },
        .assignment => |assigment| //
        try writer.print("{f} = {f}", .{ assigment.target, assigment.value }),
        .ignore => |ignore| try writer.print("{f}", .{ignore}),
        .block => |block| {
            try writer.print(":{{\n", .{});
            for (p.getBlock(block).body.items) |s|
                try writer.print("{f}\n", .{s});
            try writer.print("}}", .{});
        },
        .@"return" => |ret| if (ret == .null)
            try writer.print("return", .{})
        else
            try writer.print("return {f}", .{ret}),

        .empty => try writer.print("{{}}", .{}),
        else => try writer.print("{s}\n", .{@tagName(statement)}),
    }
}
pub fn formatType(@"type": Parser.Type, writer: *Writer) !void {
    switch (@"type") {
        .scalar => |scalar| try writer.print("{c}{s}", .{
            @tagName(scalar.layout)[0],
            @tagName(scalar.width)[1..],
        }),
        .vector => |vector| try writer.print("{f}x{d}", .{ Type{ .scalar = vector.component }, @intFromEnum(vector.len) }),
        .unknown => |unknown| try writer.print("{f}", .{unknown.*}),
        .type_of => |type_of| try writer.print("@TypeOf({f})", .{type_of.*}),
        .entry_point => |entry_point| try writer.print("entrypoint(.{s})", .{@tagName(entry_point)}),
        else => try writer.print("{s}", .{@tagName(@"type")}),
    }
}
pub fn formatExpression(expr: Parser.Expression, writer: *Writer) !void {
    switch (expr) {
        .identifier => |identifier| try writer.print("\"{s}", .{identifier}),
        .var_ref => |var_ref| try writer.print("'{s}", .{var_ref.name}),
        .value => |value| try writer.print("{f}", .{value}),
        .null => try writer.print("[NULL]", .{}),
        .bin_op => |bin_op| try writer.print("({f} {s} {f})", .{ bin_op.left.*, @tagName(bin_op.op), bin_op.right.* }),
        .branch => |branch| {
            try writer.print("if({f}) {f}", .{ branch.condition.*, branch.true.* });
            if (branch.false.* != .empty) try writer.print(" else {f}", .{branch.false.*});
        },
        else => try writer.print("expr={any}", .{expr}),
    }
}
pub fn formatValue(value: Value, writer: *Writer) !void {
    switch (value.type) {
        .compfloat => try writer.print("{}", .{value.payload.get(.compfloat)}),
        .compint => try writer.print("{}", .{value.payload.get(.compint)}),
        .bool => try writer.print("{}", .{value.payload.get(.bool)}),
        .type => try writer.print("{f}", .{value.payload.get(.type)}),
        .scalar => |scalar| inline for (&tp.Scalar.all) |s| {
            if (std.meta.eql(scalar, s))
                try writer.print("{f}[{d}]", .{ value.type, value.payload.getFromType(.{ .scalar = s }) });
        },
        .entry_point => |entry_point| {
            try writer.print("{f}{{\n", .{Type{ .entry_point = entry_point }});
            for (p.getEntryPoint(value.payload.get(.entry_point)).scope.body.items) |s| {
                try writer.print("{f}\n", .{s});
            }
            try writer.print("}}", .{});
        },
        else => try writer.print("value={any}", .{value}),
    }
}

const Type = tp.Type;
const Value = Parser.Value;
const Statement = Parser.Statement;
const Token = Tokenizer.Token;
const Writer = std.Io.Writer;
