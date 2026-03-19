const std = @import("std");
const util = @import("util.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");
//debug structs for formatting

pub const DebugType = struct {
    self: *Parser,
    type: Type,
    pub fn format(entry: DebugType, writer: *std.Io.Writer) !void {
        if (entry.type == .@"anytype")
            return try writer.writeAll("anytype");

        const te = entry.self.getTypeEntry(entry.type);
        switch (te) {
            .function => |ftype| {
                try writer.print("fn(", .{});
                for (0..ftype.parameter_count) |i| try writer.print("{f}{s}", .{
                    DebugType{
                        .self = entry.self,
                        .type = entry.self.function_type_elems.items[ftype.id + i],
                    },
                    if (i + 1 == ftype.parameter_count) "" else ", ",
                });
                try writer.print(") {f}", .{DebugType{
                    .self = entry.self,
                    .type = entry.self.function_type_elems.items[ftype.id + ftype.parameter_count],
                }});
            },
            .pointer => |pointed| try writer.print("*{f}", .{DebugType{ .self = entry.self, .type = pointed }}),
            else => try TypeEntry.format(te, writer),
        }
    }
};
const DebugValue = struct {
    self: *Parser,
    value: Parser.Value,
    pub fn format(entry: DebugValue, writer: *std.Io.Writer) !void {
        const type_entry = entry.self.getTypeEntry(entry.value.type);
        switch (type_entry) {
            .compint => try writer.print("{d}", .{
                util.extract(i128, entry.self.getNumberValue(entry.value.payload)),
            }),
            .bool => try writer.print("{}", .{util.extract(bool, entry.self.getNumberValue(entry.value.payload))}),
            .type => try writer.print("{f}", .{DebugType{
                .self = entry.self,
                .type = @enumFromInt(entry.value.payload),
            }}),
            .scalar => |scalar| switch (scalar.layout) {
                inline else => |sl| switch (scalar.width) {
                    inline else => |sw| {
                        const T = (TypeEntry.Scalar{ .layout = sl, .width = sw }).ToZig();
                        try writer.print("{s}[{}]", .{
                            @typeName(T),
                            util.extract(T, entry.self.getNumberValue(entry.value.payload)),
                        });
                    },
                },
            },
            .function => {
                const function_entry = entry.self.getFunctionEntry(@enumFromInt(entry.value.payload));
                _ = function_entry;
                try writer.print("FN'{d}'", .{entry.value.payload});
            },
            .vector => |vector| switch (vector.len) {
                inline else => |len| switch (vector.scalar.layout) {
                    inline else => |sl| switch (vector.scalar.width) {
                        inline else => |sw| {
                            const uvec = entry.self.getVectorList(sw.value()).items[entry.value.payload];

                            const T = (TypeEntry.Scalar{ .layout = sl, .width = sw }).ToZig();
                            try writer.print("{s}x{}[", .{ @typeName(T), vector.len.value() });
                            inline for (0..comptime len.value()) |i| {
                                try writer.print("{d}", .{util.extract(T, uvec[i])});
                                try writer.writeAll(if (i + 1 >= len.value()) "]" else ", ");
                            }
                        },
                    },
                },
            },
            else => {},
        }
    }
};
pub const DebugNodeU = struct {
    self: *Parser,
    scope: Scope,
    node: Node,

    pub fn format(entry: DebugNodeU, writer: *std.Io.Writer) !void {
        var node = entry.node;
        try (DebugNode{ .self = entry.self, .scope = entry.scope, .node = &node }).format(writer);
    }
};
pub const DebugNode = struct {
    self: *Parser,
    scope: Scope,
    node: *Node,
    pub fn format(entry: DebugNode, writer: *std.Io.Writer) !void {
        const scope = entry.scope;
        const body = entry.self.getScopeEntry(scope).body.items;
        switch (body[entry.node.*]) {
            .@"return" => {
                entry.node.* += 1;
                try writer.print("return {f}", .{entry});
            },
            .constructor => |constructor| {
                entry.node.* += 1;
                try writer.print("{f}{{", .{entry});

                for (0..constructor.elem_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == constructor.elem_count) "" else ", " });
                try writer.print("}}", .{});
            },
            .indexing => {
                entry.node.* += 1;
                try writer.print("{f}[{f}]", .{ entry, entry });
            },
            .call => |function_call| {
                entry.node.* += 1;
                try writer.print("{f}(", .{entry});
                for (0..function_call.argument_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == function_call.argument_count) "" else ", " });
                try writer.print(")", .{});
            },
            .function_permutation => {
                entry.node.* += 1;
                try writer.writeAll("FNPERM");
            },
            .function_type_declaration => |fn_type_decl| {
                entry.node.* += 1;
                try writer.print("fn(", .{});

                for (0..fn_type_decl.parameter_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == fn_type_decl.parameter_count) "" else ", " });
                try writer.print(") {f}", .{entry});
            },
            .function_declaration => |fn_decl| {
                entry.node.* += 1;
                const parameter_count = entry.self.getFunctionEntry(fn_decl.function).parameter_count;
                try writer.print("fn (", .{});

                for (0..parameter_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == parameter_count) "" else ", " });
                try writer.print(") {f}{{\n", .{entry});

                const body_scope = entry.self.getFunctionEntry(fn_decl.function).scope;

                entry.self.dumpScope(body_scope, false, true);
            },
            .function_parameter => |fn_param| {
                entry.node.* += 1;
                try writer.print("{s}{s}: {f}", .{
                    if (fn_param.qualifier == .none) "" else switch (fn_param.qualifier) {
                        inline else => |tag| @tagName(tag) ++ " ",
                    },
                    entry.self.tokenizer.slice(fn_param.name),
                    entry,
                });
            },
            .assignment => {
                entry.node.* += 1;
                try writer.print("{f} = {f}", .{ entry, entry });
            },
            .variable_declaration, .folded_variable_declaration => |var_decl| {
                if (body[entry.node.*] == .folded_variable_declaration) try writer.writeByte('*');
                entry.node.* += 1;
                try writer.print("{s}", .{@tagName(var_decl.qualifier)});
                if (var_decl.qualifier == .compute) {
                    try writer.print("({f})", .{entry});
                } else entry.node.* += 1;
                try writer.print(" {s}: {f} = {f}", .{
                    entry.self.tokenizer.slice(var_decl.name),
                    entry,
                    entry,
                });
            },
            .bin_op => |bin_op| {
                entry.node.* += 1;
                try writer.print("({f} <{s}> {f})", .{
                    entry,
                    @tagName(bin_op.op),
                    entry,
                });
            },
            .u_op => |u_op| {
                entry.node.* += 1;
                try writer.print("<{s}>{f}", .{ @tagName(u_op.op), entry });
            },
            .value => |value| {
                try writer.print("{f}", .{DebugValue{ .self = entry.self, .value = value }});
                entry.node.* += 1;
            },
            .identifier => |identifier| {
                try writer.print("\"{s}", .{entry.self.tokenizer.slice(identifier)});
                entry.node.* += 1;
            },
            .builtin => |builtin_node| {
                try writer.print("@{s}", .{@tagName(builtin_node.builtin)});
                entry.node.* += 1;
            },
            .variable_reference => |var_ref| {
                defer entry.node.* += 1;
                switch (entry.self.getNodeEntry(var_ref.scope, var_ref.node).*) {
                    inline .function_declaration, .function_permutation_header => {
                        const param_node = var_ref.node + 1 + entry.self.nodeSequenceConsumption(
                            var_ref.scope,
                            var_ref.node + 1,
                            var_ref.value,
                        );
                        const name = switch (entry.self.getNodeEntry(var_ref.scope, param_node).*) {
                            inline .function_parameter, .function_permutation_parameter => |p| p.name,
                            else => unreachable,
                        };
                        try writer.print("fp'{s}", .{entry.self.tokenizer.slice(name)});
                    },
                    inline .folded_variable_declaration, .variable_declaration => |vd| try writer.print("'{s}", .{entry.self.tokenizer.slice(vd.name)}),
                    else => unreachable,
                }
            },
            .null => {
                entry.node.* += 1;
                try writer.print("<null>", .{});
            },
            .@"anytype" => {
                entry.node.* += 1;
                try writer.print("anytype", .{});
            },

            else => {
                entry.node.* += 1;
                if (entry.node.* < body.len)
                    try writer.print("{f}", .{entry});
            },
        }
    }
};
pub const DebugToken = struct {
    self: Tokenizer,
    token: Token,
    pub fn format(entry: DebugToken, writer: *std.Io.Writer) !void {
        const token_kind = entry.self.kind(entry.token);
        try writer.print("'{s}'", .{@tagName(token_kind)});
        switch (token_kind) {
            .identifier,
            .int_literal,
            .float_literal,
            .type_literal,
            => try writer.print(" : \"{s}\"", .{entry.self.slice(entry.token)}),
            else => {},
        }
    }
};

const Type = Parser.Type;
const Node = Parser.Node;
const TypeEntry = Parser.TypeEntry;
const Token = Parser.Token;
const Scope = Parser.Scope;
