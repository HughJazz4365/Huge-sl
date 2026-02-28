//bin op assignments( a *= b, c -= 1 )
//TODO: valid_...(valid_var_decl) node entry variations for semantically analyzed
//statements (fold on them would return immediately)
//TODO: function variants
//TODO: comptime function full/partial dispatch
//TODO: @import("i.hgsl")

const std = @import("std");
const util = @import("util.zig");
const zigbuiltin = @import("builtin");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");
const error_message = @import("errorMessage.zig");
const hgsl = @import("root.zig");

tokenizer: Tokenizer = undefined,
allocator: Allocator,

types: List(TypeEntry) = .empty,

scalar_values: List(u128) = .empty,
composite_values: List(u8) = .empty,

functions: List(FunctionEntry) = .empty,
structs: List(StructEntry) = .empty,

scopes: List(ScopeEntry) = .empty,
current_scope: Scope = .entry_file,

token: Token = 0,

error_info: ErrorInfo = .unknown,

pub fn dump(self: *Parser) void {
    std.debug.print("types:\n", .{});
    for (self.types.items) |t| std.debug.print("--- {any}\n", .{t});
    // std.debug.print("structs:\n", .{});
    // for (self.structs.items) |t| std.debug.print("--- {any}\n", .{t});

    std.debug.print("---BODY: \n", .{});
    self.current_scope = .entry_file;
    self.dumpCurrentScope(false, true);

    // if (true) //
    if (false) //
        for (1..self.scopes.items.len) |i| {
            const s: Scope = @enumFromInt(i);
            self.current_scope = s;
            std.debug.print("SCOPE[{d}], nodes: {d}\n", .{ i, self.getScope(s).body.items.len });
            self.dumpCurrentScope(true, false);
        };
}

fn dumpCurrentScope(self: *Parser, nodes: bool, formatted: bool) void {
    const body = self.getBodySlice();
    if (nodes)
        for (body) |node| std.debug.print("\t{any}\n", .{node});
    if (formatted) {
        var node: Node = 0;
        while (node < body.len) {
            std.debug.print("\t{f}\n", .{FatNode{
                .self = self,
                .node = &node,
            }});
        }
    }
}

pub fn new(allocator: Allocator) Error!Parser {
    const types_initial_capacity = 16;
    const scalar_values_initial_capacity = 16;
    const composite_values_initial_capacity = 8 * 4 * 4;
    var self: Parser = .{
        .allocator = allocator,
    };

    self.types = try .initCapacity(allocator, types_initial_capacity);
    self.scalar_values = try .initCapacity(allocator, scalar_values_initial_capacity);
    self.composite_values = try .initCapacity(allocator, composite_values_initial_capacity);

    self.types.appendAssumeCapacity(.type);

    return self;
}

pub fn parse(self: *Parser, tokenizer: Tokenizer) Error!void {
    self.tokenizer = tokenizer;

    const s = try self.parseFile(tokenizer);
    self.current_scope = self.getStruct(s).scope;
    try self.foldScope(self.current_scope);
}

fn foldScope(self: *Parser, scope: Scope) Error!void {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = scope;

    if (self.getScope(self.current_scope).container.isDecl())
        try self.foldDeclScope()
    else
        try self.foldBlockScope();
}

fn foldBlockScope(self: *Parser) Error!void {
    const body = self.getBodySlice();

    //go through statements in reverse order
    var statements: List(Node) = .empty; //TODO: have one global stack for all nested blocks
    defer statements.deinit(self.allocator);
    var i: Node = 0;
    while (i < body.len) {
        try statements.append(self.allocator, i);
        i += self.nodeConsumption(i);
    }

    for (0..statements.items.len) |index| {
        const node = statements.items[statements.items.len - index - 1];
        if (self.isStatementSignificantBlockScope(node))
            try self.foldNode(node);
    }
}

fn foldDeclScope(self: *Parser) Error!void {
    const body = self.getBodySlice();
    var i: Node = 0;
    while (i < body.len) {
        if (self.isStatementSignificantDeclScope(i))
            try self.foldNode(i);
        i += self.nodeConsumption(i);
    }
}

fn isStatementSignificantDeclScope(self: *Parser, node: Node) bool {
    return switch (self.getNodeEntry(node).*) {
        .var_decl => |var_decl| //
        util.enumInRange(Qualifier, var_decl.qualifier, .vertex, .compute),
        else => false,
    };
}
fn isStatementSignificantBlockScope(self: *Parser, node: Node) bool {
    return switch (self.getNodeEntry(node).*) {
        .@"return", .assignment => true,
        else => false,
    };
}

fn foldNode(self: *Parser, node: Node) Error!void {
    const entry = self.getNodeEntry(node);
    switch (entry.*) {
        .var_decl => |var_decl| try self.foldVarDecl(node, var_decl),
        .function_decl => |fn_decl| {
            try self.foldScope(self.getFunction(fn_decl.function).scope);
        },
        .@"return" => |token| {
            try self.foldNode(node + 1);
            const rt = try self.currentScopeReturnTypeAndDeclLocation(node);

            if (self.getNodeEntry(node + 1).* == .null) {
                if (self.getType(rt[0]) != .void)
                    return self.errorOut(.{
                        .token = token,
                        .payload = .{ .missing_return_value = rt[1] },
                    });
            } else try self.implicitCast(node + 1, rt[0]);
        },
        .assignment => |token| {
            const target_node = node + 1;
            try self.foldNode(target_node);
            if (!self.isValidAssignmentTarget(target_node))
                return self.errorOut(.{
                    .token = token,
                    .payload = .invalid_assignment_target,
                });

            const value_node = target_node + self.nodeConsumption(target_node);
            try self.foldNode(value_node);
            const target_type = try self.typeOf(target_node);
            try self.implicitCast(value_node, target_type);
        },
        .indexing => {
            const target_node = node + 1;
            try self.foldNode(target_node);
            try self.foldNode(target_node + self.nodeConsumption(target_node));
        },
        .constructor => |constructor| {
            const type_node = node + 1;
            try self.foldNode(type_node);
            const type_opt: ?Type = switch (self.getNodeEntry(type_node).*) {
                .null => null,
                .value => |value| if (value.type != .type)
                    return self.errorOut(.{
                        .token = constructor.token,
                        .payload = .{ .not_a_type = value.type },
                    })
                else
                    @enumFromInt(value.payload),
                else => return self.errorOut(.{
                    .token = constructor.token,
                    .payload = .{ .not_a_type = try self.typeOf(type_node) },
                }),
            };

            const first_elem_node = type_node + self.nodeConsumption(type_node);
            if (type_opt) |@"type"| switch (constructor.elem_count) {
                0 => {},
                1 => try self.foldCast(@"type", first_elem_node, constructor.token),
                else => try self.foldConstructor(@"type", constructor, first_elem_node),
            };
        },
        .u_op => |op| try self.foldUOp(op, node),
        .identifier => |token| if (try self.getVariableReference(self.current_scope, node, token)) |vr| {
            entry.* = if (self.getVariableReferenceValue(vr)) |value|
                .{ .value = value }
            else
                .{ .var_ref = vr };
        } else return self.errorOut(.{ .token = token, .payload = .undeclared_identifier }),
        else => |e| {
            _ = e;
            // std.debug.print("idk how to fold {s}\n", .{@tagName(e)});
        },
    }
}
fn foldCast(self: *Parser, @"type": Type, value: Node, token: Token) Error!void {
    try self.foldNode(value);
    const value_type = try self.typeOf(value);

    if (!self.isTypeExplicitlyCastable(value_type, @"type"))
        return self.errorOut(.{ .token = token, .payload = .{
            .invalid_cast = .{ .from = value_type, .to = @"type" },
        } });
}
fn foldConstructor(self: *Parser, @"type": Type, constructor: ConstructorNode, first: Node) Error!void {
    const constructor_structure = try self.constructorStructure(@"type");
    if (!constructor_structure.canHaveConstructor())
        return self.errorOut(.{
            .token = constructor.token,
            .payload = .{ .type_cant_have_constructor = @"type" },
        });

    var elem = first;
    var occupied: u32 = 0;
    for (0..constructor.elem_count) |_| {
        try self.foldNode(elem);
        const elem_consumption = self.nodeConsumption(elem);
        defer elem += elem_consumption;

        const elem_type = try self.typeOf(elem);
        if (self.isTypeExplicitlyCastable(elem_type, constructor_structure.element)) {
            occupied += 1;
        } else {
            const elem_constructor_structure = try self.constructorStructure(elem_type);
            //check if elem_cs.element and cs.element have the same type depth??
            if (self.isTypeExplicitlyCastable(
                elem_constructor_structure.element,
                constructor_structure.element,
            )) {
                occupied += elem_constructor_structure.len;
            } else return self.errorOut(.{
                .token = self.getNodeEntry(elem).token(),
                .payload = .{ .wrong_constructor_element_type = .{
                    .constructor_type = @"type",
                    .element_type = elem_type,
                } },
            });
        }
    }
    //non matching constructor element count
    if (occupied != constructor_structure.len)
        return self.errorOut(.{
            .token = constructor.token,
            .payload = .{ .non_matching_constructor_element_count = .{
                .constructor_type = @"type",
                .expected_count = constructor_structure.len,
                .got_count = occupied,
            } },
        });
}
fn isValidAssignmentTarget(self: *Parser, node: Node) bool {
    const entry = self.getNodeEntry(node).*;
    return switch (entry) {
        else => true,
    };
}
fn currentScopeReturnTypeAndDeclLocation(self: *Parser, node: Node) Error!@Tuple(&.{ Type, Token }) {
    var scope = self.current_scope;
    while (true) {
        const entry = self.getScope(scope).*;
        if (entry.container.isDecl()) return self.errorOut(.{
            .token = self.getNodeEntry(node).token(),
            .payload = .return_outside_function,
        });
        if (entry.container == .function) {
            const decl_token = self.getNodeEntryScope(
                entry.parent,
                self.getFunction(entry.container.function).node,
            ).token();
            return .{ try self.getFunctionDeclReturnType(entry.container.function, entry.parent), decl_token };
        } else {
            scope = entry.parent;
            continue;
        }
    } else unreachable;
}
fn getFunctionDeclReturnType(self: *Parser, function: Function, parent_scope: Scope) Error!Type {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = parent_scope;

    const entry = self.getFunction(function).*;
    const node = entry.node + 1 + self.nodeSequenceConsumption(self.current_scope, entry.node + 1, entry.arg_count);
    try self.foldNode(node);

    const rtype_node = self.getNodeEntry(node).*;
    if (rtype_node != .value or rtype_node.value.type != .type)
        return self.errorOut(.{
            .token = rtype_node.token(),
            .payload = .unable_to_resolve_comptime_value,
        });
    return @enumFromInt(rtype_node.value.payload);
}
fn foldVarDecl(self: *Parser, node: Node, var_decl: VariableDeclaration) Error!void {
    //check for redelaration
    if (try self.getVariableReference(self.current_scope, node, var_decl.name)) |vr|
        return self.errorOut(.{
            .token = var_decl.qualifier_token,
            .payload = .{ .redeclaration = .{
                .statement = self.getNodeEntryScope(vr.scope, vr.node).token(),
                .name = var_decl.name,
            } },
        });

    const qualifier_info_node = node + 1;
    try self.foldNode(qualifier_info_node);

    const qualifier_info_node_consumption = self.nodeConsumption(node + 1);
    const type_node = node + 1 + qualifier_info_node_consumption;
    try self.foldNode(type_node);

    const type_node_entry = self.getNodeEntry(type_node).*;
    if (type_node_entry != .null and (type_node_entry != .value or type_node_entry.value.type != .type))
        return self.errorOut(.{
            .token = type_node_entry.token(),
            .payload = .unable_to_resolve_comptime_value,
        });

    const type_node_consumption = self.nodeConsumption(type_node);
    const initializer_node = type_node + type_node_consumption;
    try self.foldNode(initializer_node);

    //if type is <null> inferr type from initializer
    //else implicitly cast initializer to variable type
    if (self.getNodeEntry(type_node).* != .null) {
        switch (type_node_entry) {
            .value => |value| if (value.type != .type)
                return self.errorOut(.{
                    .token = type_node_entry.token(),
                    .payload = .{ .not_a_type = value.type },
                }),
            else => return self.errorOut(.{
                .token = type_node_entry.token(),
                .payload = .{ .not_a_type = try self.typeOf(type_node) },
            }),
        }

        try self.implicitCast(
            initializer_node,
            @enumFromInt(self.getNodeEntry(type_node).value.payload),
        );
    } else {
        self.getNodeEntry(type_node).* = .{ .value = .{
            .type = .type,
            .payload = @intFromEnum(try self.typeOf(initializer_node)),
        } };
    }
    const t: Type = @enumFromInt(self.getNodeEntry(type_node).value.payload);

    if (!self.isQualifierCompatibleWithType(var_decl.qualifier, t))
        return self.errorOut(.{
            .token = var_decl.qualifier_token,
            .payload = .{ .qualifier_incompatible_with_type = .{
                .qualifier = var_decl.qualifier,
                .type = t,
            } },
        });
}
fn foldUOp(self: *Parser, u_op: UOpNode, node: Node) Error!void {
    std.debug.print("FOLD UOP: {}\n", .{u_op.op});
    const target = node + 1;
    try self.foldNode(target);
    const target_consumption = self.nodeConsumption(target);
    switch (u_op.op) {
        .pointer => {
            const as_type = try self.asType(target, false);
            const pointer_type = try self.addType(.{ .pointer = as_type });
            @memset(self.getBodySlice()[target .. target + target_consumption], .pad);
            self.getNodeEntry(node).* = .{ .value = .{
                .type = .type,
                .payload = @intFromEnum(pointer_type),
            } };
        },
        else => @panic("FOLD U OP"),
    }
}
fn isQualifierCompatibleWithType(self: *Parser, qualifier: Qualifier, @"type": Type) bool {
    const entry = self.getType(@"type");
    return switch (qualifier) {
        .env, .push, .shared => !entry.isComptime(),
        .vertex, .fragment, .compute => entry == .function,
        else => true,
    };
}

fn getVariableReferenceValue(self: *Parser, var_ref: VariableReference) ?Value {
    const value_node = switch (self.getNodeEntryScope(var_ref.scope, var_ref.node).*) {
        .var_decl => |vd| if (vd.qualifier == .@"const")
            var_ref.node + 1 + self.nodeSequenceConsumption(var_ref.scope, var_ref.node + 1, 2)
        else
            return null,
        else => var_ref.node,
    };
    return switch (self.getNodeEntryScope(var_ref.scope, value_node).*) {
        .value => |value| value,
        else => null,
    };
}
fn getVariableReference(self: *Parser, scope: Scope, node: Node, token: Token) Error!?VariableReference {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = scope;

    const scope_entry = self.getScope(scope);
    const current = if (scope_entry.container == .@"struct")
        try self.getDeclScopeVarRef(node, token)
    else
        try self.getBlockScopeVarRef(node, token);

    return if (current) |c| .{ .node = c, .scope = scope } else //
    if (self.isScopeFile(scope)) null else //
    try self.getVariableReference(scope_entry.parent, scope_entry.getDeclNode(self), token);
}
//both those functions should return VariableReference
//get block scope var ref should loop statements while < node: and check
fn getBlockScopeVarRef(self: *Parser, node: Node, token: Token) Error!?Node {
    return self.getDeclScopeVarRef(node, token);
}

fn getDeclScopeVarRef(self: *Parser, node: Node, token: Token) Error!?Node {
    const body = self.getBodySlice();
    var i: Node = 0;
    const name = self.tokenizer.slice(token);
    // std.debug.print("GET VAR REF OF: {s}\n", .{name});
    while (i < body.len) {
        const consumption = self.nodeConsumption(i);
        defer i += consumption;

        if (i == node) continue;
        switch (self.getNodeEntry(i).*) {
            .var_decl => |vd| if (vd.name == token or util.strEql(
                name,
                self.tokenizer.slice(vd.name),
            )) {
                try self.foldNode(i);
                return i;
            },
            else => {},
        }
    }
    return null;
}
fn implicitCast(self: *Parser, node: Node, @"type": Type) Error!void {
    // std.debug.print("IMPLICIT CAST TO : {f}\n", .{FatType{ .self = self, .type = @"type" }});
    const entry = self.getNodeEntry(node);
    switch (entry.*) {
        .constructor => |elem_count| {
            _ = elem_count;
            const type_node = node + 1;
            const type_node_entry = self.getNodeEntry(type_node);
            const changed: bool = switch (type_node_entry.*) {
                .null => true,
                .value => |value| //
                if (!self.isTypeImplicitlyCastable(@enumFromInt(value.payload), @"type"))
                    return self.errorOut(.{
                        .token = entry.token(),
                        .payload = .cant_implicitly_cast,
                    })
                else
                    true,
                else => return self.errorOut(.unknown),
            };
            type_node_entry.* = .{ .value = .{
                .type = .type,
                .payload = @intFromEnum(@"type"),
            } };
            if (changed) try self.foldNode(node);
        },
        // inline else => |_, tag| @panic("implicit cast ts: " ++ @tagName(tag)),
        else => {},
        // constructor: u32, //[count][type][elems..]

    }
    _ = .{ self, node, @"type" };
}
fn isTypeExplicitlyCastable(self: *Parser, from: Type, to: Type) bool {
    if (from == to) return true;
    const from_entry = self.getType(from);
    const to_entry = self.getType(to);

    const from_is_number = switch (from_entry) {
        .bool, .compint, .compfloat, .scalar => true,
        else => false,
    };
    return switch (to_entry) {
        //TODO: allow pointers to cast to u32x2
        .bool,
        .compint,
        .compfloat,
        => from_is_number,
        .scalar => |scalar| from_is_number or //
            blk: {
                //check for '*T -> u64' cast
                if (scalar.layout == .uint and
                    scalar.width == ._64 and
                    from_entry == .pointer) break :blk true;

                //check for 'enum{...} -> integer' cast
                break :blk scalar.layout != .float and
                    from_entry == .@"enum";
            },
        .vector => |to_vec| from_is_number or
            (from_entry == .vector and from_entry.vector.len == to_vec.len),
        .matrix => |to_mat| from_is_number or
            from_entry == .matrix or
            (to_mat.m == to_mat.n and from_entry == .vector and from_entry.vector.len == to_mat.m),

        .pointer => (from_entry == .scalar and
            from_entry.scalar.layout == .uint and
            from_entry.scalar.width == ._64),
        .@"enum" => switch (from_entry) {
            .bool, .compint => true,
            .scalar => |scalar| scalar.layout != .float,
            else => false,
        },

        else => false,
    };
}
fn isTypeImplicitlyCastable(self: *Parser, from: Type, to: Type) bool {
    if (from == to) return true;
    const from_entry = self.getType(from);
    const to_entry = self.getType(to);

    return switch (to_entry) {
        .scalar => from_entry == .scalar or
            from_entry == .compint or
            from_entry == .compfloat,
        else => false,
    };
}

fn typeOf(self: *Parser, node: Node) Error!Type {
    return switch (self.getNodeEntry(node).*) {
        .builtin => try self.addType(.{ .vector = .{ .len = ._4, .scalar = .{ .layout = .float, .width = ._32 } } }),
        .value => |value| value.type,
        .bin_op => |bin_op| try self.typeOfBinOp(
            bin_op.op,
            try self.typeOf(node + 1),
            try self.typeOf(node + 1 + self.nodeConsumption(node + 1)),
        ),
        .u_op => |u_op| try self.typeOfUOp(u_op.op, try self.typeOf(node + 1)),
        .var_ref => |vr| @enumFromInt(self.getNodeEntryScope(vr.scope, vr.node + 1 + self.nodeConsumption(vr.node + 1)).value.payload),

        //return type of target for now
        .indexing => blk: {
            //fold indexing
            const target_type = try self.typeOf(node + 1);
            const entry = self.getType(target_type);
            break :blk switch (entry) {
                .pointer => |pointed| pointed,
                else => target_type,
            };
        },
        .function_decl => |fn_decl| try self.typeOfFunctionDecl(node, fn_decl.function),
        .constructor => switch (self.getNodeEntry(node + 1).*) {
            .value => |value| if (value.type == .type)
                @enumFromInt(value.payload)
            else
                return self.errorOut(.unknown),
            else => return self.errorOut(.unknown),
        },

        else => |e| {
            std.debug.print("idk type of '{s}'\n", .{@tagName(e)});
            return self.errorOut(.unknown);
        },
    };
}
fn typeOfFunctionDecl(self: *Parser, node: Node, function: Function) Error!Type {
    const entry = self.getFunction(function);
    if (entry.arg_count != 0) @panic("type of function decl with args!");

    const arg_offset: Node = 0;
    const prev_types_len = self.types.items.len;

    const type_header = try self.appendTypeRaw(.{ .function = entry.arg_count });
    const rtype = try self.asType(node + 1 + arg_offset, true);
    const only_references = self.getType(rtype) == .ref;
    if (!only_references) return type_header;

    if (self.findFunctionType(type_header)) |ft| {
        self.types.items.len = prev_types_len;
        return ft;
    } else return type_header;
}
fn findFunctionType(self: *Parser, ft: Type) ?Type {
    const arg_count = self.getType(ft).function;
    return for (self.types.items[0..@intFromEnum(ft)], 0..) |entry, i| {
        var target_arg_offset: u32 = 0;
        if (entry == .function and entry.function == arg_count and for (0..arg_count + 1) |j| {
            const arg_id, const target_arg_id = .{ @intFromEnum(ft) + j + 1, i + target_arg_offset + 1 };
            const arg = self.types.items[arg_id];
            const target_arg = self.types.items[target_arg_id];
            defer target_arg_offset += self.typeLength(@enumFromInt(target_arg_id));

            if (arg.ref != @as(Type, @enumFromInt(target_arg_id)) and (target_arg != .ref or arg.ref != target_arg.ref))
                break false;
        } else true) return @enumFromInt(i);
    } else null;
}

fn asType(self: *Parser, node: Node, comptime create_ref: bool) Error!Type {
    const entry = self.getNodeEntry(node).*;
    return switch (entry) {
        .value => |value| blk: {
            if (value.type != .type) return self.errorOut(.{
                .token = entry.token(),
                .payload = .{ .not_a_type = value.type },
            });
            break :blk if (create_ref)
                try self.appendTypeRaw(.{ .ref = @enumFromInt(value.payload) })
            else
                @enumFromInt(value.payload);
        },
        else => return self.errorOut(.{
            .token = entry.token(),
            .payload = .{ .not_a_type = try self.typeOf(node) },
        }),
    };
}
fn typeOfBinOp(self: *Parser, op: BinaryOperator, left: Type, right: Type) Error!Type {
    _ = .{ self, right };
    return switch (op) {
        else => left,
    };
}
fn typeOfUOp(self: *Parser, op: UnaryOperator, operand: Type) Error!Type {
    _ = self;
    return switch (op) {
        else => operand,
    };
}
fn nodeConsumptionScope(self: *Parser, scope: Scope, node: u32) u32 {
    // array_type_decl,
    // function_decl: Function, //[fn_decl][args...][rtype][body...]
    // branch,
    var base: u32 = switch (self.getNodeEntryScope(scope, node).*) {
        .var_decl => 1 + self.nodeSequenceConsumption(scope, node + 1, 3),
        .bin_op, .indexing, .assignment => 1 + self.nodeSequenceConsumption(scope, node + 1, 2),
        .u_op, .@"return", .function_param => 1 + self.nodeConsumption(node + 1),
        .null => 1,
        .function_decl => 1 + self.nodeConsumption(node + 1), //args
        .constructor => |constructor| 1 + self.nodeSequenceConsumption(scope, node + 1, constructor.elem_count + 1),
        .function_type_decl => |fn_type_decl| 1 + self.nodeSequenceConsumption(scope, node + 1, fn_type_decl.arg_count + 1),
        else => 1,
    };
    const body = self.getScope(scope).body.items;
    while (node + base < body.len and body[node + base] == .pad) base += 1;
    return base;
}
fn nodeConsumption(self: *Parser, node: Node) u32 {
    return self.nodeConsumptionScope(self.current_scope, node);
}
fn nodeSequenceConsumption(self: *Parser, scope: Scope, node: Node, count: usize) u32 {
    var len: u32 = 0;
    for (0..count) |_|
        len += self.nodeConsumptionScope(scope, node + len);
    return len;
}

fn parseFile(self: *Parser, tokenizer: Tokenizer) Error!Struct {
    const last_tokenizer = self.tokenizer;
    defer self.tokenizer = last_tokenizer;
    self.tokenizer = tokenizer;

    const struct_handle = try self.addStruct(.{ .is_file = true });
    const struct_type = try self.addType(.{ .@"struct" = struct_handle });
    _ = .{struct_type};

    self.getStruct(struct_handle).scope = try self.parseScope(.{
        .container = .{ .@"struct" = struct_handle },
        .parent = self.current_scope,
    });

    return struct_handle;
}

fn parseScope(self: *Parser, entry: ScopeEntry) Error!Scope {
    const scope = try self.addScope(entry);

    const last_scope = self.current_scope;
    self.current_scope = scope;
    defer self.current_scope = last_scope;

    while (true) {
        _ = self.skipEndl();
        const peek = self.tokenizer.kind(self.token);
        if (!self.isScopeFile(self.current_scope)) {
            if (peek == .eof) return self.errorOut(.{
                .token = self.getNodeEntryScope(last_scope, self.getScope(self.current_scope).getDeclNode(self)).token(),
                .payload = .unclosed_scope,
            });
            if (peek == .@"}") {
                self.token += 1;
                break;
            }
        } else if (peek == .eof) {
            self.token += 1;
            break;
        }

        try self.parseStatement();
    }
    return scope;
}

fn parseStatement(self: *Parser) Error!void {
    switch (self.getScope(self.current_scope).container) {
        .@"struct" => try self.parseStatementStructDecl(),
        .function => try self.parseStatementBlock(),
        // else => @panic("parse statement unknown container"),
    }
}
fn parseStatementBlock(self: *Parser) Error!void {
    switch (self.tokenizer.kind(self.token)) {
        .@"const", .@"var", .shared, .push, .env, .fragment, .vertex, .compute => |q| {
            self.token += 1;
            try self.parseVarDecl(.fromTokenKind(q), self.token - 1);
        },

        .@"return" => {
            _ = try self.appendNode(.{ .@"return" = self.token });
            self.token += 1;
            if (self.isAtScopeEnd())
                _ = try self.appendNode(.null)
            else
                _ = try self.parseExpression2(.scope_end);
        },
        else => {
            const peek = self.token;
            const is_assignment = ass: switch (self.tokenizer.kind(self.token)) {
                .@"=" => break :ass true,
                else => if (self.isAtScopeEnd()) false else {
                    self.token += 1;
                    continue :ass self.tokenizer.kind(self.token);
                },
            };
            self.token = peek;
            if (is_assignment) {
                _ = try self.appendNode(.{ .assignment = peek });
                _ = try self.parseExpression2(.{ .kind = .@"=" }); //TARGET
                self.token += 1; //skip '=' and endl
                _ = self.skipEndl();
                _ = try self.parseExpression2(.scope_end); //VALUE
            } else _ = try self.parseExpression2(.scope_end);
        },
    }
}
fn parseStatementStructDecl(self: *Parser) Error!void {
    switch (self.tokenizer.kind(self.token)) {
        .@"const", .@"var", .shared, .push, .env, .fragment, .vertex, .compute => |q| {
            self.token += 1;
            try self.parseVarDecl(.fromTokenKind(q), self.token - 1);
        },
        .@"return" => return self.errorOut(.{
            .token = self.token,
            .payload = .return_outside_function,
        }),
        else => return self.errorOut(.{
            .token = self.token - 1,
            .payload = .unexpected_token,
        }),
    }
}
fn parseVarDecl(self: *Parser, qualifier: Qualifier, qualifier_token: Token) Error!void {
    const name_token = self.token;
    if (self.tokenizer.kind(name_token) != .identifier)
        return self.errorOut(.{ .token = name_token, .payload = .unexpected_token });
    self.token += 1;

    _ = try self.appendNode(.{ .var_decl = .{
        .qualifier = qualifier,
        .qualifier_token = qualifier_token,
        .name = name_token,
    } });
    try self.parseQualifierInfo();

    const type_node = self.nextNode();
    if (self.tokenizer.kind(self.token) == .@":") {
        self.token += 1;
        _ = try self.parseExpression1();
    } else _ = try self.appendNode(.null);

    if (self.tokenizer.kind(self.token) != .@"=") {
        if (!self.isAtScopeEnd())
            return self.errorOut(.{ .token = self.token, .payload = .unexpected_token });

        _ = try self.appendNode(.null);
        //if both type and initializer are null OR
        //qualifier must have initializer - error out
        if (qualifier.mustHaveInitializer() or self.getNodeEntry(type_node).* == .null)
            return self.errorOut(.{
                .token = qualifier_token,
                .payload = .{ .missing_initializer = qualifier },
            });
    } else {
        if (!qualifier.canHaveInitializer())
            return self.errorOut(.{
                .token = qualifier_token,
                .payload = .{ .qualifier_cant_have_initializer = qualifier },
            });
        self.token += 1;
        _ = try self.parseExpression2(.scope_end);
    }
}
fn parseQualifierInfo(self: *Parser) Error!void {
    _ = try self.appendNode(.null); //qualifier info
}

const ExpressionDelimiter = union(enum) {
    kind: Tokenizer.TokenKind,
    kind_or_comma: Tokenizer.TokenKind,
    scope_end,
    pub fn shouldStop(delimiter: ExpressionDelimiter, self: *Parser) bool {
        return switch (delimiter) {
            .kind => |kind| self.tokenizer.kind(self.token) == kind,
            .kind_or_comma => |kind| self.tokenizer.kind(self.token) == kind or
                self.tokenizer.kind(self.token) == .@",",
            .scope_end => //
            self.tokenizer.kind(self.token) == .endl or
                self.tokenizer.kind(self.token) == .eof or
                self.tokenizer.kind(self.token) == .@"}",
        };
    }
};
pub inline fn isAtScopeEnd(self: *Parser) bool {
    const eos: ExpressionDelimiter = .scope_end;
    return eos.shouldStop(self);
}
inline fn parseExpression2(self: *Parser, delimiter: ExpressionDelimiter) Error!u32 {
    return self.parseExpressionRecursive(delimiter, 0);
}
fn parseExpressionRecursive(self: *Parser, delimiter: ExpressionDelimiter, bp: u8) Error!u32 {
    var left_len = try self.parseExpression1();

    var iter: usize = 0;
    while (!delimiter.shouldStop(self)) {
        const op = if (Tokenizer.binOpFromTokenKind(self.tokenizer.kind(self.token))) |bop|
            bop
        else
            return self.errorOut(.{ .token = self.token, .payload = .unexpected_token });

        if (Tokenizer.bindingPower(op) <= bp) break;
        self.token += 1;

        // try self.tokenizer.skipEndl(); //?

        const bin_op_node: NodeEntry = .{ .bin_op = .{ .op = op, .token = self.token - 1 } };
        try self.insertNthLastNode(left_len, bin_op_node);
        iter += 1;

        left_len += 1 + try self.parseExpressionRecursive(delimiter, Tokenizer.bindingPower(op));
    }
    return left_len;
}
fn insertNodeAssumeCapacity(self: *Parser, i: u32, entry: NodeEntry) void {
    self.getScope(self.current_scope).body.insertAssumeCapacity(i, entry);
}
fn insertNthLastNode(self: *Parser, n: u32, entry: NodeEntry) Error!void {
    const body = &self.getScope(self.current_scope).body;
    try body.insert(self.allocator, body.items.len - n, entry);
}
fn parseExpression1(self: *Parser) Error!u32 {
    const node = self.nextNode();
    var len = try self.parseExpression0();
    sw: switch (self.tokenizer.kind(self.token)) {
        .@"[" => {
            try self.insertNthLastNode(len, .{ .indexing = self.token });
            self.token += 1;
            len += 1 + try self.parseExpression2(.{ .kind = .@"]" });
            self.token += 1;
            continue :sw self.tokenizer.kind(self.token);
        },
        .@"{" => {
            const open_token = self.token;
            self.token += 1;
            try self.insertNthLastNode(len, .null);
            const seq = try self.parseSequence(.@"}");
            self.getNodeEntry(node).* = .{ .constructor = .{
                .elem_count = seq.count,
                .token = open_token,
            } };
            len += 1 + seq.node_consumption;
            continue :sw self.tokenizer.kind(self.token);
        },

        //     else => return expr,
        // }
        else => return len,
    }
}

//returns the node list size not the amount of consumed tokens
fn parseExpression0(self: *Parser) Error!u32 {
    return switch (self.tokenizer.kind(self.token)) {
        .@"(" => blk: {
            self.token += 1;
            const expr = self.parseExpression2(.{ .kind = .@")" });
            self.token += 1;
            break :blk expr;
        },
        .int_literal => blk: {
            _ = try self.appendNode(.{ .value = .{
                .type = try self.addType(.compint),
                .payload = try self.addScalarValue(
                    parseIntLiteral(self.tokenizer.slice(self.token)),
                ),
            } });

            self.token += 1;
            break :blk 1;
        },
        // .float_literal
        .@"fn" => blk: {
            self.token += 1;
            break :blk try self.parseFunctionTypeOrDecl();
        },
        .type_literal => blk: {
            const type_entry = self.tokenizer.parseTypeLiteral(self.token);
            self.token += 1;
            _ = try self.appendNode(.{ .value = .{
                .type = .type,
                .payload = @intFromEnum(try self.addType(type_entry)),
            } });
            break :blk 1;
        },
        .identifier => blk: {
            _ = try self.appendNode(.{ .identifier = self.token });
            self.token += 1;
            break :blk 1;
        },
        .builtin => blk: {
            _ = try self.appendNode(.{ .builtin = self.token });
            self.token += 1;
            break :blk 1;
        },
        .true, .false => |tf| blk: {
            _ = try self.appendNode(.{ .value = .{
                .type = try self.addType(.bool),
                .payload = try self.addScalarValue(util.fit(u128, tf == .true)),
            } });
            self.token += 1;
            break :blk 1;
        },
        .void => blk: {
            _ = try self.appendNode(.{ .value = .{
                .type = .type,
                .payload = @intFromEnum(try self.addType(.void)),
            } });
            self.token += 1;
            break :blk 1;
        },
        .neg, .pos, .not, .sat, .abs, .sqrt, .invsqrt, .norm, .mag, .sqrmag, .pointer => |op| blk: {
            const u_op = Tokenizer.uOpFromTokenKind(op).?;
            _ = try self.appendNode(.{ .u_op = .{ .op = u_op, .token = self.token } });
            self.token += 1;
            break :blk 1 + try self.parseExpression1();
        },
        .@"." => switch (self.tokenizer.kind(self.token + 1)) {
            .@"{" => blk: {
                self.token += 2;
                const node = try self.appendNode(.{ .constructor = .{
                    .elem_count = 0,
                    .token = self.token - 1,
                } });
                _ = try self.appendNode(.null);

                const seq = try self.parseSequence(.@"}");

                self.getNodeEntry(node).constructor.elem_count = seq.count;
                break :blk 1 + seq.node_consumption;
            },
            .identifier => @panic("enum literal"),
            else => return self.errorOut(.{ .token = self.token + 1, .payload = .unexpected_token }),
        },
        else => return self.errorOut(.{ .token = self.token, .payload = .unexpected_token }),
    };
}
const ParseArgumentsResult = struct {
    count: u32 = 0,
    node_consumption: u32 = 0,
};
fn parseFunctionTypeOrDecl(self: *Parser) Error!u32 {
    const header_token = self.token - 1;
    //TODO: in parse sequence put skipEndl() in else block of while loop
    //to allow endl before delimiter
    //TODO: parseExpression1 but without @"{" for function rtype
    const header = try self.appendNode(.{ .function_type_decl = .{
        .token = header_token,
        .arg_count = 0,
    } });

    var arg_count: u32 = 0;
    var arg_node_consumption: Node = 0;
    var must_be_fn_decl = false;

    if (self.tokenizer.kind(self.token) == .@"(") {
        self.token += 1;
        while (self.tokenizer.kind(self.tokenPastEndl()) != .@")") {
            const qualifier: ParameterQualifier = switch (self.tokenizer.kind(self.token)) {
                .@"comptime" => .@"comptime",
                .flat => .flat,
                .linear => .linear,
                else => .none,
            };
            if (qualifier != .none) self.token += 1;
            const State = enum { idk, param, arg_type };
            var state: State = if (qualifier != .none) .param else .idk;

            const name = self.token;

            const meat = self.tokenizer.kind(self.token);
            if (meat != .identifier)
                if (state == .param or must_be_fn_decl) {
                    return self.errorOut(.{ .token = self.token, .payload = .unexpected_token });
                } else {
                    state = .arg_type;
                    arg_node_consumption += try self.parseExpression2(.{ .kind_or_comma = .@")" });
                };
            self.token += 1;

            //state cannot be .arg_type as it would errorOut on 'parseExpression2'
            if (self.tokenizer.kind(self.token) == .@":") {
                state = .param;
                self.token += 1;
                _ = self.skipEndl();

                _ = try self.appendNode(.{ .function_param = .{ .name = name, .qualifier = qualifier } });

                if (self.tokenizer.kind(self.token) == .@"anytype") {
                    _ = try self.appendNode(.{ .@"anytype" = self.token });
                    self.token += 1;
                    arg_node_consumption += 2;
                } else arg_node_consumption += 1 + try self.parseExpression2(.{ .kind_or_comma = .@")" });
            } else if (state == .param or must_be_fn_decl) {
                //parameter with inferred type
                //change to <pad> later??
                _ = try self.appendNode(.{ .function_param = .{ .name = name, .qualifier = qualifier } });
                _ = try self.appendNode(.null);
                arg_node_consumption += 2; //header + <null>

            } else if (meat == .identifier) {
                _ = try self.appendNode(.{ .identifier = name });
                arg_node_consumption += 1;
            }

            if (state == .param) {
                defer must_be_fn_decl = true;
                if (!must_be_fn_decl and arg_count > 0)
                    try self.convertArgTypesToFunctionParameters(header + 1, arg_count, header_token);
            }

            arg_count += 1;

            if (self.tokenizer.kind(self.token) == .@",") {
                self.token += 1;
                _ = self.skipEndl();
            }
        }
        self.token += 1;
        _ = self.skipEndl();
    }
    if (self.isAtScopeEnd()) return self.errorOut(.{
        .token = header_token,
        .payload = .missing_return_type,
    });

    const rtype_len = try self.parseExpression0();

    if (self.tokenizer.kind(self.tokenPastEndl()) == .@"{") {
        if (!must_be_fn_decl) try self.convertArgTypesToFunctionParameters(header + 1, arg_count, header_token);

        _ = self.skipEndl();
        self.token += 1;
        const function = try self.addFunction(.{ .node = header, .arg_count = arg_count });
        const scope = try self.parseScope(.{
            .parent = self.current_scope,
            .container = .{ .function = function },
        });
        self.getFunction(function).scope = scope;
        self.getNodeEntry(header).* = .{ .function_decl = .{
            .function = function,
            .token = header_token,
        } };
    } else {
        if (must_be_fn_decl) return self.errorOut(.{
            .token = header_token,
            .payload = .missing_function_body,
        });
        self.getNodeEntry(header).function_type_decl.arg_count = arg_count;
    }

    return 1 + arg_node_consumption + rtype_len;
}
fn convertArgTypesToFunctionParameters(self: *Parser, node: Node, count: u32, fn_decl_token: Token) Error!void {
    const scope = self.getScope(self.current_scope);
    try scope.body.ensureUnusedCapacity(self.allocator, count);

    for (0..count) |i| {
        const current = node + 2 * @as(u32, @truncate(i));
        const name: Token = switch (self.getNodeEntry(current).*) {
            .identifier => |id| id,
            else => return self.errorOut(.{
                .token = fn_decl_token,
                .payload = .invalid_function_declaration,
            }),
        };

        self.insertNodeAssumeCapacity(current, .{ .function_param = .{
            .name = name,
            .qualifier = .none,
        } });

        self.getNodeEntry(current + 1).* = .null;
    }
}
fn parseSequence(self: *Parser, delimiter: Tokenizer.TokenKind) Error!ParseArgumentsResult {
    _ = self.skipEndl();
    var result: ParseArgumentsResult = .{};

    while (self.tokenizer.kind(self.tokenPastEndl()) != delimiter) {
        result.node_consumption += try self.parseExpression2(.{ .kind_or_comma = delimiter });
        result.count += 1;
        if (self.tokenizer.kind(self.token) == .@",") {
            self.token += 1;
            _ = self.skipEndl();
        }
    }
    self.token += if (self.tokenizer.kind(self.token) == .endl) 2 else 1;
    return result;
}

fn nextNode(self: *Parser) Node {
    return @truncate(self.getBodySlice().len);
}

fn parseIntLiteral(str: []const u8) i128 {
    return str[0] - '0';
}

fn isScopeFile(self: *Parser, scope: Scope) bool {
    const entry = self.getScope(scope);
    return if (entry.container == .@"struct")
        self.getStruct(entry.container.@"struct").is_file
    else
        false;
}

fn skipEndl(self: *Parser) bool {
    const is_endl = self.tokenizer.kind(self.token) == .endl;
    self.token += @intFromBool(is_endl);
    return is_endl;
}

fn tokenPastEndl(self: *Parser) Token {
    return self.token + @intFromBool(self.tokenizer.kind(self.token) == .endl);
}

fn getScalarValue(self: *Parser, id: u32) u128 {
    return self.scalar_values.items[id];
}
fn getType(self: *Parser, @"type": Type) TypeEntry {
    return self.types.items[@intFromEnum(@"type")];
}
fn addType(self: *Parser, entry: TypeEntry) Error!Type {
    return self.addTypeOpt(entry, false);
}
fn addTypeOpt(self: *Parser, entry: TypeEntry, comptime create_ref: bool) Error!Type {
    if (entry != .ref)
        for (0..self.types.items.len) |i| {
            if (std.meta.eql(entry, self.getType(@enumFromInt(i))))
                return if (create_ref)
                    self.appendTypeRaw(.{ .ref = @enumFromInt(i) })
                else
                    @enumFromInt(i);
        };
    return self.appendTypeRaw(entry);
}
fn appendTypeRaw(self: *Parser, entry: TypeEntry) Error!Type {
    const l = self.types.items.len;
    try self.types.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getNodeEntry(self: *Parser, node: Node) *NodeEntry {
    return self.getNodeEntryScope(self.current_scope, node);
}
fn getNodeEntryScope(self: *Parser, scope: Scope, node: Node) *NodeEntry {
    return &self.getScope(scope).body.items[node];
}
fn appendNode(self: *Parser, entry: NodeEntry) Error!Node {
    const scope = self.getScope(self.current_scope);
    const l: Node = @truncate(scope.body.items.len);
    try scope.body.append(self.allocator, entry);
    return l;
}

fn addScalarValue(self: *Parser, scalar: anytype) Error!u32 {
    for (self.scalar_values.items, 0..) |s, i| if (s == util.fit(u128, scalar)) return @truncate(i);
    const l: u32 = @truncate(self.scalar_values.items.len);
    try self.scalar_values.append(self.allocator, util.fit(u128, scalar));
    return l;
}

fn addScope(self: *Parser, entry: ScopeEntry) Error!Scope {
    const l = self.scopes.items.len;
    try self.scopes.append(self.allocator, entry);
    return @enumFromInt(l);
}
inline fn getBodySlice(self: *Parser) []NodeEntry {
    return self.getScope(self.current_scope).body.items;
}
inline fn getScope(self: *Parser, handle: Scope) *ScopeEntry {
    return &self.scopes.items[@intFromEnum(handle)];
}
const Scope = enum(u32) { entry_file, _ };
pub const ScopeEntry = struct {
    body: List(NodeEntry) = .empty,
    parent: Scope = .entry_file,
    container: Container,
    const Container = union(enum) {
        @"struct": Struct,
        function: Function,
        pub fn isDecl(self: Container) bool {
            return self == .@"struct";
        }
    };
    pub fn getDeclNode(entry: ScopeEntry, self: *Parser) Node {
        return switch (entry.container) {
            .@"struct" => |s| self.getStruct(s).node,
            .function => |f| self.getFunction(f).node,
        };
    }
};

fn addStruct(self: *Parser, entry: StructEntry) Error!Struct {
    const l = self.structs.items.len;
    try self.structs.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getStruct(self: *Parser, handle: Struct) *StructEntry {
    return &self.structs.items[@intFromEnum(handle)];
}
pub const Struct = enum(u32) { entry_file = 0, _ };
pub const StructEntry = struct {
    name: Name = undefined,
    // name: []const u8 = undefined,
    scope: Scope = undefined,
    node: Node = undefined,

    len: u32 = 0,
    is_file: bool = false,
    const Name = union {
        token: Token,
        tokenizer: u32,
    };
};

fn addFunction(self: *Parser, entry: FunctionEntry) Error!Function {
    const l = self.functions.items.len;
    try self.functions.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getFunction(self: *Parser, handle: Function) *FunctionEntry {
    return &self.functions.items[@intFromEnum(handle)];
}
pub const Function = enum(u32) { _ };
pub const FunctionEntry = struct {
    scope: Scope = undefined,
    node: Node = undefined,

    name: Token = undefined,
    len: u32 = 0,

    arg_count: u32 = 0,

    stage_dependency: StageDependency = .{},
    pub const StageDependency = packed struct {
        vertex: bool = false,
        fragment: bool = false,
        compute: bool = false,
    };
};

// const CompositeHeader = packed struct(u8);

pub const Node = u32;
pub const NodeEntry = union(enum) {
    pad,
    null,

    identifier: Token,
    builtin: Token,

    bin_op: BinOpNode, //[bin_op][left][right]
    u_op: UOpNode, //[u_op][operand]
    value: Value,

    indexing: Token, //[][target][index]
    constructor: ConstructorNode, //[count][type][elems..]

    array_type_decl, //??

    function_type_decl: FunctionTypeDecl, //[fn_type_decl][arg_count][arg_types...][rtype]
    function_decl: FunctionDecl, //[fn_decl][args...][rtype][body...]
    @"anytype": Token,
    function_param: FunctionParameter,
    //[fn_param][type / anytype / null(if the same as the next one)]
    var_ref: VariableReference,
    assignment: Token, //[assignment][target][value]

    @"return": Token, //[return][return value]
    branch,
    loop: [11]u8,

    //statement
    var_decl: VariableDeclaration, //[var_decl][qualifier_info][type][initializer]
    pub fn token(self: NodeEntry) Token {
        return switch (self) {
            .null, .pad => 0, //unknown
            .array_type_decl, .branch, .loop, .var_ref => 0, //

            .function_param => |fp| fp.name,
            .var_decl => |vd| vd.qualifier_token,
            .identifier,
            .builtin,
            .assignment,
            .indexing,
            .@"return",
            .@"anytype",
            => |tok| tok,
            inline else => |o| o.token,
        };
    }
};
const FunctionTypeDecl = struct {
    arg_count: u32,
    token: Token,
};
const FunctionDecl = struct {
    function: Function,
    token: Token,
};
const ConstructorNode = struct {
    elem_count: u32,
    token: Token,
};
const BinOpNode = struct {
    op: BinaryOperator,
    token: Token,
};
const UOpNode = struct {
    op: UnaryOperator,
    token: Token,
};
const FunctionParameter = struct {
    name: Token,
    qualifier: ParameterQualifier = .none,
};
pub const ParameterQualifier = enum {
    none,
    @"comptime",
    linear,
    flat,
};
const VariableReference = struct {
    scope: Scope,
    node: Node,
    value: Node = 0,
};
const VariableDeclaration = struct {
    qualifier: Qualifier,
    qualifier_token: Token,

    name: Token,
};
pub const Qualifier = enum {
    @"const",
    @"var",
    env,

    push,
    shared,

    vertex,
    fragment,
    compute, //[workgroup size]
    pub fn canHaveInitializer(self: Qualifier) bool {
        return self != .push;
    }
    pub fn mustHaveInitializer(self: Qualifier) bool {
        return switch (self) {
            .fragment, .vertex, .compute, .@"const", .env => true,
            else => false,
        };
    }
    pub fn fromTokenKind(kind: Tokenizer.TokenKind) Qualifier {
        return @enumFromInt(@intFromEnum(kind) -
            @intFromEnum(Tokenizer.TokenKind.@"const"));
    }
};
const Value = struct {
    type: Type,
    payload: u32,
    token: Token = undefined,
};

fn typeLength(self: *Parser, @"type": Type) u32 {
    return switch (self.getType(@"type")) {
        .function => |function| //
        1 + self.typeSequenceLength(@enumFromInt(@intFromEnum(@"type") + 1), function + 1),
        else => 1,
    };
}
fn typeSequenceLength(self: *Parser, @"type": Type, count: usize) u32 {
    var len: u32 = 0;
    for (0..count) |_| len += self.typeLength(@enumFromInt(@intFromEnum(@"type") + len));
    return len;
}
const ConstructorStructure = struct {
    element: Type,
    len: u32 = 1,
    pub inline fn canHaveConstructor(self: ConstructorStructure) bool {
        return self.len > 1;
    }
};
pub fn constructorStructure(self: *Parser, @"type": Type) Error!ConstructorStructure {
    const entry = self.getType(@"type");
    return switch (entry) {
        .vector => |vector| .{
            .len = vector.len.value(),
            .element = try self.addType(.{ .scalar = vector.scalar }),
        },
        .matrix => |matrix| .{
            .len = matrix.n.value(),
            .element = try self.addType(.{ .vector = .{ .len = matrix.m, .scalar = matrix.scalar } }),
        },
        else => .{ .element = @"type" },
    };
}

pub const Type = enum(u32) { type, _ };
pub const TypeEntry = union(enum) {
    void,
    type,
    bool,
    compint,
    compfloat,

    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    array: u32, //length
    pointer: Type,

    @"enum": u32,
    @"struct": Struct,
    function: u32, //[function][args...][rtype]

    ref: Type,
    pub const Tag = std.meta.Tag(@This());
    pub fn isComptime(self: TypeEntry) bool {
        return switch (self) {
            .void, .type, .compint, .compfloat, .function => true,
            else => false,
        };
    }
    pub fn format(self: TypeEntry, writer: *std.Io.Writer) !void {
        switch (self) {
            .scalar => |scalar| try writer.print("{c}{s}", .{
                "fui"[@intFromEnum(scalar.layout)],
                @tagName(scalar.width)[1..],
            }),
            .vector => |vector| try writer.print("{f}x{s}", .{
                TypeEntry{ .scalar = vector.scalar },
                @tagName(vector.len)[1..],
            }),
            .ref => |id| try writer.print("ref({d})", .{@intFromEnum(id)}),
            else => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};

pub const Matrix = packed struct {
    scalar: Scalar,
    m: Vector.Len,
    n: Vector.Len,
};
pub const Vector = packed struct {
    scalar: Scalar,
    len: Len,
    pub const Len = enum(u2) {
        _2,
        _3,
        _4,
        pub fn value(self: Len) u32 {
            return 2 + @as(u32, @intFromEnum(self));
        }
    };
};
pub const Scalar = packed struct {
    width: Width,
    layout: Layout,
    pub const Width = enum(u2) { _8, _16, _32, _64 };
    pub const Layout = enum(u2) { float, uint, int };

    pub const all: [4 * 3 - 1]Scalar = blk: {
        const len = 4 * 3 - 1;
        var result: [len]Scalar = undefined;
        var count: usize = 0;
        for (@typeInfo(Width).@"enum".fields) |wef| {
            for (@typeInfo(Layout).@"enum".fields) |lef| {
                const s: Scalar = .{
                    .width = @as(Width, @enumFromInt(wef.value)),
                    .layout = @as(Layout, @enumFromInt(lef.value)),
                };
                if (s.width == ._8 and s.layout == .float) continue;
                result[count] = s;
                count += 1;
            }
        }
        break :blk result;
    };
    pub fn ToZig(comptime scalar: Scalar) type {
        return if (scalar.layout == .float)
            std.meta.Float(@max(16, @intFromEnum(scalar.width)))
        else
            @Int(if (scalar.layout == .int) .signed else .unsigned, @intFromEnum(scalar.width));
    }
};
pub fn errorOut(self: *Parser, error_info: ErrorInfo) Error {
    self.error_info = error_info;
    return Error.CompilationError;
}

//debug structs for formatting

pub const FatToken = struct {
    self: Tokenizer,
    token: Token,
    pub fn format(entry: FatToken, writer: *std.Io.Writer) !void {
        const kind = entry.self.kind(entry.token);
        try writer.print("'{s}'", .{@tagName(kind)});
        switch (kind) {
            .identifier,
            .int_literal,
            .float_literal,
            .type_literal,
            => try writer.print(" : \"{s}\"", .{entry.self.slice(entry.token)}),
            else => {},
        }
    }
};
pub const FatType = struct {
    self: *Parser,
    type: Type,
    pub fn format(entry: FatType, writer: *std.Io.Writer) !void {
        const te = entry.self.getType(entry.type);
        switch (te) {
            .function => |function| {
                try writer.print("fn(", .{});
                for (0..function) |i| try writer.print("{f}, ", .{FatType{
                    .self = entry.self,
                    .type = @enumFromInt(@intFromEnum(entry.type) + i + 1),
                }});
                try writer.print("):{f}", .{FatType{
                    .self = entry.self,
                    .type = @enumFromInt(@intFromEnum(entry.type) + function + 1),
                }});
            },
            .ref => |ref| try format(.{ .self = entry.self, .type = ref }, writer),
            .pointer => |pointed| try writer.print("*{f}", .{FatType{ .self = entry.self, .type = pointed }}),
            else => try TypeEntry.format(te, writer),
        }
    }
};
const FatValue = struct {
    self: *Parser,
    value: Value,
    pub fn format(entry: FatValue, writer: *std.Io.Writer) !void {
        const type_entry = entry.self.getType(entry.value.type);
        switch (type_entry) {
            .compint => try writer.print("{d}", .{
                util.extract(i128, entry.self.getScalarValue(entry.value.payload)),
            }),
            .bool => try writer.print("{}", .{util.extract(bool, entry.self.getScalarValue(entry.value.payload))}),
            .type => try writer.print("{f}", .{FatType{
                .self = entry.self,
                .type = @enumFromInt(entry.value.payload),
            }}),
            else => {},
        }
    }
};
const FatNode = struct {
    self: *Parser,
    node: *Node,
    pub fn format(entry: FatNode, writer: *std.Io.Writer) !void {
        const body = entry.self.getBodySlice();
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
            .function_type_decl => |fn_type_decl| {
                entry.node.* += 1;
                try writer.print("fn(", .{});

                for (0..fn_type_decl.arg_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == fn_type_decl.arg_count) "" else ", " });
                try writer.print(") {f}", .{entry});
            },
            .function_decl => |fn_decl| {
                entry.node.* += 1;
                const arg_count = entry.self.getFunction(fn_decl.function).arg_count;
                try writer.print("fn (", .{});

                for (0..arg_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == arg_count) "" else ", " });
                try writer.print(") {f}{{\n", .{entry});

                const last_scope = entry.self.current_scope;
                entry.self.current_scope = entry.self.getFunction(fn_decl.function).scope;
                defer entry.self.current_scope = last_scope;

                entry.self.dumpCurrentScope(false, true);
            },
            .function_param => |fn_param| {
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
            .var_decl => |var_decl| {
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
                try writer.print("{f}", .{FatValue{ .self = entry.self, .value = value }});
                entry.node.* += 1;
            },
            .identifier => |identifier| {
                try writer.print("\"{s}", .{entry.self.tokenizer.slice(identifier)});
                entry.node.* += 1;
            },
            .builtin => |builtin| {
                try writer.print("{s}", .{entry.self.tokenizer.slice(builtin)});
                entry.node.* += 1;
            },
            .var_ref => |vr| {
                entry.node.* += 1;
                try writer.print("'{s}", .{
                    entry.self.tokenizer.slice(entry.self.getNodeEntryScope(vr.scope, vr.node).var_decl.name),
                });
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

const ErrorInfo = error_message.ParserErrorInfo;
const Error = hgsl.Error;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
const UnaryOperator = Tokenizer.UnaryOperator;
const Token = Tokenizer.Token;
const TokenEntry = Tokenizer.TokenEntry;
