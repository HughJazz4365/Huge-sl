//YES WE REMOVE current_scope all together??

//DIFFERENT CHECKS FOR CONSISTENCY in foldFNdecl?
//IF FUNCTION ARG TYPE IS NOT A VALUE JUST RETURN ANYTYPE

//NodeEntry.folded_functieon_declaration

//MODIFY:
//fold function decl
//Scope.Container

//on fold double() call we should produce  new function variant!
//custom POInter alignment

//decl scope initializers must be comptime known

//BIG TODO: functions, control flow, imports
//TODO: proper int/float literal parsing

//TODO: if arg_count=0 function_type.id could just be Type
//with no function_type_elems involved

//bin op assignments( a *= b, c -= 1 )

//TODO: foldInsignificant
//(to avoid 'const a = undecl_id' in the middle of the function)
//(dont go into inner functions)

const std = @import("std");
const util = @import("util.zig");
const zigbuiltin = @import("builtin");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");
const error_message = @import("errorMessage.zig");
const hgsl = @import("root.zig");

const u8x4 = @Vector(4, u8);
const u16x4 = @Vector(4, u16);
const u32x4 = @Vector(4, u32);
const u64x4 = @Vector(4, u64);
tokenizer: Tokenizer = undefined,
allocator: Allocator,

types: List(TypeEntry) = .empty,
function_type_elems: List(Type) = .empty,

number_values: List(u128) = .empty,
x8_vectors: List(u8x4) = .empty,
x16_vectors: List(u16x4) = .empty,
x32_vectors: List(u32x4) = .empty,
x64_vectors: List(u64x4) = .empty,
matrix_values: List(u8) = .empty,

functions: List(FunctionEntry) = .empty,
structs: List(StructEntry) = .empty,

scopes: List(ScopeEntry) = .empty,
current_scope: Scope = .root_source_file,
context_scope: ?Scope = null,

token: Token = 0,

error_info: ErrorInfo = .unknown,

//each entry should store Scope to allow recursion
dependency_stack: List(Token) = .empty,

//"output"
entry_points: List(EntryPointInfo) = .empty,
global_push_constant_count: usize = 0,

push_constants: List(PushConstantInfo) = .empty,

pub fn dump(self: *Parser) void {
    // std.debug.print("push constant infos({d}, global: {d}):\n", .{
    //     self.push_constants.items.len,
    //     self.global_push_constant_count,
    // });
    // for (self.push_constants.items) |pc_info| std.debug.print("--- {s}: {f}\n", .{
    //     self.tokenizer.slice(pc_info.name),
    //     FatType{ .self = self, .type = pc_info.type },
    // });

    // std.debug.print("entry point infos({d}):\n", .{self.entry_points.items.len});
    // for (self.entry_points.items) |ep_info|
    //     std.debug.print("---(fn:{d}) {s} {s}[pc = {d}..{d}]\n", .{
    //         @intFromEnum(ep_info.function),
    //         @tagName(ep_info.stage_info),
    //         self.tokenizer.slice(ep_info.name),
    //         ep_info.local_push_constant_offset,
    //         ep_info.local_push_constant_offset + ep_info.local_push_constant_count,
    //     });
    // std.debug.print("function type elem:\n", .{});
    // for (self.function_type_elems.items) |t| std.debug.print("--- {}\n", .{t});
    // std.debug.print("types:\n", .{});
    // for (self.types.items) |t| std.debug.print("--- {any}\n", .{t});
    // std.debug.print("structs:\n", .{});
    // for (self.structs.items) |t| std.debug.print("--- {any}\n", .{t});

    std.debug.print("---BODY: \n", .{});
    self.current_scope = .root_source_file;
    self.dumpCurrentScope(false, true);

    // if (true) //
    if (false) //
        for (1..self.scopes.items.len) |i| {
            const s: Scope = @enumFromInt(i);
            self.current_scope = s;
            std.debug.print("SCOPE[{d}], nodes: {d}\n", .{ i, self.getScopeEntry(s).body.items.len });
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
            std.debug.print("\t{f}\n", .{DebugNode{
                .self = self,
                .node = &node,
            }});
        }
    }
}

pub fn new(allocator: Allocator) Error!Parser {
    const types_initial_capacity = 16;
    const scalar_values_initial_capacity = 16;
    var self: Parser = .{
        .allocator = allocator,
    };

    self.dependency_stack = try .initCapacity(allocator, 16);
    self.types = try .initCapacity(allocator, types_initial_capacity);
    self.number_values = try .initCapacity(allocator, scalar_values_initial_capacity);

    self.types.appendAssumeCapacity(.type);

    return self;
}
pub fn deinit(self: *Parser) void {
    self.types.deinit(self.allocator);
    self.functions.deinit(self.allocator);
    self.structs.deinit(self.allocator);
    self.scopes.deinit(self.allocator);
    self.dependency_stack.deinit(self.allocator);

    self.number_values.deinit(self.allocator);
    self.x8_vectors.deinit(self.allocator);
    self.x16_vectors.deinit(self.allocator);
    self.x32_vectors.deinit(self.allocator);
    self.x64_vectors.deinit(self.allocator);
    self.matrix_values.deinit(self.allocator);

    self.push_constants.deinit(self.allocator);
    self.entry_points.deinit(self.allocator);
}

pub fn parse(self: *Parser, tokenizer: Tokenizer) Error!void {
    self.tokenizer = tokenizer;

    const s = try self.parseFile(tokenizer);
    self.current_scope = self.getStructEntry(s).scope;
    try self.foldScope(self.current_scope);

    // try self.gatherEntryPointInfos();
}

fn gatherEntryPointInfos(self: *Parser) Error!void {
    self.current_scope = .root_source_file;
    var statement: Node = 0;
    const body = self.getBodySlice();
    while (statement < body.len) {
        defer statement += self.nodeConsumptionUnscoped(statement);
        if (body[statement] != .folded_var_decl) continue;
        const var_decl = body[statement].folded_var_decl;

        const qualifier_info_node = statement + 1;
        const type_node = qualifier_info_node + self.nodeConsumptionUnscoped(qualifier_info_node);
        const initializer_node = type_node + self.nodeConsumptionUnscoped(type_node);

        switch (var_decl.qualifier) {
            .push => {
                const pc_info: PushConstantInfo = .{
                    .name = var_decl.name,
                    .type = @enumFromInt(self.getValuePayloadUnscoped(type_node)),
                };
                try self.push_constants.append(self.allocator, pc_info);
                self.global_push_constant_count += 1;
            },
            else => |q| if (@as(?hgsl.ShaderStageInfo, switch (q) {
                .vertex => .vertex,
                .fragment => .fragment,
                .compute => .{ .compute = @splat(1) },
                else => null,
            })) |si| {
                const ep_info: EntryPointInfo = .{
                    .name = var_decl.name,
                    .function = @enumFromInt(self.getValuePayloadUnscoped(initializer_node)),
                    .stage_info = si,
                    // .workgroup_size = getvalue(qual_info)
                };
                try self.entry_points.append(self.allocator, ep_info);
            },
        }
    }
    for (self.entry_points.items) |*ep_info| {
        var pc_count: usize = 0;
        ep_info.local_push_constant_offset = self.push_constants.items.len;
        defer ep_info.local_push_constant_count = pc_count;

        self.current_scope = self.getFunctionEntry(ep_info.function).scope;

        statement = 0;
        const ep_body = self.getBodySlice();
        while (statement < ep_body.len) {
            defer statement += self.nodeConsumptionUnscoped(statement);

            if (ep_body[statement] != .folded_var_decl) continue;
            const var_decl = ep_body[statement].folded_var_decl;

            if (var_decl.qualifier != .push) continue;

            pc_count += 1;
            const type_node = statement + 1 + self.nodeConsumptionUnscoped(statement + 1);
            try self.push_constants.append(self.allocator, .{
                .name = var_decl.name,
                .type = @enumFromInt(self.getValuePayloadUnscoped(type_node)),
            });
        }
    }
}

const EntryPointInfo = struct {
    name: Token,
    function: Function,

    local_push_constant_offset: usize = undefined,
    local_push_constant_count: usize = undefined,

    stage_info: hgsl.ShaderStageInfo,
};

const PushConstantInfo = struct {
    name: Token,
    type: Type,
};

fn foldScope(self: *Parser, scope: Scope) Error!void {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = scope;

    if (self.getScopeEntry(self.current_scope).container.isDecl())
        try self.foldDeclScope()
    else
        try self.foldBlockScope(0);
}

fn foldBlockScope(self: *Parser, node: Node) Error!void {
    //fold in reverse order
    const consumption = self.nodeConsumptionUnscoped(node);
    const next_node = node + consumption;
    if (self.getBodySlice().len > next_node) try self.foldBlockScope(next_node);

    if (self.isStatementSignificantBlockScope(node))
        try self.foldNode(node, .value);
}

fn foldDeclScope(self: *Parser) Error!void {
    const body = self.getBodySlice();
    var i: Node = 0;
    while (i < body.len) {
        if (self.isStatementSignificantDeclScope(i))
            try self.foldNode(i, .value);
        i += self.nodeConsumptionUnscoped(i);
    }
}

fn isStatementSignificantDeclScope(self: *Parser, node: Node) bool {
    // if (true) return true;
    return switch (self.getNodeEntryUnscoped(node).*) {
        .var_decl => |var_decl| //
        var_decl.qualifier.isEntryPoint() or var_decl.qualifier == .push,
        else => false,
    };
}

fn isStatementSignificantBlockScope(self: *Parser, node: Node) bool {
    // if (true) return true;
    return switch (self.getNodeEntryUnscoped(node).*) {
        .var_decl => |var_decl| var_decl.qualifier == .push,
        .@"return", .assignment => true,
        else => false,
    };
}

pub fn getValue(self: *Parser, scope: Scope, node: Node) Error!?Value {
    return switch (self.getNodeEntry(scope, node).*) {
        .value => |value| value,
        .function_declaration => |fn_decl| .{
            .type = try self.typeOf(scope, node),
            .payload = @intFromEnum(fn_decl.function),
            .token = fn_decl.token,
        },
        else => null,
    };
}

pub inline fn getValuePayloadUnscoped(self: *Parser, node: Node) u32 {
    return self.getValuePayload(self.current_scope, node);
}
pub fn getValuePayload(self: *Parser, scope: Scope, node: Node) u32 {
    return switch (self.getNodeEntry(scope, node).*) {
        .value => |value| value.payload,
        .function_declaration => |fn_decl| @intFromEnum(fn_decl.function),
        else => unreachable,
    };
}

const FoldMode = enum { //union(enum)
    force,
    value,
    reference,
    declaration,
};
fn foldNode(self: *Parser, node: Node, mode: FoldMode) Error!void {
    switch (self.getNodeEntryUnscoped(node).*) {
        .call => |fn_call| try self.foldCall(fn_call, node, mode),
        .function_declaration => |fn_decl| try self.foldFunctionDeclaration(fn_decl, node),
        .function_type_declaration => |fn_type_decl| try self.foldFunctionTypeDeclaration(fn_type_decl, node, mode),
        .indexing => {
            const target_node = node + 1;
            try self.foldNode(target_node, mode);
            try self.foldNode(target_node + self.nodeConsumptionUnscoped(target_node), mode);
        },
        .constructor => |constructor| {
            const type_node = node + 1;
            try self.foldNode(type_node, mode);
            const @"type" = try self.asType(type_node);

            const first_elem_node = type_node + self.nodeConsumptionUnscoped(type_node);
            if (@"type" != .@"anytype") switch (constructor.elem_count) {
                0 => {},
                1 => try self.foldCast(@"type", first_elem_node, constructor.token, node, mode),
                else => try self.foldConstructor(@"type", constructor, first_elem_node, mode),
            };
        },
        .u_op => |op| try self.foldUOp(op, node, mode),
        .identifier => |token| try self.foldIdentifier(node, token, mode),

        .var_decl => |var_decl| try self.foldVariableDeclaration(var_decl, node),
        .@"return" => |token| try self.foldReturn(node, token),
        .assignment => |token| try self.foldAssignment(node, token),
        else => |e| {
            _ = e;
            // std.debug.print("idk how to fold {s}\n", .{@tagName(e)});
        },
    }
}

fn foldCall(self: *Parser, fn_call: Call, node: Node, mode: FoldMode) Error!void {
    const callee_node = node + 1;
    try self.foldNode(callee_node, mode);

    const callee_type = try self.typeOf(self.current_scope, callee_node);
    if (self.getTypeEntry(callee_type) != .function)
        return self.errorOut(.{
            .token = self.getNodeToken(callee_node),
            .payload = .{ .unexpected_type_tag = .{ .got = callee_type, .expected = .function } },
        });

    // std.debug.print("FOLD FNCALL< CALLEE TYPE: {f}\n", .{DebugType{ .self = self, .type = callee_type }});
    std.debug.print("FOLD FNCALL< CALLEE: {f}\n", .{DebugNodeU{ .self = self, .node = callee_node }});
    const function_type_entry = self.getTypeEntry(callee_type).function;
    const arg_types = self.function_type_elems.items[function_type_entry.id .. function_type_entry.id + function_type_entry.arg_count];

    const first_arg_node = callee_node + self.nodeConsumptionUnscoped(callee_node);
    var arg_node = first_arg_node;

    if (fn_call.arg_count != arg_types.len)
        return self.errorOut(.{
            .token = fn_call.token,
            .payload = .{ .argument_count_mismatch = .{
                .got = fn_call.arg_count,
                .expected = @truncate(arg_types.len),
            } },
        });

    for (0..fn_call.arg_count) |i| {
        // std.debug.print("FOLD ARG NODE< NODE: {f}\n", .{DebugNodeU{ .self = self, .node = arg_node }});
        try self.foldNode(arg_node, mode);
        try self.implicitCast(arg_node, arg_types[i]);
        arg_node += self.nodeConsumptionUnscoped(arg_node);
    }
    switch (self.getNodeEntryUnscoped(callee_node).*) {
        .builtin => |builtin_node| //
        try self.foldBuiltinCall(builtin_node.builtin, first_arg_node, node, mode),
        else => try self.foldFunctionCall(
            @enumFromInt(self.getValuePayloadUnscoped(callee_node)),
            node,
            first_arg_node,
        ),
        // else => try foldFunctionCall( fn_call: (unknown type), node: (unknown type), mode: FoldMode),
    }
    //call in declaration cant be replaced
}

fn foldFunctionCall(
    self: *Parser,
    function: Function,
    node: Node,
    first_arg_node: Node,
) Error!void {
    //(T0, A1, A2, T3) ?RT
    //(T0, T1, T3, T4) RT
    //if(there is no  anytypes)(dispatch if all args are comptime)
    //fill all anytypes with actual types
    //create function permutation
    //fill all anytype arg types with @TypeOf(args[i])

    //fold new decl
    _ = .{ self, function, node, first_arg_node };
}
fn foldBuiltinCall(self: *Parser, builtin: Builtin, first_arg_node: Node, node: Node, mode: FoldMode) Error!void {
    const node_consumption = self.nodeConsumptionUnscoped(node);
    switch (builtin) {
        .TypeOf => {
            const @"type" = try self.typeOf(self.current_scope, first_arg_node);
            std.debug.print("===| TYPE OF : {f}\n", .{DebugType{ .self = self, .type = @"type" }});
            if (mode == .declaration and @"type" == .@"anytype")
                return;
            self.replaceNodeWithValue(node, node_consumption, .{
                .type = .type,
                .payload = @intFromEnum(@"type"),
            });
        },

        else => unreachable,
    }
}

fn foldIdentifier(self: *Parser, node: Node, token: Token, mode: FoldMode) Error!void {
    const var_ref = try self.getVariableReference(self.current_scope, node, token) orelse
        (if (mode == .declaration) self.getVariableReferenceNodeContextScope(self.context_scope.?, token) else null) orelse
        return self.errorOut(.{ .token = token, .payload = .undeclared_identifier });

    self.getNodeEntryUnscoped(node).* = if (try self.getVariableReferenceValue(var_ref)) |value|
        .{ .value = value }
    else
        .{ .var_ref = var_ref };
}

fn foldAssignment(self: *Parser, node: Node, token: Token) Error!void {
    const target_node = node + 1;
    try self.foldNode(target_node, .reference);
    if (!self.isValidAssignmentTarget(target_node))
        return self.errorOut(.{
            .token = token,
            .payload = .invalid_assignment_target,
        });

    const value_node = target_node + self.nodeConsumptionUnscoped(target_node);
    try self.foldNode(value_node, .value);
    const target_type = try self.typeOf(self.current_scope, target_node);
    try self.implicitCast(value_node, target_type);
}

fn foldReturn(self: *Parser, node: Node, token: Token) Error!void {
    try self.foldNode(node + 1, .value);
    const rt = try self.currentScopeReturnTypeAndDeclLocation(node);

    if (self.getNodeEntryUnscoped(node + 1).* == .null) {
        if (self.getTypeEntry(rt[0]) != .void)
            return self.errorOut(.{
                .token = token,
                .payload = .{ .missing_return_value = rt[1] },
            });
    } else try self.implicitCast(node + 1, rt[0]);
}
fn foldFunctionTypeDeclaration(self: *Parser, fn_type_decl: FunctionTypeDeclaration, node: Node, mode: FoldMode) Error!void {
    const arg_count = fn_type_decl.arg_count;
    var type_node = node + 1;
    for (0..arg_count + 1) |_| {
        try self.foldNode(type_node, mode);
        _ = try self.asType(type_node);
        type_node += self.nodeConsumptionUnscoped(type_node);
    }

    const fte = self.function_type_elems.items;

    const function_type: TypeEntry.FunctionType = blk: {
        for (0..fte.len) |i| {
            var arg_type_node: Node = node + 1;
            if (i + arg_count >= fte.len) break;
            for (0..arg_count) |j| {
                defer arg_type_node += self.nodeConsumptionUnscoped(arg_type_node);

                const arg_type = self.asTypeOpt(self.current_scope, arg_type_node).?;
                if (arg_type != fte[i + j]) break;
            } else if (self.asTypeOpt(self.current_scope, arg_type_node).? == fte[i + arg_count])
                break :blk .{ .id = @truncate(i), .arg_count = arg_count };
            //check rtype
        }

        type_node = node + 1; //append arg_types to the function_type_elems
        try self.function_type_elems.ensureUnusedCapacity(self.allocator, arg_count + 1);
        const function_type_id: u32 = @truncate(self.function_type_elems.items.len);
        for (0..arg_count + 1) |_| {
            self.function_type_elems.appendAssumeCapacity(try self.asType(type_node));
            type_node += self.nodeConsumptionUnscoped(type_node);
        }
        break :blk .{ .id = function_type_id, .arg_count = arg_count };
    };
    self.replaceNodeWithValue(node, type_node - node, .{
        .type = .type,
        .payload = @intFromEnum(try self.addType(.{ .function = function_type })),
    });
}
fn foldFunctionDeclaration(self: *Parser, fn_decl: FunctionDeclarationNode, node: Node) Error!void {
    const function_entry = self.getFunctionEntry(fn_decl.function);
    self.context_scope = function_entry.scope;
    defer self.context_scope = null;

    const arg_count = function_entry.arg_count;

    var is_consistent = true;
    var arg_type_node: Node = node + 2;

    for (0..arg_count) |_| {
        const consumption = self.nodeConsumptionUnscoped(arg_type_node);
        defer arg_type_node += 1 + consumption;

        try self.foldNode(arg_type_node, .declaration);

        switch (self.getNodeEntryUnscoped(arg_type_node).*) {
            .null, .@"anytype" => is_consistent = false,
            else => _ = try self.asType(arg_type_node),
        }
    }
    const rtype_node = arg_type_node - 1;
    try self.foldNode(rtype_node, .declaration);
    const rtype = self.asTypeOpt(self.current_scope, rtype_node) orelse
        if (try self.typeOf(self.current_scope, rtype_node) == .type)
            .@"anytype"
        else
            return self.errorOutNotAType(rtype_node);
    if (rtype == .@"anytype") is_consistent = false;

    if (arg_count > 0)
        self.fillInferredFunctionDeclarationArgumentTypes(node + 2, 0, arg_count, rtype);

    function_entry.flags.is_consistent = is_consistent;
    if (is_consistent)
        try self.foldScope(function_entry.scope);
}

//this might break on arg types/rtypes not known at declaration
fn fillInferredFunctionDeclarationArgumentTypes(
    self: *Parser,
    arg_type_node: Node,
    arg_index: usize,
    arg_count: u32,
    rtype: Type,
) void {
    if (arg_index >= arg_count) return;

    const consumption = self.nodeConsumptionUnscoped(arg_type_node);
    const entry = self.getNodeEntryUnscoped(arg_type_node);
    if (arg_index + 1 == arg_count) { //if last index get rtype
        if (entry.* == .null) {
            //argument type inferred from rtype must be comptime known??
            entry.* = .{ .value = .{ .type = .type, .payload = @intFromEnum(rtype) } };
        }
        return;
    }
    const next_arg_type_node = arg_type_node + 1 + consumption;
    self.fillInferredFunctionDeclarationArgumentTypes(next_arg_type_node, arg_index + 1, arg_count, rtype);
    if (entry.* == .null)
        entry.* = self.getNodeEntryUnscoped(next_arg_type_node).*;
}

fn foldCast(
    self: *Parser,
    @"type": Type,
    value_node: Node,
    token: Token,
    node: Node,
    mode: FoldMode,
) Error!void {
    try self.foldNode(value_node, mode);
    const value_type = try self.typeOf(self.current_scope, value_node);

    if (!self.isTypeExplicitlyCastable(value_type, @"type"))
        return self.errorOut(.{ .token = token, .payload = .{
            .invalid_cast = .{ .from = value_type, .to = @"type" },
        } });

    //if value is comptime known cast it to the desired type
    //replacing .{.constructor = ...} with just 'value'
    if (try self.getValue(self.current_scope, value_node)) |value| {
        self.replaceNodeWithValue(
            node,
            self.nodeConsumptionUnscoped(node),
            try self.castValue(@"type", .{
                .payload = value.payload,
                .type = value.type,
                .token = token,
            }),
        );
    }
}
fn castValue(self: *Parser, @"type": Type, value: Value) Error!Value {
    const to_entry = self.getTypeEntry(@"type");
    const from_entry = self.getTypeEntry(value.type);
    return if (value.type == @"type") value else .{
        .type = @"type",
        .token = value.token,
        .payload = switch (to_entry) {
            //TODO: enum, ptr values to scalar
            .bool, .compint, .compfloat, .scalar => //
            try self.addNumberValue(self.numberValueCast(
                value.payload,
                from_entry,
                to_entry,
            )),
            .vector => |vector| switch (from_entry) {
                .vector => unreachable,
                else => try self.addVectorValueSplat(
                    vector,
                    (try self.castValue(
                        try self.addType(.{ .scalar = vector.scalar }),
                        value,
                    )).payload,
                ),
            },
            //if from - vector => element wise
            //else splat the value
            else => unreachable,
        },
    };
}
fn numberValueCast(self: *Parser, id: u32, from_entry: TypeEntry, to_entry: TypeEntry) u128 {
    @setEvalBranchQuota(10_000);
    const bits = self.getNumberValue(id);
    inline for (all_number_type_entries) |fe| {
        if (activeTag(from_entry) == activeTag(fe) and (fe != .scalar or from_entry.scalar.eql(fe.scalar))) {
            inline for (all_number_type_entries) |te| {
                if (activeTag(to_entry) == activeTag(te) and (te != .scalar or to_entry.scalar.eql(te.scalar))) {
                    return util.fit(
                        u128,
                        util.numberCast(
                            te.ToZig(),
                            util.extract(fe.ToZig(), bits),
                        ),
                    );
                }
            }
        }
    }
    unreachable;
}
const all_scalar_type_entries: []const TypeEntry = blk: {
    var slice: []const TypeEntry = &.{ .bool, .compint, .compfloat };
    for (&util.allEnumValues(TypeEntry.Scalar.Layout)) |sl|
        for (&util.allEnumValues(TypeEntry.Scalar.Width)) |sw| {
            if (!(sl == .float and sw == ._8))
                slice = slice ++ &[_]TypeEntry{.{ .scalar = .{ .layout = sl, .width = sw } }};
        };
    break :blk slice;
};
const all_number_type_entries: []const TypeEntry = //
    &[_]TypeEntry{ .bool, .compint, .compfloat } ++ all_scalar_type_entries;

fn foldConstructor(
    self: *Parser,
    @"type": Type,
    constructor: ConstructorNode,
    first: Node,
    mode: FoldMode,
) Error!void {
    const constructor_structure = try self.constructorStructure(@"type");
    if (!constructor_structure.canHaveConstructor())
        return self.errorOut(.{
            .token = constructor.token,
            .payload = .{ .type_cant_have_constructor = @"type" },
        });

    var elem = first;
    var occupied: u32 = 0;
    //check if constuctor elements have compatible types
    for (0..constructor.elem_count) |_| {
        try self.foldNode(elem, mode);
        const elem_consumption = self.nodeConsumptionUnscoped(elem);
        defer elem += elem_consumption;

        const elem_type = try self.typeOf(self.current_scope, elem);
        if (self.isTypeExplicitlyCastable(elem_type, constructor_structure.element)) {
            occupied += 1;
            const elem_entry = self.getNodeEntryUnscoped(elem);
            if (elem_entry.* == .value)
                elem_entry.* = .{ .value = try self.castValue(constructor_structure.element, elem_entry.value) };
        } else {
            const elem_constructor_structure = try self.constructorStructure(elem_type);
            //check if elem_cs.element and cs.element have the same type depth??
            if (self.isTypeExplicitlyCastable(
                elem_constructor_structure.element,
                constructor_structure.element,
            )) {
                occupied += elem_constructor_structure.len;
            } else return self.errorOut(.{
                .token = self.getNodeEntryUnscoped(elem).token(),
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
    const entry = self.getNodeEntryUnscoped(node).*;
    return switch (entry) {
        else => true,
    };
}
fn currentScopeReturnTypeAndDeclLocation(self: *Parser, node: Node) Error!@Tuple(&.{ Type, Token }) {
    var scope = self.current_scope;
    while (true) {
        const entry = self.getScopeEntry(scope).*;
        if (entry.container.isDecl()) return self.errorOut(.{
            .token = self.getNodeEntryUnscoped(node).token(),
            .payload = .return_outside_function,
        });
        if (entry.container == .function) {
            const decl_token = self.getNodeEntry(
                entry.parent,
                self.getFunctionEntry(entry.container.function).node,
            ).token();
            return .{ try self.getFunctionDeclarationReturnType(entry.container.function, entry.parent), decl_token };
        } else {
            scope = entry.parent;
            continue;
        }
    } else unreachable;
}
fn getFunctionDeclarationReturnType(self: *Parser, function: Function, parent_scope: Scope) Error!Type {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = parent_scope;

    const entry = self.getFunctionEntry(function).*;
    const node = entry.node + 1 + self.nodeSequenceConsumption(self.current_scope, entry.node + 1, entry.arg_count);
    try self.foldNode(node, .value);

    const rtype_node = self.getNodeEntryUnscoped(node).*;
    if (rtype_node != .value or rtype_node.value.type != .type)
        return self.errorOut(.{
            .token = rtype_node.token(),
            .payload = .unable_to_resolve_comptime_value,
        });
    return @enumFromInt(rtype_node.value.payload);
}
fn foldVariableDeclaration(self: *Parser, var_decl: VariableDeclaration, node: Node) Error!void {
    self.getNodeEntryUnscoped(node).* = .{ .folded_var_decl = var_decl };

    //check for redelaration
    if (try self.getVariableReference(self.current_scope, node, var_decl.name)) |vr| {
        var decls: [2]Token = .{ var_decl.qualifier_token, self.getNodeEntry(vr.scope, vr.node).token() };
        if (decls[1] < decls[0] or !self.isScopeParentOfCurrent(vr.scope))
            std.mem.swap(Token, &decls[0], &decls[1]);

        return self.errorOut(.{
            .token = decls[1],
            .payload = .{ .redeclaration = .{
                .statement = decls[0],
                .name = var_decl.name,
            } },
        });
    }

    try self.dependency_stack.append(self.allocator, var_decl.name);
    defer self.dependency_stack.items.len -= 1; //pop

    const qualifier_info_node = node + 1;
    try self.foldNode(qualifier_info_node, .value);

    const qualifier_info_node_consumption = self.nodeConsumptionUnscoped(node + 1);
    const type_node = node + 1 + qualifier_info_node_consumption;
    try self.foldNode(type_node, .value);

    const type_node_entry = self.getNodeEntryUnscoped(type_node).*;
    if (type_node_entry != .null) {
        if (type_node_entry != .value)
            return self.errorOut(.{
                .token = type_node,
                .payload = .unable_to_resolve_comptime_value,
            });
        const type_of_type_node = try self.typeOf(self.current_scope, type_node);
        if (type_of_type_node != .type)
            return self.errorOutNotAType(type_node);
    }

    const type_node_consumption = self.nodeConsumptionUnscoped(type_node);
    const initializer_node = type_node + type_node_consumption;
    try self.foldNode(initializer_node, .value);

    //if type is <null> inferr type from initializer
    //else implicitly cast initializer to variable type
    if (type_node_entry != .null) {
        try self.implicitCast(initializer_node, try self.asType(type_node));
    } else {
        self.getNodeEntryUnscoped(type_node).* = .{ .value = .{
            .type = .type,
            .payload = @intFromEnum(try self.typeOf(self.current_scope, initializer_node)),
        } };
    }
    const t: Type = @enumFromInt(self.getNodeEntryUnscoped(type_node).value.payload);

    if (!self.isQualifierCompatibleWithType(var_decl.qualifier, t))
        return self.errorOut(.{
            .token = var_decl.qualifier_token,
            .payload = .{ .qualifier_incompatible_with_type = .{
                .qualifier = var_decl.qualifier,
                .type = t,
            } },
        });
}

fn foldUOp(self: *Parser, u_op: UOpNode, node: Node, mode: FoldMode) Error!void {
    // std.debug.print("FOLD UOP: {}\n", .{u_op.op});
    const target = node + 1;
    try self.foldNode(target, mode);
    const target_consumption = self.nodeConsumptionUnscoped(target);
    switch (u_op.op) {
        .pointer => {
            const as_type = try self.asType(target);
            const pointer_type = try self.addType(.{ .pointer = as_type });
            self.replaceNodeWithValue(node, target_consumption + 1, .{
                .type = .type,
                .payload = @intFromEnum(pointer_type),
            });
        },
        else => @panic("FOLD U OP"),
    }
}
fn isQualifierCompatibleWithType(self: *Parser, qualifier: Qualifier, @"type": Type) bool {
    if (@"type" == .@"anytype") return true;
    const entry = self.getTypeEntry(@"type");
    return switch (qualifier) {
        .env, .push, .workgroup => !entry.isComptime(),
        .vertex, .fragment, .compute => entry == .function,
        else => true,
    };
}

pub fn getVariableReferenceQualifier(self: *Parser, var_ref: VariableReference) Qualifier {
    const entry = self.getNodeEntry(var_ref.scope, var_ref.node).*;
    return switch (entry) {
        .var_decl, .folded_var_decl => |vd| vd.qualifier,
        else => .@"const",
    };
}
pub fn getVariableReferenceValue(self: *Parser, var_ref: VariableReference) Error!?Value {
    const value_node = switch (self.getNodeEntry(var_ref.scope, var_ref.node).*) {
        .var_decl, .folded_var_decl => |vd| if (vd.qualifier == .@"const")
            var_ref.node + 1 + self.nodeSequenceConsumption(var_ref.scope, var_ref.node + 1, 2)
        else
            return null,
        else => var_ref.node,
    };
    return try self.getValue(var_ref.scope, value_node);
}
fn getVariableReference(self: *Parser, scope: Scope, node: Node, token: Token) Error!?VariableReference {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = scope;

    for (self.dependency_stack.items) |rs| {
        if (rs == token or util.strEql(self.tokenizer.slice(rs), self.tokenizer.slice(token))) {
            return self.errorOut(.{ .token = token, .payload = .dependency_loop });
        }
    }

    const scope_entry = self.getScopeEntry(scope);

    var i: Node = 0; //get scope var ref (move into own function?)
    const cap = if (scope_entry.container.isDecl()) self.getBodySlice().len else node;
    const current: ?Node = while (i < cap) {
        defer i += self.nodeConsumptionUnscoped(i);

        if (i == node and scope_entry.container.isDecl()) continue;
        if (try self.doesStatementContainVariableReference(i, token))
            break i;
    } else null;

    return if (current) |c| .{ .node = c, .scope = scope } else //
    if (self.isScopeFile(scope)) null else //
    try self.getVariableReference(scope_entry.parent, scope_entry.getDeclNode(self), token);
}

fn getVariableReferenceNodeContextScope(self: *Parser, context_scope: Scope, token: Token) ?VariableReference {
    std.debug.print("getVariableReferenceNodeContextScope: {s}\n", .{self.tokenizer.slice(token)});
    const name = self.tokenizer.slice(token);
    const scope_entry = self.getScopeEntry(context_scope);
    return switch (scope_entry.container) {
        .function => |function| blk: {
            const entry = self.getFunctionEntry(function);
            const decl_node = entry.node;

            var param_node = decl_node + 1;
            break :blk for (0..entry.arg_count) |_| {
                const param = self.getNodeEntryUnscoped(param_node).function_parameter;
                const param_name = self.tokenizer.slice(param.name);
                if (util.strEql(name, param_name))
                    break .{
                        .node = param_node,
                        .scope = scope_entry.parent,
                    };

                param_node += 1 + self.nodeConsumptionUnscoped(param_node + 1);
            } else null;
        },
        else => null,
    };
}

fn doesStatementContainVariableReference(self: *Parser, node: Node, token: Token) Error!bool {
    const name = self.tokenizer.slice(token);
    return switch (self.getNodeEntryUnscoped(node).*) {
        .var_decl, .folded_var_decl => |vd| if (util.strEql(
            name,
            self.tokenizer.slice(vd.name),
        )) {
            try self.foldNode(node, .value);
            return true;
        } else false,
        else => false,
    };
}
fn implicitCast(self: *Parser, node: Node, @"type": Type) Error!void {
    const from = try self.typeOf(self.current_scope, node);
    if (from == @"type") return;
    if (!self.isTypeImplicitlyCastable(from, @"type")) {
        return self.errorOut(.{
            .token = self.getNodeEntryUnscoped(node).token(),
            .payload = .{ .cant_implicitly_cast = .{
                .from = from,
                .to = @"type",
            } },
        });
    }

    const entry = self.getNodeEntryUnscoped(node);
    switch (entry.*) {
        .constructor => |elem_count| {
            _ = elem_count;
            const type_node = node + 1;
            const type_node_entry = self.getNodeEntryUnscoped(type_node);
            const changed: bool = switch (type_node_entry.*) {
                .null => true,
                .value => false,
                else => return self.errorOut(.unknown),
            };
            type_node_entry.* = .{ .value = .{
                .type = .type,
                .payload = @intFromEnum(@"type"),
            } };
            if (changed) try self.foldNode(node, .value);
        },
        // inline else => |_, tag| @panic("implicit cast ts: " ++ @tagName(tag)),
        else => {},
        // constructor: u32, //[count][type][elems..]

    }
    _ = .{ self, node, @"type" };
}
fn isTypeExplicitlyCastable(self: *Parser, from: Type, to: Type) bool {
    if (from == to or from == .@"anytype") return true;
    const from_entry = self.getTypeEntry(from);
    const to_entry = self.getTypeEntry(to);

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
        .vector => from_is_number or
            from_entry == .vector,
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
    if (from == to or from == .@"anytype" or to == .@"anytype") return true;
    const from_entry = self.getTypeEntry(from);
    const to_entry = self.getTypeEntry(to);

    return switch (to_entry) {
        .scalar => from_entry == .scalar or
            from_entry == .compint or
            from_entry == .compfloat,
        else => false,
    };
}

pub fn sizeOf(self: *Parser, @"type": Type) u32 {
    const entry = self.getTypeEntry(@"type");
    return switch (entry) {
        .scalar => |scalar| scalar.width.value(),
        .vector => |vector| vector.scalar.width.value() / 8 * vector.len.value(),
        inline else => |_, tag| @panic(@tagName(tag)),
    };
}

pub fn typeOf(self: *Parser, scope: Scope, node: Node) Error!Type {
    return switch (self.getNodeEntry(scope, node).*) {
        .@"anytype" => .type,
        .builtin => |builtin_node| try self.typeOfBuiltin(builtin_node.builtin),
        .value => |value| value.type,
        .bin_op => |bin_op| try self.typeOfBinOp(
            scope,
            bin_op.op,
            try self.typeOf(scope, node + 1),
            try self.typeOf(scope, node + 1 + self.nodeConsumption(scope, node + 1)),
        ),
        .u_op => |u_op| try self.typeOfUOp(
            scope,
            u_op.op,
            try self.typeOf(scope, node + 1),
        ),
        .var_ref => |var_ref| try self.typeOfVariableReference(var_ref),

        //return type of target for now
        .indexing => blk: {
            //fold indexing
            const target_type = try self.typeOf(scope, node + 1);
            const entry = self.getTypeEntry(target_type);
            break :blk switch (entry) {
                .pointer => |pointed| pointed,
                else => target_type,
            };
        },
        .function_type_declaration => .type,
        //store function type in the FunctionEntry
        .function_declaration => |fn_decl| try self.typeOfFunctionDeclaration(scope, fn_decl, node),
        .constructor => return self.asTypeOpt(scope, node + 1) orelse
            .@"anytype",

        .null => .@"anytype",
        .call => blk: {
            std.debug.print("FUNCTIONCALL: {f}\n", .{DebugNodeU{ .self = self, .node = node }});
            const function_node = node + 1;
            const function_type = try self.typeOf(scope, function_node);
            const ftype = self.getTypeEntry(function_type).function;

            const rtype = self.function_type_elems.items[ftype.id + ftype.arg_count];
            break :blk rtype;
        },
        else => |e| {
            std.debug.print("idk type of '{s}'\n", .{@tagName(e)});
            return self.errorOut(.unknown);
        },
    };
}

fn typeOfBuiltin(self: *Parser, builtin: Builtin) Error!Type {
    // position, // f32x4
    // vertex_id, // u32
    // TypeOf, //fn(anytype)type
    return switch (builtin) {
        .TypeOf => try self.addType(.{
            .function = try self.addFunctionType(&.{.@"anytype"}, .type),
        }),
        .vertex_id => try self.addType(.{
            .scalar = .{ .layout = .uint, .width = ._32 },
        }),
        .position => try self.addType(.{ .vector = .{
            .len = ._4,
            .scalar = .{ .layout = .float, .width = ._32 },
        } }),
        // else => unreachable,
    };
}

pub fn typeOfVariableReference(self: *Parser, var_ref: VariableReference) Error!Type {
    const decl_entry = self.getNodeEntry(var_ref.scope, var_ref.node).*;
    const node = switch (decl_entry) {
        .var_decl, .folded_var_decl => //
        var_ref.node + 1 + self.nodeConsumption(var_ref.scope, var_ref.node + 1),
        .function_parameter => var_ref.node + 1,
        else => unreachable,
    };

    return try self.asTypeScope(var_ref.scope, node);
}
fn typeOfFunctionDeclaration(self: *Parser, scope: Scope, fn_decl: FunctionDeclarationNode, node: Node) Error!Type {
    const fte = self.function_type_elems.items;
    const arg_count = self.getFunctionEntry(fn_decl.function).arg_count;

    const function_type: TypeEntry.FunctionType = blk: {
        //check the element list for matches
        for (0..fte.len) |i| {
            var arg_type_node: Node = node + 2;
            if (i + arg_count >= fte.len) break;
            for (0..arg_count) |j| {
                defer arg_type_node += 1 + self.nodeConsumption(scope, arg_type_node);

                const arg_type = self.asTypeOpt(scope, arg_type_node) orelse .@"anytype";
                if (arg_type != fte[i + j]) break;
            } else if (self.asTypeOpt(scope, arg_type_node - 1) orelse .@"anytype" == fte[i + arg_count])
                break :blk .{ .id = @truncate(i), .arg_count = arg_count };
            //check rtype
        }

        //add elements to the functions_type_elems
        try self.function_type_elems.ensureUnusedCapacity(self.allocator, arg_count + 1);
        const function_type_id: u32 = @truncate(self.function_type_elems.items.len);

        var type_node = node + 2; //append arg_types to the function_type_elems
        for (0..arg_count + 1) |i| {
            if (i == arg_count) type_node -= 1;
            self.function_type_elems.appendAssumeCapacity(self.asTypeOpt(scope, type_node) orelse .@"anytype");
            type_node += 1 + self.nodeConsumption(scope, type_node);
        }
        break :blk .{ .id = function_type_id, .arg_count = arg_count };
    };
    return try self.addType(.{ .function = function_type });
}
fn asTypeScope(self: *Parser, scope: Scope, node: Node) Error!Type {
    const last_scope = self.current_scope;
    defer self.current_scope = last_scope;
    self.current_scope = scope;
    return self.asType(node);
}

fn asType(self: *Parser, node: Node) Error!Type {
    if (self.asTypeOpt(self.current_scope, node)) |@"type"|
        return @"type"
    else {
        const type_of = try self.typeOf(self.current_scope, node);

        if (type_of == .type)
            return self.errorOut(.{ .token = self.getNodeToken(node), .payload = .unable_to_resolve_comptime_value })
        else
            return self.errorOutNotAType(node);
    }
}
fn asTypeOpt(self: *Parser, scope: Scope, node: Node) ?Type {
    const entry = self.getNodeEntry(scope, node).*;
    return switch (entry) {
        .@"anytype", .null => .@"anytype",
        .value => |value| if (value.type == .type)
            @enumFromInt(value.payload)
        else if (value.type == .@"anytype")
            .@"anytype"
        else
            null,

        else => null,
    };
}
fn typeOfBinOp(self: *Parser, scope: Scope, op: BinaryOperator, left: Type, right: Type) Error!Type {
    _ = .{ self, right, scope };
    return switch (op) {
        else => left,
    };
}
fn typeOfUOp(self: *Parser, scope: Scope, op: UnaryOperator, operand: Type) Error!Type {
    _ = .{ self, scope };
    return switch (op) {
        else => operand,
    };
}
pub inline fn nodeConsumptionUnscoped(self: *Parser, node: Node) u32 {
    return self.nodeConsumption(self.current_scope, node);
}
pub fn nodeConsumption(self: *Parser, scope: Scope, node: u32) u32 {
    // array_type_decl,
    // function_decl: Function, //[fn_decl][args...][rtype][body...]
    // branch,
    var consumption: u32 = switch (self.getNodeEntry(scope, node).*) {
        .var_decl, .folded_var_decl => 1 + self.nodeSequenceConsumption(scope, node + 1, 3),
        .bin_op, .indexing, .assignment => 1 + self.nodeSequenceConsumption(scope, node + 1, 2),
        .u_op, .@"return", .function_parameter => 1 + self.nodeConsumptionUnscoped(node + 1),
        .null => 1,
        .function_declaration => 1 + self.nodeConsumptionUnscoped(node + 1), //args
        .constructor => |constructor| 1 + self.nodeSequenceConsumption(scope, node + 1, constructor.elem_count + 1),
        inline .call, .function_type_declaration => |fn_type_decl| 1 + self.nodeSequenceConsumption(scope, node + 1, fn_type_decl.arg_count + 1),
        else => 1,
    };
    const body = self.getScopeEntry(scope).body.items;
    while (node + consumption < body.len and body[node + consumption] == .pad) consumption += 1;
    return consumption;
}
pub fn nodeSequenceConsumption(self: *Parser, scope: Scope, node: Node, count: usize) u32 {
    var len: u32 = 0;
    for (0..count) |_|
        len += self.nodeConsumption(scope, node + len);
    return len;
}

fn parseFile(self: *Parser, tokenizer: Tokenizer) Error!Struct {
    const last_tokenizer = self.tokenizer;
    defer self.tokenizer = last_tokenizer;
    self.tokenizer = tokenizer;

    const struct_handle = try self.addStruct(.{ .is_file = true });
    const struct_type = try self.addType(.{ .@"struct" = struct_handle });
    _ = .{struct_type};

    self.getStructEntry(struct_handle).scope = try self.parseScope(.{
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
                .token = self.getNodeEntry(last_scope, self.getScopeEntry(self.current_scope).getDeclNode(self)).token(),
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
    if (self.getScopeEntry(self.current_scope).container.isDecl())
        try self.parseStatementStructDecl()
    else
        try self.parseStatementBlock();
}
fn parseStatementBlock(self: *Parser) Error!void {
    switch (self.tokenizer.kind(self.token)) {
        .@"const", .@"var", .env, .push, .workgroup => |q| {
            //workgroup, push can only be in entry point
            self.token += 1;
            try self.parseVarDecl(.fromTokenKind(q), self.token - 1);
        },
        .fragment, .vertex, .compute => {
            return self.errorOut(.{
                .token = self.token,
                .payload = .qualifier_can_only_be_in_file_scope,
            });
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
        .@"const", .@"var", .env => |q| {
            self.token += 1;
            try self.parseVarDecl(.fromTokenKind(q), self.token - 1);
        },
        .workgroup, .push, .fragment, .vertex, .compute => |q| {
            if (!self.isScopeFile(self.current_scope))
                return self.errorOut(.{
                    .token = self.token,
                    .payload = .qualifier_can_only_be_in_file_scope,
                });
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
        _ = try self.parseExpression2(.{ .kind_or_scope_end = .@"=" });
    } else _ = try self.appendNode(.null);

    if (self.tokenizer.kind(self.token) != .@"=") {
        if (!self.isAtScopeEnd())
            return self.errorOut(.{ .token = self.token, .payload = .unexpected_token });

        _ = try self.appendNode(.null);
        //if both type and initializer are null OR
        //qualifier must have initializer - error out
        if (qualifier.mustHaveInitializer() or self.getNodeEntryUnscoped(type_node).* == .null)
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
    kind_or_scope_end: Tokenizer.TokenKind,
    scope_end,
    pub fn shouldStop(delimiter: ExpressionDelimiter, self: *Parser) bool {
        const current = self.tokenizer.kind(self.token);
        const at_scope_end = current == .endl or
            current == .eof or
            current == .@"}";

        return switch (delimiter) {
            .kind => |kind| current == kind,
            .kind_or_comma => |kind| current == kind or current == .@",",
            .scope_end => at_scope_end,
            .kind_or_scope_end => |kind| at_scope_end or current == kind,
        };
    }
};
pub inline fn isAtScopeEnd(self: *Parser) bool {
    const eos: ExpressionDelimiter = .scope_end;
    return eos.shouldStop(self);
}

inline fn parseExpression2(self: *Parser, delimiter: ExpressionDelimiter) Error!u32 {
    return self.parseBinaryExpressionRecursive(delimiter, 0);
}
fn parseBinaryExpressionRecursive(self: *Parser, delimiter: ExpressionDelimiter, bp: u8) Error!u32 {
    var left_len = try self.parseExpression1(false);

    var iter: usize = 0;
    while (!delimiter.shouldStop(self)) {
        const op = if (Tokenizer.binOpFromTokenKind(self.tokenizer.kind(self.token))) |bop|
            bop
        else
            return self.errorOut(.{ .token = self.token, .payload = .unexpected_token });

        if (Tokenizer.bindingPower(op) <= bp) break;
        self.token += 1;

        _ = self.skipEndl();

        const bin_op_node: NodeEntry = .{ .bin_op = .{ .op = op, .token = self.token - 1 } };
        try self.insertNthLastNode(left_len, bin_op_node);
        iter += 1;

        left_len += 1 + try self.parseBinaryExpressionRecursive(delimiter, Tokenizer.bindingPower(op));
    }
    return left_len;
}
fn replaceNodeWithValue(self: *Parser, node: Node, consumption: u32, value: Value) void {
    @memset(self.getBodySlice()[node + 1 .. node + consumption], .pad);
    self.getNodeEntryUnscoped(node).* = .{ .value = value };
}
fn insertNodeAssumeCapacity(self: *Parser, i: u32, entry: NodeEntry) void {
    self.getScopeEntry(self.current_scope).body.insertAssumeCapacity(i, entry);
}

//insert node at body[len - n]
fn insertNthLastNode(self: *Parser, n: u32, entry: NodeEntry) Error!void {
    const body = &self.getScopeEntry(self.current_scope).body;
    try body.insert(self.allocator, body.items.len - n, entry);
}

fn parseExpression1(self: *Parser, exclude_constructor: bool) Error!u32 {
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
            if (exclude_constructor) break :sw;

            const open_token = self.token;
            self.token += 1;
            try self.insertNthLastNode(len, .null);
            const seq = try self.parseSequence(.@"}");
            self.getNodeEntryUnscoped(node).* = .{ .constructor = .{
                .elem_count = seq.count,
                .token = open_token,
            } };
            len += 1 + seq.node_consumption;
            continue :sw self.tokenizer.kind(self.token);
        },
        //.@"." => {//member access, postfix fn call
        .@"(" => {
            const open_token = self.token;
            self.token += 1;
            try self.insertNthLastNode(len, .null);
            const seq = try self.parseSequence(.@")");
            self.getNodeEntryUnscoped(node).* = .{ .call = .{
                .arg_count = seq.count,
                .token = open_token,
            } };
            len += 1 + seq.node_consumption;
            continue :sw self.tokenizer.kind(self.token);
        },

        else => break :sw,
    }
    return len;
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
                .payload = try self.addNumberValue(
                    parseIntLiteral(self.tokenizer.slice(self.token)),
                ),
            } });

            self.token += 1;
            break :blk 1;
        },
        // .float_literal
        .@"fn" => blk: {
            self.token += 1;
            break :blk try self.parseFunctionTypeOrDeclaration();
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
            if (util.matchToEnum(
                Builtin,
                self.tokenizer.slice(self.token)[1..],
            )) |valid_builtin|
                _ = try self.appendNode(.{ .builtin = .{
                    .builtin = valid_builtin,
                    .token = self.token,
                } })
            else
                return self.errorOut(.{ .token = self.token, .payload = .invalid_builtin });
            self.token += 1;
            break :blk 1;
        },
        .true, .false => |tf| blk: {
            _ = try self.appendNode(.{ .value = .{
                .type = try self.addType(.bool),
                .payload = try self.addNumberValue(util.fit(u128, tf == .true)),
            } });
            self.token += 1;
            break :blk 1;
        },
        inline .compint, .compfloat => |c| blk: {
            _ = try self.appendNode(.{ .value = .{
                .type = .type,
                .payload = @intFromEnum(try self.addType(@unionInit(TypeEntry, @tagName(c), {}))),
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
            break :blk 1 + try self.parseExpression1(false);
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

                self.getNodeEntryUnscoped(node).constructor.elem_count = seq.count;
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
fn parseFunctionTypeOrDeclaration(self: *Parser) Error!u32 {
    const header_token = self.token - 1;
    //TODO: in parse sequence put skipEndl() in else block of while loop
    //to allow endl before delimiter
    const header = try self.appendNode(.{ .function_type_declaration = .{
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
            const State = enum { idk, fn_decl, type_decl };
            var state: State = if (qualifier != .none) .fn_decl else .idk;

            const name = self.token;

            const meat = self.tokenizer.kind(self.token);
            if (meat != .identifier)
                if (state == .fn_decl or must_be_fn_decl) {
                    return self.errorOut(.{
                        .token = self.token,
                        .payload = .unexpected_token,
                    });
                } else {
                    state = .type_decl;
                    arg_node_consumption += try self.parseExpression2(.{ .kind_or_comma = .@")" });
                    if (self.tokenizer.kind(self.token) == .@",")
                        self.token += 1;
                }
            else
                self.token += 1;

            //state cannot be .type_decl as it would errorOut on 'parseExpression2'
            if (self.tokenizer.kind(self.token) == .@":") {
                state = .fn_decl;
                self.token += 1;
                _ = self.skipEndl();

                _ = try self.appendNode(.{ .function_parameter = .{ .name = name, .qualifier = qualifier } });

                if (self.tokenizer.kind(self.token) == .@"anytype") {
                    _ = try self.appendNode(.{ .@"anytype" = self.token });
                    self.token += 1;
                    arg_node_consumption += 2;
                } else arg_node_consumption += 1 + try self.parseExpression2(.{ .kind_or_comma = .@")" });
            } else if (state == .fn_decl or must_be_fn_decl) {
                //parameter with inferred type
                _ = try self.appendNode(.{ .function_parameter = .{ .name = name, .qualifier = qualifier } });
                _ = try self.appendNode(.null);
                arg_node_consumption += 2; //header + <null>

            } else if (meat == .identifier) {
                _ = try self.appendNode(.{ .identifier = name });
                arg_node_consumption += 1;
            }

            if (state == .fn_decl) {
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

    const rtype_len = try self.parseExpression1(true);

    if (self.tokenizer.kind(self.tokenPastEndl()) == .@"{") {
        if (!must_be_fn_decl) try self.convertArgTypesToFunctionParameters(header + 1, arg_count, header_token);

        _ = self.skipEndl();
        self.token += 1;
        const function = try self.addFunction(.{ .node = header, .arg_count = arg_count });
        const scope = try self.parseScope(.{
            .parent = self.current_scope,
            .container = .{ .function = function },
        });
        self.getFunctionEntry(function).scope = scope;
        self.getNodeEntryUnscoped(header).* = .{ .function_declaration = .{
            .function = function,
            .token = header_token,
        } };
    } else {
        if (must_be_fn_decl) return self.errorOut(.{
            .token = header_token,
            .payload = .missing_function_body,
        });
        self.getNodeEntryUnscoped(header).function_type_declaration.arg_count = arg_count;
    }

    return 1 + arg_node_consumption + rtype_len;
}
fn convertArgTypesToFunctionParameters(self: *Parser, node: Node, count: u32, fn_decl_token: Token) Error!void {
    const scope = self.getScopeEntry(self.current_scope);
    try scope.body.ensureUnusedCapacity(self.allocator, count);

    for (0..count) |i| {
        const current = node + 2 * @as(u32, @truncate(i));
        const name: Token = switch (self.getNodeEntryUnscoped(current).*) {
            .identifier => |id| id,
            else => return self.errorOut(.{
                .token = fn_decl_token,
                .payload = .invalid_function_declaration,
            }),
        };

        self.insertNodeAssumeCapacity(current, .{ .function_parameter = .{
            .name = name,
            .qualifier = .none,
        } });

        self.getNodeEntryUnscoped(current + 1).* = .null;
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
    const entry = self.getScopeEntry(scope);
    return if (entry.container == .@"struct")
        self.getStructEntry(entry.container.@"struct").is_file
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

fn getNumberValue(self: *Parser, id: u32) u128 {
    return self.number_values.items[id];
}

pub fn addNumberValue(self: *Parser, scalar: anytype) Error!u32 {
    for (self.number_values.items, 0..) |s, i| if (s == util.fit(u128, scalar)) return @truncate(i);
    const l: u32 = @truncate(self.number_values.items.len);
    try self.number_values.append(self.allocator, util.fit(u128, scalar));
    return l;
}

pub fn getTypeEntry(self: *Parser, @"type": Type) TypeEntry {
    return self.types.items[@intFromEnum(@"type")];
}

fn addFunctionType(self: *Parser, arg_types: []const Type, rtype: Type) Error!TypeEntry.FunctionType {
    const fte = self.function_type_elems.items;
    const arg_count: u32 = @truncate(arg_types.len);

    //check the element list for matches
    for (0..fte.len) |i| {
        if (i + arg_count >= fte.len) break;
        if (std.mem.eql(Type, arg_types, fte[i .. i + arg_count]) and
            fte[i + arg_count] == rtype)
            return .{ .id = @truncate(i), .arg_count = arg_count };
    }
    try self.function_type_elems.ensureUnusedCapacity(self.allocator, arg_count + 1);
    const function_type_id: u32 = @truncate(self.function_type_elems.items.len);
    self.function_type_elems.appendSliceAssumeCapacity(arg_types);
    self.function_type_elems.appendAssumeCapacity(rtype);

    return .{ .id = function_type_id, .arg_count = arg_count };
}

fn addType(self: *Parser, entry: TypeEntry) Error!Type {
    for (0..self.types.items.len) |i| {
        if (std.meta.eql(entry, self.getTypeEntry(@enumFromInt(i))))
            return @enumFromInt(i);
    }
    const id: Type = @enumFromInt(self.types.items.len);
    try self.types.append(self.allocator, entry);
    return id;
}

pub inline fn getNodeToken(self: *Parser, node: Node) Token {
    return self.getNodeEntryUnscoped(node).token();
}
pub fn getNodeEntryUnscoped(self: *Parser, node: Node) *NodeEntry {
    return self.getNodeEntry(self.current_scope, node);
}
pub fn getNodeEntry(self: *Parser, scope: Scope, node: Node) *NodeEntry {
    return &self.getScopeEntry(scope).body.items[node];
}
fn appendNode(self: *Parser, entry: NodeEntry) Error!Node {
    const scope = self.getScopeEntry(self.current_scope);
    const l: Node = @truncate(scope.body.items.len);
    try scope.body.append(self.allocator, entry);
    return l;
}

fn addVectorValueSplat(self: *Parser, vector: TypeEntry.Vector, elem_id: u32) Error!u32 {
    const bits = self.getNumberValue(elem_id);
    return switch (vector.scalar.width) {
        inline else => |width| blk: {
            const list = self.getVectorList(width.value());
            const U = @Int(.unsigned, @truncate(width.value()));
            var vec: @Vector(4, U) = @splat(0);
            const ptr: [*]U = @ptrCast(&vec);
            for (0..vector.len.value()) |i| ptr[i] = util.extract(U, bits);
            try list.append(self.allocator, vec);
            break :blk @truncate(list.items.len - 1);
        },
    };
}

fn addVectorValue(self: *Parser, V: type, value: V) Error!u32 {
    const T = @typeInfo(V).vector.child;
    const len = @typeInfo(V).vector.len;
    const width = @bitSizeOf(T);
    const list = self.getVectorList(width);
    const mask = blk: {
        var m: @Vector(4, i32) = @splat(-1);
        for (0..len) |i| m[i] = i;
        break :blk m;
    };
    const padded = @shuffle(T, value, @as(@Vector(2, T), @splat(0)), mask);
    try list.append(self.allocator, @bitCast(padded));
    return @truncate(list.items.len - 1);
}

fn getVectorList(self: *Parser, comptime width: comptime_int) *List(@Vector(4, @Int(.unsigned, width))) {
    return switch (width) {
        8 => &self.x8_vectors,
        16 => &self.x16_vectors,
        32 => &self.x32_vectors,
        64 => &self.x64_vectors,
        else => comptime unreachable,
    };
}
fn isScopeParentOfCurrent(self: *Parser, scope: Scope) bool {
    var s = self.current_scope;

    return while (true) {
        if (self.isScopeFile(scope)) break false;
        const entry = self.getScopeEntry(s);
        if (entry.parent == scope) break true;
        s = entry.parent;
    } else false;
}

fn addScope(self: *Parser, entry: ScopeEntry) Error!Scope {
    const l = self.scopes.items.len;
    try self.scopes.append(self.allocator, entry);
    return @enumFromInt(l);
}
inline fn getBodySlice(self: *Parser) []NodeEntry {
    return self.getScopeEntry(self.current_scope).body.items;
}
pub inline fn getScopeEntry(self: *Parser, handle: Scope) *ScopeEntry {
    return &self.scopes.items[@intFromEnum(handle)];
}
pub const Scope = enum(u32) { root_source_file, _ };
pub const ScopeEntry = struct {
    body: List(NodeEntry) = .empty,
    parent: Scope = .root_source_file,
    container: Container,
    const Container = union(enum) {
        @"struct": Struct,
        function: Function,
        function_permutation: void,
        pub fn isDecl(self: Container) bool {
            return self == .@"struct";
        }
    };
    pub fn getDeclNode(entry: ScopeEntry, self: *Parser) Node {
        return switch (entry.container) {
            .@"struct" => |s| self.getStructEntry(s).node,
            .function => |f| self.getFunctionEntry(f).node,
            else => unreachable,
        };
    }
};

fn addStruct(self: *Parser, entry: StructEntry) Error!Struct {
    const l = self.structs.items.len;
    try self.structs.append(self.allocator, entry);
    return @enumFromInt(l);
}
fn getStructEntry(self: *Parser, handle: Struct) *StructEntry {
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
pub fn getFunctionEntry(self: *Parser, handle: Function) *FunctionEntry {
    return &self.functions.items[@intFromEnum(handle)];
}

//how to get a function permutation
// fn(T: type, a: T, b: anytype) @TypeOf(b)->
// T = f32, b = u32{2}
// fn(a: f32, b: u32) u32{
//    return .{a} + b
// }

//for (arguments){
//   if(comptime) {
//     if(!passed_arg.isComptime()) errorOut()
//     add the value to the list
//   }if(anytype)
//     add @TypeOf(passed.arg) to args
//   else(add existing type) to args
//   THEY CAN DEPEND ON EACH OTHER
//}

//fold header in created scope then save the data into
// function permutation entry or smth,
// then use that created scope to copy over
// function body, and fold the scope

//FunctionPermutation.getVarRef{
// 1. for(arguments)
// 2. for(comptime_values)
// 3. go through statements
//}
pub const FunctionPermutation = enum(u32) { _ };
pub const FunctionPermutationEntry = struct {
    function: Function,
    scope: Scope,

    comptime_args: []Value,
    arguments: []Argument,
    pub const Argument = struct {
        name: Token,
        type: Type,
    };
};

pub const Function = enum(u32) { _ };
pub const FunctionEntry = struct {
    scope: Scope = undefined,
    node: Node = undefined,

    arg_count: u32 = 0,

    flags: Flags = .{},

    // name: Token = undefined,
    pub const Flags = packed struct {
        vertex_dep: bool = false,
        fragment_dep: bool = false,
        compute_dep: bool = false,
        is_consistent: bool = false,
        is_pure: bool = false,
    };
};

// const CompositeHeader = packed struct(u8);
const BuiltinNode = struct {
    builtin: Builtin,
    token: Token,
};
const Builtin = enum {
    position, // f32x4
    vertex_id, // u32
    TypeOf, //fn(anytype)type
};

pub const Node = u32;
pub const NodeEntry = union(enum) {
    pad,
    null,

    identifier: Token,
    builtin: BuiltinNode,

    bin_op: BinOpNode, //[bin_op][left][right]
    u_op: UOpNode, //[u_op][operand]
    value: Value,

    indexing: Token, //[][target][index]
    constructor: ConstructorNode, //[count][type][elems..]

    call: Call, //[][callee][args..]
    array_type_decl, //??

    function_type_declaration: FunctionTypeDeclaration, //[fn_type_decl][arg_count][arg_types...][rtype]
    function_declaration: FunctionDeclarationNode, //[fn_decl][args...][rtype][body...]
    @"anytype": Token,
    function_parameter: FunctionParameter,
    //[fn_param][type / anytype / null(if the same as the next one)]
    var_ref: VariableReference,
    assignment: Token, //[assignment][target][value]

    @"return": Token, //[return][return value]
    branch,
    loop: [11]u8,

    //statement
    var_decl: VariableDeclaration, //[var_decl][qualifier_info][type][initializer]
    folded_var_decl: VariableDeclaration,
    pub fn token(self: NodeEntry) Token {
        return switch (self) {
            .null, .pad => 0, //unknown
            .array_type_decl, .branch, .loop, .var_ref => 0, //

            .function_parameter => |fp| fp.name,
            .var_decl, .folded_var_decl => |vd| vd.qualifier_token,
            .identifier,
            .assignment,
            .indexing,
            .@"return",
            .@"anytype",
            => |tok| tok,
            inline else => |o| o.token,
        };
    }
};
const Call = struct {
    arg_count: u32,
    token: Token,
};
const FunctionTypeDeclaration = struct {
    arg_count: u32,
    token: Token,
};
const FunctionDeclarationNode = struct {
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
pub const VariableReference = struct {
    scope: Scope,
    node: Node,
    value: Node = 0,
};
pub const VariableDeclaration = struct {
    qualifier: Qualifier,
    qualifier_token: Token,

    name: Token,
};
pub const Qualifier = enum {
    @"const",
    @"var",
    env,

    push,
    workgroup,

    vertex,
    fragment,
    compute, //[workgroup size]
    pub fn isEntryPoint(self: Qualifier) bool {
        return util.enumInRange(Qualifier, self, .vertex, .compute);
    }
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
pub const Value = struct {
    type: Type,
    payload: u32,
    token: Token = undefined,
};

fn typeLength(self: *Parser, @"type": Type) u32 {
    return switch (self.getTypeEntry(@"type")) {
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
    const entry = self.getTypeEntry(@"type");
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

pub const Type = enum(u32) { type = 0, @"anytype" = ~@as(u32, 0), _ };
//make 16 bytes?
pub const TypeEntry = union(enum) {
    void,
    type,
    bool,
    compint,
    compfloat,

    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    array: Array, //length
    pointer: Type,

    @"enum": u32,
    @"struct": Struct,
    function: FunctionType,

    pub const Tag = std.meta.Tag(@This());
    pub const FunctionType = struct {
        id: u32,
        arg_count: u32,
    };
    const Array = struct { len: u32, child: Type };
    pub const Matrix = packed struct {
        scalar: Scalar,
        m: Vector.Len,
        n: Vector.Len,
        pub fn format(self: Matrix, writer: *std.Io.Writer) !void {
            try writer.print("{f}x{s}x{s}", .{
                self.scalar,
                @tagName(self.m)[1..],
                @tagName(self.n)[1..],
            });
        }
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
        pub fn format(self: Vector, writer: *std.Io.Writer) !void {
            try writer.print("{f}x{s}", .{
                self.scalar,
                @tagName(self.len)[1..],
            });
        }
    };
    pub const Scalar = packed struct {
        width: Width,
        layout: Layout,
        pub const Layout = enum(u2) { float, uint, int };
        pub const Width = enum(u2) {
            _8,
            _16,
            _32,
            _64,
            pub fn value(width: Width) u32 {
                return @as(u32, 1 << 3) << @intFromEnum(width);
            }
        };

        pub fn ToZig(comptime scalar: Scalar) type {
            return if (scalar.layout == .float)
                std.meta.Float(@max(16, scalar.width.value()))
            else
                @Int(if (scalar.layout == .int) .signed else .unsigned, scalar.width.value());
        }
        pub inline fn eql(a: Scalar, b: Scalar) bool {
            return a.width == b.width and a.layout == b.layout;
        }
        pub fn format(self: Scalar, writer: *std.Io.Writer) !void {
            try writer.print("{c}{s}", .{
                "fui"[@intFromEnum(self.layout)],
                @tagName(self.width)[1..],
            });
        }
    };
    pub fn isComptime(self: TypeEntry) bool {
        return switch (self) {
            .void, .type, .compint, .compfloat, .function => true,
            else => false,
        };
    }
    pub fn ToZig(comptime self: TypeEntry) type {
        return switch (self) {
            .scalar => |scalar| scalar.ToZig(),
            .compint => hgsl.CI,
            .compfloat => hgsl.CF,
            .bool => bool,
            else => @compileError("cannot cast to zig - " ++ @tagName(self)),
        };
    }
    pub fn format(self: TypeEntry, writer: *std.Io.Writer) !void {
        switch (self) {
            inline .scalar, .vector, .matrix => |numeric| try numeric.format(writer),
            else => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};

pub fn errorOutNotAType(self: *Parser, node: Node) Error {
    return self.errorOut(.{
        .token = self.getNodeToken(node),
        .payload = .{ .unexpected_type_tag = .{
            .expected = .type,
            .got = try self.typeOf(self.current_scope, node),
        } },
    });
}
pub fn errorOut(self: *Parser, error_info: ErrorInfo) Error {
    self.error_info = error_info;
    return Error.CompilationError;
}

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
                for (0..ftype.arg_count) |i| try writer.print("{f}{s}", .{
                    DebugType{
                        .self = entry.self,
                        .type = entry.self.function_type_elems.items[ftype.id + i],
                    },
                    if (i + 1 == ftype.arg_count) "" else ", ",
                });
                try writer.print(") {f}", .{DebugType{
                    .self = entry.self,
                    .type = entry.self.function_type_elems.items[ftype.id + ftype.arg_count],
                }});
            },
            .pointer => |pointed| try writer.print("*{f}", .{DebugType{ .self = entry.self, .type = pointed }}),
            else => try TypeEntry.format(te, writer),
        }
    }
};
const DebugValue = struct {
    self: *Parser,
    value: Value,
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
const DebugNodeU = struct {
    self: *Parser,
    node: Node,

    pub fn format(entry: DebugNodeU, writer: *std.Io.Writer) !void {
        var node = entry.node;
        try (DebugNode{ .self = entry.self, .node = &node }).format(writer);
    }
};
const DebugNode = struct {
    self: *Parser,
    node: *Node,
    pub fn format(entry: DebugNode, writer: *std.Io.Writer) !void {
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
            .call => |function_call| {
                entry.node.* += 1;
                try writer.print("{f}(", .{entry});
                for (0..function_call.arg_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == function_call.arg_count) "" else ", " });
                try writer.print(")", .{});
            },
            .function_type_declaration => |fn_type_decl| {
                entry.node.* += 1;
                try writer.print("fn(", .{});

                for (0..fn_type_decl.arg_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == fn_type_decl.arg_count) "" else ", " });
                try writer.print(") {f}", .{entry});
            },
            .function_declaration => |fn_decl| {
                entry.node.* += 1;
                const arg_count = entry.self.getFunctionEntry(fn_decl.function).arg_count;
                try writer.print("fn (", .{});

                for (0..arg_count) |i|
                    try writer.print("{f}{s}", .{ entry, if (i + 1 == arg_count) "" else ", " });
                try writer.print(") {f}{{\n", .{entry});

                const last_scope = entry.self.current_scope;
                entry.self.current_scope = entry.self.getFunctionEntry(fn_decl.function).scope;
                defer entry.self.current_scope = last_scope;

                entry.self.dumpCurrentScope(false, true);
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
            .var_decl, .folded_var_decl => |var_decl| {
                if (body[entry.node.*] == .folded_var_decl) try writer.writeByte('*');
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
            .var_ref => |vr| {
                entry.node.* += 1;
                const node_entry = entry.self.getNodeEntry(vr.scope, vr.node).*;
                const token = switch (node_entry) {
                    .folded_var_decl => |var_decl| var_decl.name,
                    .function_parameter => |fn_param| fn_param.name,
                    else => unreachable,
                };

                try writer.print("'{s}", .{entry.self.tokenizer.slice(token)});
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

pub const DebugToken = Tokenizer.DebugToken;
const ErrorInfo = error_message.ParserErrorInfo;
const Error = hgsl.Error;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
const UnaryOperator = Tokenizer.UnaryOperator;
const Token = Tokenizer.Token;
const TokenEntry = Tokenizer.TokenEntry;
const activeTag = std.meta.activeTag;

// .identifier => |identifier| blk: {
//     for (self.access_dependency_stack.items) |i|
//         if (util.strEql(identifier, i))
//             return self.errorOut(.{ .dependency_loop = identifier });
//     try self.access_dependency_stack.append(self.arena.allocator(), identifier);
//     defer _ = self.access_dependency_stack.pop();

//     const vr = try self.getVariableReference(identifier);
//     break :blk if (access == .full and vr.var_ref.qualifier == .@"const" and vr.value != .null)
//         vr.value
//     else
//         .{ .var_ref = vr.var_ref };
// },
