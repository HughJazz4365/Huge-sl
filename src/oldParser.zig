const std = @import("std");
const zigbuiltin = @import("builtin");
const debug = @import("debug.zig");
// const bi = @import("builtin.zig");
// const ct = @import("comptime.zig");
const tp = @import("type.zig");
const util = @import("util.zig");
const Parser = @This();
const Tokenizer = @import("Tokenizer.zig");
const error_message = @import("errorMessage.zig");
const hgsl = @import("root.zig");
// const Settings = hgsl.Settings;

const flatten_blocks = false;

tokenizer: Tokenizer = undefined,

allocator: Allocator,
arena: std.heap.ArenaAllocator,

file_struct: StructID = @enumFromInt(std.math.maxInt(u32)),
current_scope: *Scope = undefined,

structs: List(Struct) = .empty,
entry_points: List(EntryPoint) = .empty,
blocks: List(Scope) = .empty,

access_dependency_stack: List([]const u8) = undefined,
pub const StructID = enum(u32) { _ };
pub const EntryPointID = enum(u32) { _ };
pub const BlockID = enum(u32) { _ };

// intermediate_value_index: u32 = 0,
// settings: Settings,

pub fn new(tokenizer: Tokenizer, allocator: Allocator) Error!Parser {
    var self: Parser = .{
        .tokenizer = tokenizer,
        .allocator = allocator,
        .arena = .init(allocator),
    };
    self.access_dependency_stack = try .initCapacity(self.arena.allocator(), 32);
    return self;
}
pub fn parse(self: *Parser) Error!void {
    debug.p = self;
    const file_struct_id = try self.parseFile(self.tokenizer);

    try self.foldScope(&self.getStruct(file_struct_id).scope);
    for (self.getStruct(file_struct_id).scope.body.items) |*s| {
        std.debug.print("S: {f}\n", .{s});
    }
}
fn parseFile(self: *Parser, tokenizer: Tokenizer) Error!StructID {
    const prev_file_struct = self.file_struct;
    const prev_tokenizer = self.tokenizer;
    defer {
        self.file_struct = prev_file_struct;
        const error_info = self.tokenizer.error_info;
        self.tokenizer = prev_tokenizer;
        self.tokenizer.error_info = error_info;
    }

    self.tokenizer = tokenizer;
    const fsid = try self.addStruct(.{
        .name = tokenizer.path,
        .is_file = true,
        .scope = .{ .type = .@"struct" },
    });
    self.file_struct = fsid;
    try self.parseScope(&self.getStruct(fsid).scope);

    return fsid;
}
fn addStatement(self: *Parser, statement: Statement) Error!void {
    if (statement != .var_decl and
        self.current_scope.type.isDecl()) return self.errorOut(.unknown);
    try self.current_scope.body.append(self.arena.allocator(), statement);
}
fn parseStatement(self: *Parser) Error!void {
    const t = self.tokenizer;
    const token = try self.tokenizer.next();

    // std.debug.print("T: {f}, {d}\n", .{ token, @intFromPtr(self.current_scope) });
    switch (token) {
        .@"const", .mut, .in, .out, .shared, .push => //
        try self.parseVarDecl(try self.parseQualifier(token)),
        .@"{" => if (try self.tokenizer.peekPastEndl() == .@"}") {
            try self.tokenizer.skipEndl();
            self.tokenizer.skip();
            try self.addStatement(.empty);
        } else {
            const bid = try self.addBlock(.{ .type = .raw, .parent = self.current_scope });
            try self.parseScope(self.getBlock(bid));
            try self.addStatement(.{ .block = bid });
        },
        .@"return" => try self.addStatement(.{
            .@"return" = if (self.defaultShouldStop(try self.tokenizer.peek()))
                .null
            else
                try self.parseExpression(defaultShouldStop),
        }),
        .@"continue" => try self.addStatement(.@"continue"),
        else => {
            self.tokenizer = t;
            if (token == .identifier and try self.isStructFieldDecl()) {
                if (self.current_scope.type != .@"struct")
                    return self.errorOut(.unknown);

                try self.parseVarDecl(null);
                return;
            }

            //assignment or ignore
            const target = try self.parseExpressionAtom();
            const peek = try self.tokenizer.peek();

            if (self.defaultShouldStop(peek)) {
                try self.addStatement(.{ .ignore = target });
                return;
            }

            var mod: ?Tokenizer.BinaryOperator = null;
            if (peek == .bin_op) {
                mod = peek.bin_op;
                const tokenizer_state = self.tokenizer;
                self.tokenizer.skip();
                if (try self.tokenizer.next() != .@"=") {
                    self.tokenizer = tokenizer_state;
                    try self.addStatement(.{ .ignore = try self.parseExpressionRecursive(target, defaultShouldStop, 0) });
                    return;
                }
            }
            if (mod != null or peek == .@"=") {
                if (peek == .@"=") self.tokenizer.skip();
                const value = try self.parseExpression(defaultShouldStop);
                try self.addStatement(.{ .assignment = .{
                    .target = target,
                    .value = if (mod) |m| .{ .bin_op = .{
                        .left = try self.createVal(target),
                        .right = try self.createVal(value),
                        .op = m,
                    } } else value,
                } });
            } else return self.errorOut(.unknown);
        },
    }
}
//no declarations
fn isStructFieldDecl(self: *Parser) Error!bool {
    const tstate = self.tokenizer;
    defer self.tokenizer = tstate;

    //skip first identifier
    self.tokenizer.skip();

    var peek: Token = try self.tokenizer.next();
    return s: switch (peek) {
        .@":" => true,
        .@"," => {
            peek = try self.tokenizer.next();
            if (peek != .identifier) return false;

            peek = try self.tokenizer.next();
            continue :s peek;
        },
        else => false,
    };
}
fn parseVarDecl(self: *Parser, first_qualifier: ?Qualifier) Error!void {
    const qualifier = first_qualifier.?;
    const name_token = try self.tokenizer.next();
    if (name_token != .identifier) return self.errorOut(.unknown);

    const name = name_token.identifier;

    const @"type": ?Type = switch (try self.tokenizer.peek()) {
        .@":" => blk: {
            self.tokenizer.skip();
            break :blk try self.expressionAsTypeAlloc(
                try self.parseExpressionAtom(),
            );
        },
        .@"=" => null,
        else => |e| if (self.defaultShouldStop(e))
            return self.errorOut(.unknown)
        else
            null,
    };

    const peek = try self.tokenizer.next();
    const initializer = if (peek == .@"=")
        try self.parseExpression(defaultShouldStop)
    else if (self.defaultShouldStop(peek))
        .null
    else
        return self.errorOut(.{ .unexpected_token = self.tokenizer.last });

    if (initializer == .null and qualifier == .@"const")
        return self.errorOut(.unknown);

    try self.addStatement(.{ .var_decl = .{
        .qualifier = qualifier,
        .name = name,
        .type = if (@"type") |t| t else try self.typeOfAlloc(initializer),
        .initializer = if (@"type") |t| try self.implicitCast(t, initializer) else initializer,
    } });
}

fn parseQualifier(self: *Parser, token: Token) Error!Qualifier {
    return switch (token) {
        .in, .out => blk: {
            const peek = try self.tokenizer.peek();
            const interpolation: Interpolation = .smooth;
            if (peek == .@"(") {}
            break :blk if (token == .in) .{ .in = interpolation } else .{ .out = interpolation };
        },
        inline else => |_, tag| switch (tag) {
            .@"const", .mut, .shared, .push => @unionInit(Qualifier, @tagName(tag), {}),
            else => unreachable,
        },
    };
}
const ShouldStopFn = fn (*Parser, Token) bool;
inline fn parseExpression(self: *Parser, should_stop_fn: *const ShouldStopFn) Error!Expression {
    return self.parseExpressionRecursive(try self.parseExpressionAtom(), should_stop_fn, 0);
}
fn parseExpressionRecursive(self: *Parser, l: Expression, should_stop_fn: *const ShouldStopFn, bp: u8) Error!Expression {
    //doesnt consume token it stopped at
    var left = l;
    var fat = try self.tokenizer.peekFat();
    while (!should_stop_fn(self, fat.token)) {
        if (fat.token != .bin_op) return self.errorOut(.{ .unexpected_token = fat });

        const op = fat.token.bin_op;
        if (Tokenizer.bindingPower(op) <= bp) break;

        self.tokenizer.skip();
        try self.tokenizer.skipEndl(); //?

        const right = try self.parseExpressionRecursive(try self.parseExpressionAtom(), should_stop_fn, Tokenizer.bindingPower(op));
        const add_left = try self.createVal(left);
        left = .{ .bin_op = .{
            .left = add_left,
            .right = try self.createVal(right),
            .op = op,
        } };
        fat = try self.tokenizer.peekFat();
    }
    return left;
}

fn parseExpressionAtom(self: *Parser) Error!Expression {
    // var expr = try self.parseExpressionBase();

    // sw: switch (try self.tokenizer.peek()) {
    //     .@"{" => {},
    //     else => return expr,
    // }
    return self.parseExpressionBase();
}
fn parseExpressionBase(self: *Parser) Error!Expression {
    return switch (try self.tokenizer.next()) {
        .true => .{ .value = .create(.bool, true) },
        .false => .{ .value = .create(.bool, false) },

        .identifier => |identifier| .{ .identifier = identifier },
        .int_literal => |il| .{ .value = Value.create(.compint, il) },
        .float_literal => |fl| .{ .value = Value.create(.compfloat, fl) },
        .type_literal => |@"type"| exprFromType(@"type"),
        .entrypoint => blk: {
            const stage_info = try self.parseEntryPointType();
            try self.tokenizer.skipEndl();
            if (try self.tokenizer.peek() != .@"{")
                break :blk (Type{ .entry_point = stage_info }).toExpr();

            self.tokenizer.skip();
            const epid = try self.addEntryPoint(EntryPoint{
                .stage_info = stage_info,
                .scope = .{
                    .type = .entry_point,
                    .parent = self.current_scope,
                },
            });
            try self.parseScope(&self.getEntryPoint(epid).scope);
            break :blk .{ .value = .{
                .type = .{ .entry_point = stage_info },
                .payload = .{ .entry_point = epid },
            } };
        },
        // .@"("
        .@"if" => blk: { //parsebranch
            try self.tokenizer.skipEndl();
            if (try self.tokenizer.next() != .@"(")
                return self.errorOut(.{ .unexpected_token = self.tokenizer.last });
            const condition = try self.parseExpression(bracketShouldStop);
            self.tokenizer.skip();
            try self.tokenizer.skipEndl();

            const true_statement = try self.parseBranchCaseStatement(true);

            const false_statement: ?Statement = if (try self.tokenizer.peekPastEndl() == .@"else") clk: {
                try self.tokenizer.skipEndl();
                self.tokenizer.skip();
                try self.tokenizer.skipEndl();
                break :clk try self.parseBranchCaseStatement(false);
            } else null;

            break :blk .{ .branch = .{
                .condition = try self.createVal(condition),
                .true = try self.createVal(true_statement),
                .false = if (false_statement) |fs| try self.createVal(fs) else @constCast(&Statement{ .empty = {} }),
            } };
        },
        else => self.errorOut(.{ .unexpected_token = self.tokenizer.last }),
    };
}
fn parseBranchCaseStatement(self: *Parser, is_true: bool) Error!Statement {
    var body_buf: Statement = undefined;
    var helper_scope: Scope = .{
        .type = if (is_true) .true_case else .raw,
        .parent = self.current_scope,
        .body = .initBuffer(@ptrCast(&body_buf)),
    };
    const last = self.current_scope;

    self.current_scope = &helper_scope;

    try self.parseStatement();
    self.current_scope = last;

    if (helper_scope.body.items.len == 1) {
        const s = helper_scope.body.items[0];
        if (s == .block) self.getBlock(s.block).parent = helper_scope.parent;
        return s;
    }

    return .{ .block = try self.addBlock(helper_scope) };
}
fn parseConstructor(self: *Parser, @"type": Type) Error!Expression {
    return switch (@"type") {
        // .entry_point =>
        else => self.errorOut(.unknown),
    };
}
fn parseScope(self: *Parser, scope: *Scope) Error!void {
    //expects leading '{' to be skipped
    const last_scope = self.current_scope;
    self.current_scope = scope;
    defer self.current_scope = last_scope;

    while (true) {
        try self.tokenizer.skipEndl();
        const peek = try self.tokenizer.peek();
        if (!self.current_scope.isFile()) { //check end of scope
            if (peek == .eof) return self.errorOut(.unclosed_scope);
            if (peek == .@"}") {
                self.tokenizer.skip();
                break;
            }
        } else if (peek == .eof) {
            self.tokenizer.skip();
            break;
        }

        try self.parseStatement();
    }
}
fn nameExpression(self: *Parser, expr: Expression, name: []const u8) void {
    _ = .{ expr, self, name };
}

fn parseEntryPointType(self: *Parser) Error!StageInfo {
    if (try self.tokenizer.next() != .@"(")
        return self.errorOut(.{ .unexpected_token = self.tokenizer.last });
    try self.tokenizer.skipErr();
    try self.tokenizer.skipErr();
    try self.tokenizer.skipErr();
    return .fragment;
}
pub fn getBlock(self: *Parser, bid: BlockID) *Scope {
    return &self.blocks.items[@intFromEnum(bid)];
}
pub fn addBlock(self: *Parser, block: Scope) Error!BlockID {
    const len: u32 = @truncate(self.blocks.items.len);
    try self.blocks.append(self.arena.allocator(), block);
    return @enumFromInt(len);
}

pub fn getEntryPoint(self: *Parser, epid: EntryPointID) *EntryPoint {
    return &self.entry_points.items[@intFromEnum(epid)];
}
pub fn addEntryPoint(self: *Parser, ep: EntryPoint) Error!EntryPointID {
    const len: u32 = @truncate(self.entry_points.items.len);
    try self.entry_points.append(self.arena.allocator(), ep);
    return @enumFromInt(len);
}
pub const EntryPoint = struct {
    name: []const u8 = "",
    scope: Scope = .{},

    stage_info: StageInfo,
    global_io_count: usize = 0,
};
pub const StageInfo = union(enum) {
    fragment,
    vertext,
    compute: @Vector(3, u32),
    //raytracing
    //mesh
};

pub const Expression = union(enum) {
    identifier: []const u8,
    var_ref: VariableReference,
    bin_op: BinOp,
    value: Value,
    branch: Branch,
    cast: Cast,

    null,
    pub const format = debug.formatExpression;
};
pub const Branch = struct {
    condition: *Expression,
    true: *Statement,
    false: *Statement,
};
pub const BinOp = struct {
    left: *Expression,
    right: *Expression,
    op: Tokenizer.BinaryOperator,
};

pub const Cast = struct {
    type: Type,
    expr: *Expression,
};
pub const Value = struct {
    type: Type,
    payload: Payload,
    pub inline fn create(comptime type_tag: Type.Tag, payload: PayloadType(type_tag)) Value {
        return Value.createFromType(@unionInit(Type, @tagName(type_tag), {}), payload);
    }
    pub inline fn createFromType(comptime @"type": Type, payload: PayloadTypeFromType(@"type")) Value {
        return .{
            .type = @"type",
            .payload = @unionInit(
                Payload,
                payloadTagName(std.meta.activeTag(@"type")),
                switch (@"type") {
                    .scalar, .compint, .compfloat, .bool => util.fit(u128, payload),
                    .type, .entry_point => payload,
                    else => @ptrCast(@alignCast(payload)),
                },
            ),
        };
    }
    pub const Payload = union {
        numeric: u128,
        type: Type,
        ptr: *const anyopaque,
        entry_point: EntryPointID,
        pub inline fn get(self: Payload, comptime type_tag: Type.Tag) PayloadType(type_tag) {
            return getOpt(self, type_tag, PayloadType(type_tag));
        }
        pub inline fn getFromType(self: Payload, comptime @"type": Type) PayloadTypeFromType(@"type") {
            return getOpt(self, std.meta.activeTag(@"type"), PayloadTypeFromType(@"type"));
        }
        inline fn getOpt(self: Payload, comptime type_tag: Type.Tag, T: type) T {
            return switch (type_tag) {
                .type => self.type,
                .scalar, .compint, .compfloat, .bool => util.extract(T, self.numeric),
                .entry_point => self.entry_point,
                else => unreachable,
            };
        }
    };
    pub fn payloadTagName(comptime type_tag: Type.Tag) []const u8 {
        return switch (type_tag) {
            .bool, .compint, .compfloat, .scalar => "numeric",
            .type => "type",
            .entry_point => "entry_point",
            else => "ptr",
        };
    }
    pub fn PayloadType(comptime type_tag: Type.Tag) type {
        return switch (type_tag) {
            .type => Type,
            .compint => hgsl.CI,
            .compfloat => hgsl.CF,
            .bool => bool,
            .entry_point => EntryPointID,
            else => @compileError("unknown payload type - " ++ @tagName(type_tag)),
        };
    }
    pub fn PayloadTypeFromType(comptime @"type": Type) type {
        return switch (@"type") {
            .scalar => |scalar| scalar.ToZig(),
            else => PayloadType(std.meta.activeTag(@"type")),
        };
    }
    pub const format = debug.formatValue;
};
pub const Statement = union(enum) {
    var_decl: VariableDeclaration,
    ignore: Expression,
    assignment: Assignment,
    block: BlockID,

    @"return": Expression,
    @"continue",
    discard,

    empty,

    pub const format = debug.formatStatement;
};
pub const Assignment = struct {
    target: Expression,
    value: Expression,
};
pub const ValueVariableReference = struct {
    var_ref: VariableReference,
    value: Expression,
};
pub const VariableReference = struct {
    qualifier: Qualifier,
    name: []const u8,
    type: Type,
    is_significant: *bool,
};

pub const VariableDeclaration = struct {
    qualifier: Qualifier,
    name: []const u8,
    type: Type,
    initializer: Expression = .null,

    is_significant: bool = false,

    pub fn getVariableReference(var_decl: *VariableDeclaration) ValueVariableReference {
        return .{
            .var_ref = .{
                .qualifier = var_decl.qualifier,
                .name = var_decl.name,
                .type = var_decl.type,

                .is_significant = &var_decl.is_significant,
            },
            .value = var_decl.initializer,
        };
    }
};
pub const Qualifier = union(enum) {
    @"const",
    mut,
    in: Interpolation,
    out: Interpolation,
    push,
    shared,
};
pub const Interpolation = enum {
    smooth,
    flat,
    noperspective,
};

pub const Scope = struct {
    type: ScopeType = .raw,
    parent: *Scope = undefined,

    body: List(Statement) = .empty,
    reference_index: usize = std.math.maxInt(usize),

    inserted_deferred: usize = 0,

    pub fn returnType(self: *Scope) ?Type {
        return switch (self.type) {
            .function => @panic("TODO: return type of function scope"),
            .entry_point => .void,
            else => null,
        };
    }
    pub fn getVariableReference(
        scope: *Scope,
        self: *Parser,
        name: []const u8,
    ) ?ValueVariableReference {
        for (scope.body.items, 0..) |*s, i| {
            if (!scope.type.isDecl() and i >= scope.reference_index)
                break;

            if (s.* == .var_decl and util.strEql(s.var_decl.name, name))
                return s.var_decl.getVariableReference();
        }
        return if (!scope.isFile()) scope.parent.getVariableReference(self, name) else null;
    }
    pub fn isFile(scope: *Scope) bool {
        return if (scope.type == .@"struct")
            @as(*Struct, @fieldParentPtr("scope", scope)).is_file
        else
            false;
    }
    pub const ScopeType = enum {
        function,
        entry_point,
        loop,
        @"struct",
        @"enum",
        raw,
        true_case,
        pub fn isDecl(self: ScopeType) bool {
            return self == .@"struct" or self == .@"enum";
        }
    };
};
pub fn getVariableReference(self: *Parser, name: []const u8) Error!ValueVariableReference {
    return if (self.current_scope.getVariableReference(self, name)) |vr|
        vr
    else
        self.errorOut(.{ .undeclared_identifier = name }); //undecl ref
}

const Switch = struct {
    switchon: Expression,
    case_values: []const Expression,
    case_statements: []const Statement,
    else_statement: ?Statement,
    //const some_var = switch(int){
    //  case1 => 18, case2 => 36,
    //  else => f32{int},
    //}
};

const Loop = struct {
    helper_variable: struct { //optional
        name: []const u8 = "",
        type: Type,
        initializer: Expression,
    },
    condition: Expression,
    ending_statement: Statement,

    body: Statement,
    //loop(i: u32 = 0, i < count, i += 1) {body..}
};

pub fn getStruct(self: *Parser, sid: StructID) *Struct {
    return &self.structs.items[@intFromEnum(sid)];
}
pub fn addStruct(self: *Parser, s: Struct) Error!StructID {
    const len: u32 = @truncate(self.structs.items.len);
    try self.structs.append(self.arena.allocator(), s);
    return @enumFromInt(len);
}
pub const Struct = struct {
    name: []const u8 = "",
    scope: Scope = .{},
    fields: List(Field) = .empty,

    is_file: bool = false,

    pub const Field = struct {
        name: []const u8,
        type: Type,
        default_value: Expression = .null,
    };
};

//bounds not in the moment but over time

//bounds for expression: []const Bound
//allowed for types:
//scalar, vector, matrix, array, struct, ?enum?
const Bound = struct {
    lower: ?Value = null,
    upper: ?Value = null,
    lower_inclusion: Inclusion = .inclusive,
    upper_inclusion: Inclusion = .inclusive,

    const Inclusion = enum { inclusive, exclusive };
};
fn bracketShouldStop(_: *Parser, token: Token) bool {
    return token == .@")";
}
fn defaultShouldStop(self: *Parser, token: Token) bool {
    return token == .endl or token == .eof or
        (token == .@"}" and !self.current_scope.isFile()) or
        (token == .@"else" and self.current_scope.type == .true_case);
}

pub inline fn errorOut(self: *Parser, error_info: ErrorInfo) Error {
    return self.tokenizer.errorOut(error_info);
}
//=====|folding|======

pub fn foldScope(self: *Parser, scope: *Scope) Error!void {
    const last_scope = self.current_scope;
    self.current_scope = scope;
    defer {
        self.current_scope = last_scope;
        scope.reference_index = std.math.maxInt(usize);
    }
    var found_end = false;

    for (scope.body.items, 0..) |*s, i| {
        if (found_end) return self.errorOut(.unreachable_statement);

        scope.reference_index = i;
        try self.foldStatement(s);
        found_end = self.doesStatementBreakControlFlow(s.*);

        if (s.* == .ignore and self.typeOf(&s.ignore) != .void) {
            std.debug.print("ignored type: {f}\n", .{self.typeOf(&s.ignore)});
            return self.errorOut(.non_void_ignore);
        }
    }
    if (scope.type.isDecl()) return;

    var removed_count: usize = 0;
    for (0..scope.body.items.len) |index| {
        if (index > scope.body.items.len - removed_count) break;
        const j = scope.body.items.len - index - 1;
        if (!self.isStatementSignificant(scope.body.items[j])) {
            _ = scope.body.orderedRemove(j);
            removed_count += 1;
        } else self.markStatementSignificant(scope.body.items[j]);
    }
}
fn markStatementSignificant(self: *Parser, statement: Statement) void {
    switch (statement) {
        .var_decl => |var_decl| self.markExpressionSignificant(var_decl.initializer),
        .ignore => |ignore| self.markExpressionSignificant(ignore),
        .assignment => |assignment| {
            self.markExpressionSignificant(assignment.target);
            self.markExpressionSignificant(assignment.value);
        },
        .block => |bid| for (self.getBlock(bid).body.items) |s|
            self.markStatementSignificant(s),
        .@"return" => |@"return"| self.markExpressionSignificant(@"return"),

        else => {},
    }
}
fn markExpressionSignificant(self: *Parser, expr: Expression) void {
    switch (expr) {
        .var_ref => |var_ref| {
            var_ref.is_significant.* = true;
        },
        .bin_op => |bin_op| {
            self.markExpressionSignificant(bin_op.left.*);
            self.markExpressionSignificant(bin_op.right.*);
        },
        .branch => |branch| {
            self.markExpressionSignificant(branch.condition.*);
            self.markStatementSignificant(branch.true.*);
            self.markStatementSignificant(branch.false.*);
        },
        else => {},
    }
}

//writes to/ declares output variable
//writes to buffer/image
//control flow

//makes sence for block scopes
//in decl scopes dont generate anything only global variables that are used in
//generated entry point
fn isStatementSignificant(self: *Parser, statement: Statement) bool {
    return switch (statement) {
        .@"continue", .@"return", .discard => true,
        .var_decl => |var_decl| var_decl.qualifier == .in or
            var_decl.qualifier == .out or
            var_decl.type == .entry_point or
            var_decl.type == .type or
            var_decl.is_significant or
            self.isExpressionSignificant(var_decl.initializer),
        .block => |bid| self.getBlock(bid).body.items.len > 0,
        // .assignment =>

        // var_decl: VariableDeclaration,
        // ignore: Expression,
        // assignment: Assignment,
        .empty,
        => false,
        else => true,
    };
}
fn isExpressionSignificant(self: *Parser, expr: Expression) bool {
    _ = self;
    return switch (expr) {
        // .var_ref => |var_ref| var_ref.is_significant.*,

        else => false,
    };
}

pub fn doesStatementBreakControlFlow(self: *Parser, statement: Statement) bool {
    return switch (statement) {
        .@"continue", .@"return", .discard => true,
        .ignore => |ignore| if (ignore == .branch)
            self.doesStatementBreakControlFlow(ignore.branch.true.*) and
                self.doesStatementBreakControlFlow(ignore.branch.false.*)
        else
            false,
        .block => |bid| //
        self.doesStatementBreakControlFlow(util.last(Statement, self.getBlock(bid).body.items)),
        else => false,
    };
}

pub fn foldStatement(self: *Parser, statement: *Statement) Error!void {
    const access: FoldAccess = if (self.current_scope.type == .loop) .none else .full;
    switch (statement.*) {
        .var_decl => |*vd| {
            vd.type = try self.foldType(vd.type);
            vd.initializer = try self.implicitCast(vd.type, try self.foldExpression(vd.initializer, access));
        },
        .assignment => |*ass| {
            ass.target = try self.foldExpression(ass.target, .none);
            const type_of_target = self.typeOf(&ass.target);

            ass.value = try self.implicitCast(
                type_of_target,
                try self.foldExpression(ass.value, access),
            );
        },
        .block => |bid| {
            const block = self.getBlock(bid);
            const last = self.current_scope;
            defer self.current_scope = last;

            self.current_scope = block;

            try self.foldScope(block);
        },
        .@"return" => |*@"return"| {
            var s = self.current_scope;
            const return_type: Type = if (while (!s.isFile()) {
                defer s = s.parent;
                if (s.returnType()) |rt| break rt;
            } else null) |rtype| rtype else return self.errorOut(.unexpected_return);
            @"return".* = try self.implicitCast(return_type, try self.foldExpression(@"return".*, access));
        },
        .ignore => |*ignore| {
            ignore.* = try self.foldExpression(ignore.*, access);

            if (ignore.* == .branch and ignore.branch.condition.* == .value) {
                const b = if (ignore.branch.condition.value.payload.get(.bool))
                    ignore.branch.true.*
                else
                    ignore.branch.false.*;
                statement.* = b;
            }
        },
        else => {},
    }
}
pub fn foldType(self: *Parser, @"type": Type) Error!Type {
    return switch (@"type") {
        .unknown => |unknown| blk: {
            @constCast(unknown).* = try self.foldExpression(unknown.*, .full);
            break :blk expressionAsType(unknown);
        },
        .type_of => |type_of| blk: {
            @constCast(type_of).* = try self.foldExpression(type_of.*, .full);
            break :blk self.typeOf(type_of);
        },
        else => @"type",
    };
}

fn foldExpression(self: *Parser, expr: Expression, access: FoldAccess) Error!Expression {
    return switch (expr) {
        .identifier => |identifier| blk: {
            for (self.access_dependency_stack.items) |i|
                if (util.strEql(identifier, i))
                    return self.errorOut(.{ .dependency_loop = identifier });
            try self.access_dependency_stack.append(self.arena.allocator(), identifier);
            defer _ = self.access_dependency_stack.pop();

            const vr = try self.getVariableReference(identifier);
            break :blk if (access == .full and vr.var_ref.qualifier == .@"const" and vr.value != .null)
                vr.value
            else
                .{ .var_ref = vr.var_ref };
        },
        .value => |value| blk: {
            if (value.type == .entry_point) {
                try self.foldScope(&self.getEntryPoint(value.payload.get(.entry_point)).scope);
            }
            break :blk expr;
        },
        .branch => |branch| blk: {
            const cond = try self.implicitCast(.bool, try self.foldExpression(branch.condition.*, access));
            if (cond == .value) {
                const b = if (cond.value.payload.get(.bool)) branch.true else branch.false;
                break :blk switch (b.*) {
                    .ignore => |ignore| try self.foldExpression(ignore, access),
                    else => {
                        branch.condition.* = cond;
                        try self.foldStatement(b);
                        break :blk expr;
                    },
                };
            } else {
                try self.foldStatement(branch.true);
                try self.foldStatement(branch.false);
            }
            branch.condition.* = cond;
            break :blk .{ .branch = branch };
        },
        .bin_op => |bin_op| blk: {
            bin_op.left.* = try self.foldExpression(bin_op.left.*, access);
            bin_op.right.* = try self.foldExpression(bin_op.right.*, access);
            break :blk expr;
        },

        else => expr,
    };
}
pub const FoldAccess = enum { none, constant, full };

pub fn implicitCast(self: *Parser, @"type": Type, expr: Expression) Error!Expression {
    _ = .{ self, @"type", expr };
    const type_of = self.typeOf(&expr);
    if (type_of == .unknown or
        @"type" == .unknown or
        std.meta.eql(@"type", type_of)) return expr;
    return switch (expr) {
        .value => |value| .{
            .value = switch (@"type") {
                .compfloat => if (value.type == .compint)
                    Value.create(.compfloat, @floatFromInt(value.payload.get(.compint)))
                else
                    return self.errorOut(.cant_implicitly_cast),
                .scalar => |scalar| switch (value.type) {
                    .compint => inline for (tp.Scalar.all) |s| {
                        if (std.meta.eql(scalar, s)) break Value.createFromType(
                            .{ .scalar = s },
                            util.numericCast(
                                s.ToZig(),
                                value.payload.get(.compint),
                            ),
                        );
                    } else unreachable,
                    .compfloat => if (scalar.layout == .float)
                        switch (scalar.width) {
                            inline else => |w| Value.createFromType(
                                .{ .scalar = .{ .width = w, .layout = .float } },
                                util.numericCast(
                                    std.meta.Float(@max(16, @intFromEnum(w))),
                                    value.payload.get(.compfloat),
                                ),
                            ),
                        }
                    else
                        return self.errorOut(.cant_implicitly_cast),

                    else => return self.errorOut(.cant_implicitly_cast),
                },
                else => return self.errorOut(.cant_implicitly_cast),
            },
        },
        else => expr,
        // else => return self.errorOut(.cant_implicitly_cast),
    };
}
pub fn exprFromType(@"type": Type) Expression {
    return .{ .value = .{ .type = .type, .payload = .{ .type = @"type" } } };
}
pub fn typeOf(self: *Parser, expr: *const Expression) Type {
    _ = self;
    return switch (expr.*) {
        .value => |value| value.type,
        .var_ref => |var_ref| var_ref.type,
        // bin_op: BinOp,
        // .branch => |branch|
        // branch: Branch,
        // cast: Cast,

        // null,
        else => .{ .type_of = expr },
    };
}
pub fn typeOfAlloc(self: *Parser, expr: Expression) Error!Type {
    const result = self.typeOf(&expr);
    return if (result == .type_of)
        .{ .type_of = try self.createVal(expr) }
    else
        result;
}

pub fn expressionAsType(expr: *const Expression) Type {
    return if (expr.* == .value and expr.value.type == .type)
        expr.value.payload.get(.type)
    else
        .{ .unknown = expr };
}
pub fn expressionAsTypeAlloc(self: *Parser, expr: Expression) Error!Type {
    return if (expr == .value and expr.value.type == .type)
        expr.value.payload.get(.type)
    else
        .{ .unknown = try self.createVal(expr) };
}

//====================

pub fn createVal(self: *Parser, value: anytype) !*@TypeOf(value) {
    const ptr = try self.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}
pub fn create(self: *Parser, T: type) !*T {
    return self.arena.allocator().create(T);
}

const ErrorInfo = error_message.ErrorInfo;
const Error = hgsl.Error;
const List = std.ArrayList;
const Allocator = std.mem.Allocator;
const BinaryOperator = Tokenizer.BinaryOperator;
const UnaryOperator = Tokenizer.UnaryOperator;
pub const Type = tp.Type;
pub const Token = Tokenizer.Token;
