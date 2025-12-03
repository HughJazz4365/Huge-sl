const std = @import("std");
const util = @import("util.zig");
pub const Tokenizer = @import("Tokenizer.zig");
pub const Parser = @import("Parser.zig");
pub const SpirvGen = @import("spirvgen.zig");
pub const ErrCtx = @import("errorctx.zig");

// pub const Error = error{};

pub const Compiler = struct {
    result_arena: std.heap.ArenaAllocator,
    err_ctx: ErrCtx = .{},
    settings: Settings,

    cache: [cache_size]CachedShader = @splat(.{}),
    const cache_size: usize = 10;

    pub fn compileFile(self: *Compiler, io: std.Io, allocator: Allocator, path: []const u8) anyerror!Result {
        if (path.len == 0) return error.EmptyPath;
        const source = try readFile(io, allocator, path);
        const hash = std.hash.Fnv1a_128.hash(source);

        var last_replacable: usize = cache_size;
        var hit = false;

        const index = for (&self.cache, 0..) |*c, i| {
            if (util.strEql(path, c.path)) {
                if (c.hash == hash) {
                    c.hits += 1;
                    hit = true;
                    break i;
                }
                break i;
            }
            if (c.hits == 0) break i;

            c.misses += @intFromBool(c.hits > 0);
            if (c.misses > c.hits) last_replacable = i;
        } else last_replacable;

        if (index + 1 < cache_size) {
            for (self.cache[index + 1 ..]) |*c| {
                c.misses += @intFromBool(c.hits > 0);
            }
        }
        if (hit) return self.cache[index].result;

        const result = try self.compileRaw(allocator, source, path);
        if (index < cache_size) {
            if (last_replacable == index)
                self.cache[index].result.deinit(self.result_arena.allocator());
            self.cache[index] = .{
                .hash = hash,
                .result = result,
                .path = path,
                .hits = self.cache[index].hits + 1,
                .misses = self.cache[index].misses,
            };
        }
        return result;
    }

    fn compileRaw(self: *Compiler, allocator: Allocator, source: []const u8, path: []const u8) !Result {
        self.err_ctx.reinit(source, path);
        var tokenizer: Tokenizer = .new(source, &self.err_ctx);
        const file_name = path;
        var parser = Parser.parse(
            allocator,
            &tokenizer,
            self.settings,
            file_name,
        ) catch |err| return self.err_ctx.outputUpdateIfEmpty(err);
        defer parser.deinit();

        // return .{};
        return try SpirvGen.generate(&parser, self.result_arena.allocator(), .{
            .major = 1,
            .minor = 6,
        });
    }

    /// result allocator only allocates results of the code generation
    /// but not the intermediate resources for other compilation stages
    pub fn new(
        result_allocator: Allocator,
        err_writer: ?*std.Io.Writer,
        settings: Settings,
    ) Compiler {
        return .{
            .result_arena = .init(result_allocator),
            .err_ctx = .{ .out_writer = err_writer },
            .settings = settings,
        };
    }
    pub fn deinit(self: *Compiler) void {
        // for (&self.cache) |c| {
        //     if (c.hits == 0) break;
        //     c.result.deinit(self.result_arena.allocator());
        // }
        self.result_arena.deinit();
    }
    const CachedShader = struct {
        path: []const u8 = "\x00",
        hash: u128 = 0,
        result: Result = .{},
        hits: usize = 0,
        misses: usize = 0,
    };
};
pub const IOType = union(enum) {
    scalar: Scalar,
    vector: struct { len: VectorLen, component: Scalar },
    pub fn eql(a: IOType, b: IOType) bool {
        return if (a == .scalar and b == .scalar)
            a.scalar == b.scalar
        else if (a == .vector and b == .vector)
            a.vector.len == b.vector.len and a.vector.component == b.vector.component
        else
            return false;
    }
    pub fn format(self: IOType, writer: *std.Io.Writer) !void {
        switch (self) {
            .scalar => |scalar| try writer.print("{s}", .{@tagName(scalar)}),
            .vector => |vector| try writer.print("{s}vec{d}", .{
                switch (vector.component) {
                    .f32, .f64 => "",
                    .u32, .u64 => "u",
                    .i32, .i64 => "i",
                },
                @intFromEnum(vector.len),
            }),
        }
    }
};
pub const VectorLen = enum(u32) {
    _2 = 2,
    _3 = 3,
    _4 = 4,
};
const Scalar = enum(u32) {
    f32,
    u32,
    i32,
    f64,
    u64,
    i64,
};

pub const Result = struct {
    bytes: []u8 = &.{},
    entry_point_infos: []const EntryPointInfo = &.{},
    pub fn deinit(self: Result, allocator: Allocator) void {
        allocator.free(self.bytes);
        for (self.entry_point_infos) |m| m.deinit(allocator);
        allocator.free(self.entry_point_infos);
    }
    pub fn format(self: Result, writer: *std.Io.Writer) !void {
        try writer.print("bytes.len = {}, entry points({}):\n", .{
            self.bytes.len,
            self.entry_point_infos.len,
        });
        for (self.entry_point_infos) |ep_info|
            try writer.print("{f}\n", .{ep_info});
    }
};
pub const EntryPointInfo = struct {
    name: [:0]const u8 = "",
    stage_info: StageInfo = undefined,

    push_constant_mappings: []const PushConstantMapping = &.{},
    bindings: []const Binding = &.{},

    io_mappings_ptr: [*]const IOMapping = undefined,
    input_count: u32 = 0,
    output_count: u32 = 0,
    pub fn inputMappings(self: EntryPointInfo) []const IOMapping {
        return self.io_mappings_ptr[0..self.input_count];
    }
    pub fn outputMappings(self: EntryPointInfo) []const IOMapping {
        return self.io_mappings_ptr[self.input_count .. self.input_count + self.output_count];
    }
    pub fn deinit(self: *const EntryPointInfo, allocator: Allocator) void {
        allocator.free(self.name);

        const io_mappings = self.io_mappings_ptr[0 .. self.input_count + self.output_count];
        for (io_mappings) |iom| allocator.free(iom.name);
        allocator.free(io_mappings);

        for (self.push_constant_mappings) |pcm| allocator.free(pcm.name);
        allocator.free(self.push_constant_mappings);
    }
    pub fn format(self: EntryPointInfo, writer: *std.Io.Writer) !void {
        try writer.print("[{f}] {s}{{\n", .{ self.stage_info, self.name });

        try writer.print("<==INPUT==>({}):\n", .{self.inputMappings().len});
        for (self.inputMappings()) |im|
            try writer.print("{f}\n", .{im});

        try writer.print("<==OUTPUT==>({}):\n", .{self.outputMappings().len});
        for (self.outputMappings()) |om|
            try writer.print("{f}\n", .{om});

        try writer.print("<==BINDINGS==>({}):\n", .{self.bindings.len});
        for (self.bindings) |ou|
            try writer.print("{f}\n", .{ou});

        try writer.print("<==PUSH=CONSTANTS==>({}):\n", .{self.push_constant_mappings.len});
        for (self.push_constant_mappings) |pc|
            try writer.print("{f}\n", .{pc});

        try writer.print("}}", .{});
    }
};
pub const IOMapping = struct {
    name: []const u8,
    location: u32 = undefined,
    size: u32 = 0,

    type: IOType,
    pub fn format(self: IOMapping, writer: *std.Io.Writer) !void {
        try writer.print("[Location = {d:2}, \"{s}\": {f}]", .{ self.location, self.name, self.type });
    }
};

pub const Stage = enum(u64) { vertex, fragment, compute };
pub const StageInfo = union(Stage) {
    vertex,
    fragment,
    compute: [3]u32,
    pub fn format(self: StageInfo, writer: *std.Io.Writer) !void {
        try writer.print("{s}", .{@tagName(self)});
        switch (self) {
            inline else => |val| if (@TypeOf(val) != void)
                try writer.print(":{any}", .{val}),
        }
    }
};

pub const PushConstantMapping = struct {
    name: []const u8,
    offset: u32,
    size: u32,
    pub fn format(self: PushConstantMapping, writer: *std.Io.Writer) !void {
        try writer.print("[Range ({d:3}:{d:3}), \"{s}\"]", .{
            self.offset,
            self.offset + self.size,
            self.name,
        });
    }
};
pub const Binding = struct {
    name: []const u8,
    type: BindingType = undefined,
    binding: u32,
    set: u32,
    pub fn format(self: Binding, writer: *std.Io.Writer) !void {
        try writer.print("[Set = {d:2}, Binding = {d:2}, \"{s}\": {f}]", .{
            self.set,
            self.binding,
            self.name,
            self.type,
        });
    }
};
pub const BindingType = union(enum) {
    runtime_array: DescriptorType,
    array: DescriptorArray,
    descriptor: DescriptorType,
    pub fn format(self: BindingType, writer: *std.Io.Writer) !void {
        switch (self) {
            .descriptor => |descriptor| try writer.print("{s}", .{@tagName(descriptor)}),
            .runtime_array => |descriptor| try writer.print("[]{s}", .{@tagName(descriptor)}),
            .array => |array| try writer.print("[{d}]{s}", .{ array.len, @tagName(array.descriptor_type) }),
        }
    }
};
pub const DescriptorArray = struct { len: u32, descriptor_type: DescriptorType };
pub const DescriptorType = enum {
    ubo,
    ssbo,
    sampled_texture,
    texture,
};

pub const Settings = struct {
    target_env: TargetEnv = .vulkan1_4,
    optimize: Optimize = .none,

    max_push_constant_buffer_size: u32 = 128,
};
pub const Optimize = enum { none, speed };
pub const TargetEnv = enum { vulkan1_4, opengl_spirv, opengl_glsl };

pub const minimal =
    \\const vert = entrypoint(.vertex){}
    \\const frag = entrypoint(.fragment){out col: vec4 = .{1,0,1,0}}
;
const Allocator = std.mem.Allocator;
pub fn readFile(io: std.Io, allocator: Allocator, path: []const u8) ![]const u8 {
    const source_file = try std.Io.Dir.cwd().openFile(io, path, .{});
    defer source_file.close(io);

    var reader = source_file.reader(io, &.{});

    var alloc_writer = std.Io.Writer.Allocating.init(allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    return alloc_writer.writer.buffered();
}
const List = std.ArrayList;
