const std = @import("std");
const util = @import("util.zig");
pub const Tokenizer = @import("Tokenizer.zig");
pub const Parser = @import("Parser.zig");
pub const SpirvGen = @import("spirvgen.zig");
pub const ErrCtx = @import("errorctx.zig");

pub const Error = error{};
pub fn compileFile(
    allocator: Allocator,
    path: []const u8,
    err_writer: ?*std.Io.Writer,
    settings: Settings,
) ![]u32 {
    const source = try readFile(allocator, path);
    return try compile(allocator, source, path, err_writer, settings);
}

pub fn compile(
    allocator: Allocator,
    source: []const u8,
    path: []const u8,
    err_writer: ?*std.Io.Writer,
    settings: Settings,
) ![]u32 {
    var error_ctx: ErrCtx = .{};
    error_ctx.init(source, path, err_writer);

    var tokenizer: Tokenizer = .new(source, &error_ctx);
    var parser = Parser.parse(allocator, &tokenizer, settings, "FILE") catch |err| return error_ctx.outputUpdateIfEmpty(err);
    defer parser.deinit();

    return try SpirvGen.generate(&parser, allocator);
}

pub const Compiler = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,

    err_ctx: ErrCtx = .{},
    settings: Settings,

    cache: [cache_size]CachedShader = @splat(.{}),
    const cache_size: usize = 10;

    pub fn compileFile(self: *Compiler, path: []const u8) !Result {
        const source = try readFile(self.allocator, path);
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

        const result = try self.compileRaw(source, path);
        if (index < cache_size) {
            if (last_replacable == index)
                self.cache[index].result.deinit(self.allocator);
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

    fn compileRaw(self: *Compiler, source: []const u8, path: []const u8) !Result {
        self.err_ctx.reinit(source, path);
        var tokenizer: Tokenizer = .new(source, &self.err_ctx);
        const file_name = path;
        var parser = Parser.parse(
            self.allocator,
            &tokenizer,
            self.settings,
            file_name,
        ) catch |err| return self.err_ctx.outputUpdateIfEmpty(err);
        defer parser.deinit();

        return try SpirvGen.generate(&parser, self.arena.allocator());
    }

    pub fn new(allocator: ?Allocator, err_writer: ?*std.Io.Writer, settings: Settings) Compiler {
        const a = if (allocator) |a| a else std.heap.page_allocator;
        return .{
            .allocator = a,
            .arena = .init(a),

            .err_ctx = .{ .out_writer = err_writer },
            .settings = settings,
        };
    }
    pub fn deinit(self: *Compiler) void {
        for (&self.cache) |c| {
            if (c.hits == 0) break;
            c.result.deinit(self.arena.allocator());
        }
        self.arena.deinit();
    }
    const CachedShader = struct {
        path: []const u8 = "\x00",
        hash: u128 = 0,
        result: Result = .{},
        hits: usize = 0,
        misses: usize = 0,
    };
};
pub const Result = struct {
    bytes: []u8 = &.{},
    entry_point_infos: []const EntryPointInfo = &.{},
    pub fn alloc(self: Result, allocator: Allocator) Error!Result {
        _ = allocator;
        return self;
    }
    pub fn deinit(self: Result, allocator: Allocator) void {
        allocator.free(self.bytes);
        for (self.entry_point_infos) |m| m.deinit(allocator);
        allocator.free(self.entry_point_infos);
    }
};
pub const EntryPointInfo = struct {
    name: [:0]const u8,
    stage_info: StageInfo,

    push_constant_mappings: []const PushConstantMapping,
    opaque_uniform_mappings: []const OpaqueUniformMapping,

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
        for (self.push_constant_mappings) |pcm| allocator.free(pcm.name);

        const io_mappings = self.io_mappings_ptr[0 .. self.input_count + self.output_count];
        for (io_mappings) |iom| allocator.free(iom.name);
        allocator.free(io_mappings);

        allocator.free(self.push_constant_mappings);
    }
};
pub const IOMapping = struct {
    name: []const u8,
    location: u32,
    size: u32 = 0,
};

pub const Stage = enum(u64) { vertex, fragment, compute };
pub const StageInfo = union(Stage) {
    vertex,
    fragment,
    compute: [3]u32,
};

pub const PushConstantMapping = struct {
    name: []const u8,
    offset: u32,
    size: u32,
};
pub const OpaqueUniformMapping = struct {
    name: []const u8,
    type: OpaqueType,
    binding: u32,
};
pub const OpaqueType = enum { buffer, texture };

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
pub fn readFile(allocator: Allocator, path: []const u8) ![]const u8 {
    const source_file = try std.fs.cwd().openFile(path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    return alloc_writer.writer.buffered();
}
const List = std.ArrayList;
