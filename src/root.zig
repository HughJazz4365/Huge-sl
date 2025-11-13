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

    return try SpirvGen.generate(&parser);
}

pub const Compiler = struct {
    allocator: Allocator,

    err_ctx: ErrCtx = .{},
    settings: Settings,

    cache: [cache_size]CachedShader = @splat(.{}),
    const cache_size: usize = 10;

    pub fn compileFile(self: *Compiler, path: []const u8) ![]u8 {
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
        if (hit) return self.cache[index].bytes;

        const result: []u8 = try self.compileRaw(source, path);
        if (index < cache_size) self.cache[index] = .{
            .hash = hash,
            .bytes = result,
            .path = path,
            .hits = self.cache[index].hits + 1,
            .misses = self.cache[index].misses,
        };
        return result;
    }

    fn compileRaw(self: *Compiler, source: []const u8, path: []const u8) ![]u8 {
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

        return @ptrCast(@alignCast(try SpirvGen.generate(&parser)));
    }

    pub fn new(allocator: ?Allocator, err_writer: ?*std.Io.Writer, settings: Settings) Compiler {
        return .{
            .allocator = if (allocator) |a| a else std.heap.page_allocator,
            .err_ctx = .{ .out_writer = err_writer },
            .settings = settings,
        };
    }
    pub fn deinit(self: *Compiler) void {
        for (&self.cache) |c| {
            if (c.hits == 0) break;
            self.allocator.free(c.bytes);
        }
    }
    const CachedShader = struct {
        path: []const u8 = "\x00",
        hash: u128 = 0,
        bytes: []u8 = &.{},
        hits: usize = 0,
        misses: usize = 0,
    };
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
pub fn readFile(allocator: Allocator, path: []const u8) ![]const u8 {
    const source_file = try std.fs.cwd().openFile(path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    return alloc_writer.writer.buffered();
}
const List = std.ArrayList;
