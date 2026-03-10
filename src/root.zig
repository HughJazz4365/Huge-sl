const std = @import("std");
const util = @import("util.zig");
const error_message = @import("errorMessage.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const IR = @import("IR.zig");

pub const CI = i128;
pub const CF = f64;

pub const ShaderStage = enum { fragment, vertex, compute };
pub const ShaderStageInfo = union(ShaderStage) {
    fragment,
    vertex,
    compute: [3]u32,
};

pub const Error = error{
    OutOfMemory,
    WriteFailed,

    FileReadFailed,

    CompilationError,
};
pub const Settings = struct {
    push_constant_buffer_size: u32 =
        push_buffer_size_vulkan_required,
    target: Target,
    optimize: Optimize,

    const push_buffer_size_vulkan_required = 128;

    const Target = union(enum) { vulkan: VulkanSettings };
    const Optimize = enum { none, full };

    const VulkanSettings = struct {
        bindless_implementation: enum {
            runtime_array_bindings,
            descriptor_heaps, //??
        },
        storage_texture_rt_array_binding: u32 = 0,
        sampled_texture_rt_array_binding: u32 = 1,

        buffer_rt_array_binding_fallback: u32 = 2,
    };
    fn serialize() void {} //??
    fn deserialize() void {}
};

pub const Result = struct {
    bytes: []u8,
    entry_points: []EntryPoint,

    const EntryPoint = struct {
        name: [:0]const u8,
        stage_info: ShaderStageInfo,

        push_constants: []const PushConstant = &.{},

        io_entries: [*]const IOEntry = undefined,
        input_count: u32 = 0,
        output_count: u32 = 0,
    };
    const IOEntry = struct {
        interpolation: Interpolation = .smooth,
        type: IOType,
    };
    const Interpolation = enum { smooth, linear, flat };
    const IOType = union(enum) {
        scalar: Parser.TypeEntry.Scalar,
        vector: Parser.TypeEntry.Vector,
        // matrix: Parser.TypeEntry.Matrix,
    };
    const PushConstant = struct {
        name: []const u8,
        offset: usize,
        size: usize,
    };
    pub fn free(self: Result, allocator: Allocator) void {
        allocator.free(self.bytes);
    }
};

pub fn test_() !void {
    inline for (&[_]type{
        Parser.NodeEntry,
        Parser.StructEntry,
        Parser.ScopeEntry,
        Parser.TypeEntry,
        Parser.FunctionEntry,
        Tokenizer.TokenEntry,
    }) |_| {}
    // }) |T|
    // std.debug.print("size of {s}: {d}, align: {d}\n", .{ @typeName(T), @sizeOf(T), @alignOf(T) });

    var threaded_io = std.Io.Threaded.init_single_threaded;
    const io = threaded_io.io();
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [128]u8 = undefined;
    var file_writer = std.Io.File.stdout().writer(io, &buf);

    const path = "samples/test.hgsl";
    const source = readFile(io, allocator, path) catch
        return Error.FileReadFailed;

    const clock = std.Io.Clock.awake;
    var timestamp = clock.now(io);
    var measure: u64 = 0;
    const test_count = 1;
    for (0..test_count) |_| {
        timestamp = clock.now(io);

        //============================
        var tok: Tokenizer = .{ .full_source = source, .path = path };
        tok.tokenize(allocator) catch |err| {
            if (err == Error.CompilationError)
                try error_message.printErrorMessageTokenizer(tok, &file_writer.interface);
            return err;
        };

        var parser = try Parser.new(allocator);
        parser.parse(tok) catch |err| {
            if (err == Error.CompilationError)
                try error_message.printErrorMessageParser(&parser, &file_writer.interface);
            return err;
        };
        var ir = try IR.new(&parser, allocator);
        defer ir.deinit();

        try ir.lower();

        // parser.deinit();
        //============================

        const new_timestamp = clock.now(io);
        measure += @intCast(timestamp.durationTo(new_timestamp).nanoseconds);
        timestamp = new_timestamp;
        parser.dump();
        ir.dump();
        parser.deinit();
    }
    std.debug.print(
        "time: {d} mcs(tc: {d})\n",
        .{ @as(f64, @floatFromInt(measure)) / 1_000.0 / test_count, test_count },
    );
}
pub fn compile(io: std.Io, allocator: Allocator, path: []const u8, error_writer: *std.Io.Writer) Error![]u32 {
    const source = readFile(io, allocator, path) catch
        return Error.FileReadFailed;
    defer allocator.free(source);

    var parser = try Parser.new(.{
        .source = source,

        .full_source = source,
        .path = path,
    }, allocator);
    parser.parse() catch |err| {
        try @import("errorMessage.zig").printErrorMessage(error_writer, parser.tokenizer.error_info);
        return err;
    };
    return &.{};
}

pub fn readFile(io: std.Io, allocator: Allocator, path: []const u8) ![]const u8 {
    const source_file = try std.Io.Dir.cwd().openFile(io, path, .{});
    defer source_file.close(io);

    var reader = source_file.reader(io, &.{});

    var alloc_writer = std.Io.Writer.Allocating.init(allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    return alloc_writer.writer.buffered();
}
const Allocator = std.mem.Allocator;
const List = std.ArrayList;
