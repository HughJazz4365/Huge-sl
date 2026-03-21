const std = @import("std");
const util = @import("util.zig");
const error_message = @import("errorMessage.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const IR = @import("IR.zig");

const spirv = @import("spirv.zig");

pub const CI = i128;
pub const CF = f64;

pub fn test_() !void {
    inline for (&[_]type{
        Parser.NodeEntry,
        Parser.StructEntry,
        Parser.ScopeEntry,
        Parser.TypeEntry,
        Parser.FunctionEntry,
        Parser.VariableReference,
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

        const settings: Settings = .{
            .target = .{ .vulkan = .{ .spirv_version = .v1_5 } },
        };
        //============================
        var tok: Tokenizer = .{ .full_source = source, .path = path };
        tok.tokenize(allocator) catch |err| {
            if (err == Error.CompilationError)
                try error_message.printErrorMessageTokenizer(tok, &file_writer.interface);
            return err;
        };

        var parser = try Parser.new(allocator);
        defer parser.deinit();
        parser.parse(tok) catch |err| {
            if (err == Error.CompilationError)
                try error_message.printErrorMessageParser(&parser, &file_writer.interface);
            return err;
        };
        _ = settings;

        var ir = try IR.new(&parser, allocator);
        defer ir.deinit();
        try ir.lower();

        // const result = try spirv.generate(&ir, allocator, settings);
        // _ = result;

        //============================

        const new_timestamp = clock.now(io);
        measure += @intCast(timestamp.durationTo(new_timestamp).nanoseconds);
        timestamp = new_timestamp;

        parser.dump();
        // ir.dump();
        // std.debug.print("RESULT: {any}\n", .{@as([]u32, @ptrCast(@alignCast(result.bytes)))});
    }
    std.debug.print(
        "time: {d} mcs(tc: {d})\n",
        .{ @as(f64, @floatFromInt(measure)) / 1_000.0 / test_count, test_count },
    );
}

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
    const push_buffer_size_vulkan_required = 128;

    target: Target = .{ .vulkan = .{} },
    optimize: Optimize = .full,

    vendor: GpuVendor = .other,

    push_constant_buffer_size: u32 =
        push_buffer_size_vulkan_required,

    const Target = union(enum) { vulkan: VulkanSettings };
    const Optimize = enum { none, full };

    const VulkanSettings = struct {
        buffer_device_address: bool = true,
        spirv_version: SpirvVersion = .v1_0,
        bindless_implementation: enum {
            runtime_arrays,
            descriptor_heaps, //??
        } = .runtime_arrays,
        runtime_arrays_descriptor_set: u32 = 0,
        storage_texture_rt_array_binding: u32 = 0,
        sampled_texture_rt_array_binding: u32 = 1,

        buffer_rt_array_binding_fallback: u32 = 2,
    };
    fn serialize() void {} //??
    fn deserialize() void {}
    pub const SpirvVersion = enum(u32) {
        v1_0 = spirv.versionWord(1, 0), //vulkan 1.0
        v1_1 = spirv.versionWord(1, 1),
        v1_3 = spirv.versionWord(1, 3), //vulkan 1.1
        v1_4 = spirv.versionWord(1, 4),
        v1_5 = spirv.versionWord(1, 5), //vulkan 1.2
        v1_6 = spirv.versionWord(1, 6), //vulkan 1.3
        pub inline fn higher(self: SpirvVersion, v: SpirvVersion) bool {
            return @intFromEnum(self) >= @intFromEnum(v);
        }
    };
    pub const GpuVendor = enum { nvidia, amd, other };
};

pub const Result = struct {
    bytes: []u8,
    entry_points: []EntryPoint = &.{},

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
