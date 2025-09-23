const std = @import("std");
const c = @cImport(@cInclude("shaderc/shaderc.h"));
pub const ShaderKind = enum(c.shaderc_shader_kind) {
    vertex = c.shaderc_vertex_shader,
    fragment = c.shaderc_fragment_shader,
};

pub fn glslSpirvDissasembly(
    glsl_path: []const u8,
    kind: ShaderKind,
    entry_point_name: [:0]const u8,
    optimize: bool,
) ![]const u8 {
    const source_file = try std.fs.cwd().openFile(glsl_path, .{});
    var reader = source_file.reader(&.{});

    var alloc_writer = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    _ = try reader.interface.streamRemaining(&alloc_writer.writer);

    const source = alloc_writer.writer.buffered();
    const compiler = c.shaderc_compiler_initialize() orelse return error.CompilerCreationFailed;

    const opt = c.shaderc_compile_options_initialize();
    defer c.shaderc_compile_options_release(opt);
    c.shaderc_compile_options_set_optimization_level(opt, if (optimize) c.shaderc_optimization_level_performance else c.shaderc_optimization_level_zero);
    c.shaderc_compile_options_set_source_language(opt, c.shaderc_source_language_glsl);
    c.shaderc_compile_options_set_target_env(opt, c.shaderc_target_env_vulkan, c.shaderc_env_version_vulkan_1_3);

    const result = c.shaderc_compile_into_spv_assembly(
        compiler,
        @ptrCast(source.ptr),
        source.len,
        @intFromEnum(kind),
        "filename",
        @ptrCast(entry_point_name),
        opt,
    );
    errdefer c.shaderc_result_release(result);
    if (c.shaderc_result_get_compilation_status(result) != c.shaderc_compilation_status_success) {
        std.debug.print("shader compilation failed with error message:\n{s}", .{c.shaderc_result_get_error_message(result)});
        return error.CompilationFailed;
    }
    defer c.shaderc_result_release(result);
    const length = c.shaderc_result_get_length(result);
    const buf = try std.heap.page_allocator.alloc(u8, length);
    @memcpy(buf, c.shaderc_result_get_bytes(result)[0..length]);
    return buf;
}

const Allocator = std.mem.Allocator;
