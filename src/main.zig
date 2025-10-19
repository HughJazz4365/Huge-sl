const std = @import("std");
const hgsl = @import("hgsl");
const builtin = @import("builtin");

pub fn main() !void {
    var buf: [128]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&buf);
    _ = &out_writer;

    const allocator = std.heap.page_allocator;

    // _ = path;
    // const compiled = try hgsl.compile(allocator, hgsl.minimal_frag, "minimal", &out_writer.interface);
    for ([_][]const u8{
        "func.hgsl",
        "source.hgsl",
    }) |path| {
        std.debug.print("======{s}=======\n", .{path});
        var timer = try std.time.Timer.start();
        const t_count = if (builtin.mode == .Debug) 1 else 100;
        for (0..t_count) |_| {
            const compiled = try hgsl.compileFile(allocator, path, &out_writer.interface);
            defer allocator.free(compiled);

            // write spirv binary to a file
            const out_file = try std.fs.cwd().openFile("out.spv", .{ .mode = .write_only });
            defer out_file.close();
            var out_buf: [128]u8 = undefined;
            var writer = out_file.writer(&out_buf);

            try writer.interface.writeSliceEndian(u32, compiled, .little);
            try writer.interface.flush();
        }
        if (builtin.mode != .Debug) std.debug.print("Time: {d}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0 / t_count});
    }
}
