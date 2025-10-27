const std = @import("std");
const hgsl = @import("hgsl");
const builtin = @import("builtin");

pub fn main() !void {
    var buf: [128]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&buf);

    const allocator = std.heap.page_allocator;

    var compiler: hgsl.Compiler = .new(null, &out_writer.interface);
    for ([_][]const u8{
        // "func.hgsl",
        // "vertfrag.hgsl",
        "../Huge/shader.hgsl",
        // "source.hgsl",
    }) |path| {
        std.debug.print("======{s}=======\n", .{path});
        var timer = try std.time.Timer.start();
        const t_count = if (builtin.mode == .Debug) 1 else 1000;
        for (0..t_count) |_| {
            // const compiled = try hgsl.compileFile(allocator, path, &out_writer.interface);
            const compiled = try compiler.compileFile(path);
            defer allocator.free(compiled);

            const out_file = try std.fs.cwd().createFile("out.spv", .{ .read = true });
            defer out_file.close();
            var out_buf: [128]u8 = undefined;
            var writer = out_file.writer(&out_buf);

            _ = try writer.interface.write(@ptrCast(@alignCast(compiled)));
            try writer.interface.flush();
        }
        if (builtin.mode != .Debug) std.debug.print("Time: {d}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0 / t_count});
    }
}
