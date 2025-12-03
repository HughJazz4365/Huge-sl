const std = @import("std");
const hgsl = @import("hgsl");
const builtin = @import("builtin");

pub fn main() !void {
    // const v: @Vector(3, f16) = .{ 1, 2, 3 };
    // std.debug.print("VELEM: {d}\n", .{(&v)[0]});
    // if (true) return;
    var threaded: std.Io.Threaded = .init_single_threaded;
    const io = threaded.ioBasic();

    var buf: [128]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&buf);

    var compiler: hgsl.Compiler = .new(io, null, &out_writer.interface, .{
        .optimize = if (builtin.mode == .Debug) .none else .speed,
    });
    defer compiler.deinit();
    for ([_][]const u8{
        "test.hgsl",
        // "../Huge/shader.hgsl",
        // "source.hgsl",
        // "vertfrag.hgsl",
    }) |path| {
        std.debug.print("======{s}=======\n", .{path});
        var timer = try std.time.Timer.start();
        const t_count = if (builtin.mode == .Debug) 1 else 1000;
        for (0..t_count) |_| {
            // const compiled = try hgsl.compileFile(allocator, path, &out_writer.interface);
            const compiled = try compiler.compileFile(path);
            if (false) for (compiled.mappings) |m| {
                std.debug.print(
                    \\name: {s}
                    \\pc: {any}
                    \\ou: {any}
                    \\
                , .{ m.name, m.push_constant_mappings, m.opaque_uniform_mappings });
            };
            // defer allocator.free(compiled);

            const out_file = try std.fs.cwd().createFile("out.spv", .{ .read = true });
            defer out_file.close();
            var out_buf: [128]u8 = undefined;
            var writer = out_file.writer(&out_buf);

            if (builtin.mode == .Debug)
                std.debug.print("RESULT: {f}\n", .{compiled});

            _ = try writer.interface.write(compiled.bytes);
            try writer.interface.flush();
        }
        if (builtin.mode != .Debug or true) std.debug.print("Time: {d}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0 / t_count});
    }
    // for (&compiler.cache) |c| {
    //     if (c.hits == 0) break;
    //     std.debug.print(
    //         \\path: {s}
    //         \\bytes len: {d}
    //         \\hits: {d}, misses: {d}
    //         \\=====================
    //         \\
    //     , .{ c.path, c.bytes.len, c.hits, c.misses });
    // }
}
