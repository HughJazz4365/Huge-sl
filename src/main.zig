const std = @import("std");
const hgsl = @import("hgsl");
const builtin = @import("builtin");

pub fn main() !void {
    //0.41
    // var thread_safe_alloc: std.heap.ThreadSafeAllocator = .{ .child_allocator = std.heap.page_allocator };
    // const allocator = thread_safe_alloc.allocator();
    // var threaded: std.Io.Threaded = .init(allocator);

    //0.6
    const allocator = std.heap.page_allocator;
    var threaded: std.Io.Threaded = .init_single_threaded;

    const io = threaded.ioBasic();

    var buf: [128]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&buf);

    var compiler: hgsl.Compiler = .new(
        allocator,
        &out_writer.interface,
        .{
            .optimize = if (builtin.mode == .Debug) .none else .speed,
            .spirv_version = .{ .major = 1, .minor = 5 },
        },
    );
    defer compiler.deinit();
    const path_arr = [_][]const u8{
        // "test.hgsl",
        "../Huge/shader.hgsl",
        // "../Huge/triangle.hgsl",
        // "source.hgsl",
        // "func.hgsl",
        // "vertfrag.hgsl",
    };
    var futures: [path_arr.len]std.Io.Future(anyerror!hgsl.Result) = undefined;

    var timer = try std.time.Timer.start();

    for (&path_arr, &futures) |path, *f| {
        //     f.* = io.async(hgsl.Compiler.compileFile, .{ &compiler, io, allocator, path });
        // }
        // for (&path_arr, &futures) |path, *f| {
        _ = &f;
        if (builtin.mode == .Debug)
            std.debug.print("======{s}=======\n", .{path});
        const compiled = try compiler.compileFile(io, allocator, path);
        // const compiled = try f.await(io);

        const out_file = try std.fs.cwd().createFile("out.spv", .{ .read = true });
        defer out_file.close();
        var out_buf: [128]u8 = undefined;
        var writer = out_file.writer(&out_buf);
        _ = try writer.interface.write(compiled.bytes);
        try writer.interface.flush();

        // if (builtin.mode == .Debug)
        //     std.debug.print("RESULT: {f}\n", .{compiled});
    }
    if (builtin.mode != .Debug or true)
        std.debug.print("Time: {d}ms\n", .{@as(f64, @floatFromInt(timer.read())) / 1_000_000.0 / 1});
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
