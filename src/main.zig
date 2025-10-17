const std = @import("std");
const hgsl = @import("hgsl");

pub fn main() !void {
    var buf: [128]u8 = undefined;
    var out_writer = std.fs.File.stdout().writer(&buf);
    _ = &out_writer;

    // const path = "test.hgsl";
    // const path = "source.hgsl";
    const path = "func.hgsl";

    const allocator = std.heap.page_allocator;

    // _ = path;
    // const compiled = try hgsl.compile(allocator, hgsl.minimal_frag, "minimal", &out_writer.interface);
    const compiled = try hgsl.compileFile(allocator, path, &out_writer.interface);
    defer allocator.free(compiled);

    //write spirv binary to a file
    const out_file = try std.fs.cwd().openFile("out.spv", .{ .mode = .write_only });
    defer out_file.close();
    var out_buf: [128]u8 = undefined;
    var writer = out_file.writer(&out_buf);

    try writer.interface.writeSliceEndian(u32, compiled, .little);
    try writer.interface.flush();
}
