const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib_mod = b.addModule("sl", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/root.zig"),
        .link_libc = true,
        .link_libcpp = true,
    });
    lib_mod.addIncludePath(b.dependency("shaderc", .{}).path("libshaderc/include"));
    if (true)
        try @import("buildShaderc.zig").buildShaderc(b, lib_mod, target);

    const exe = b.addExecutable(.{
        .name = "main",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "sl", .module = lib_mod }},
        }),
    });
    exe.root_module.addIncludePath(b.dependency("shaderc", .{}).path("libshaderc/include"));
    b.installArtifact(exe);

    const run = b.addRunArtifact(exe);
    run.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "run");
    run_step.dependOn(&run.step);
}
