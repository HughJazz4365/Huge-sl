const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib_mod = b.addModule("hgsl", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/root.zig"),
        .link_libc = true,
        .link_libcpp = true,
    });

    const cli = b.addExecutable(.{
        .name = "main",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "hgsl", .module = lib_mod }},
        }),
    });
    b.installArtifact(cli);

    const run = b.addRunArtifact(cli);
    run.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "run");
    run_step.dependOn(&run.step);
}
