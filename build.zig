const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const querried_optimize = b.standardOptimizeOption(.{});
    // const optimize: std.builtin.OptimizeMode = if (querried_optimize == .Debug) .Debug else .ReleaseSmall;
    const optimize = querried_optimize;

    const lib_mod = b.addModule("hgsl", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/root.zig"),
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

    // const lsp = b.addExecutable(.{
    //     .name = "lsp",
    //     .root_module = b.createModule(.{
    //         .root_source_file = b.path("lsp/lspmain.zig"),
    //         .target = target,
    //         .optimize = optimize,
    //         .imports = &.{.{ .name = "hgsl", .module = lib_mod }},
    //     }),
    // });
    // b.installArtifact(lsp);

    const run = b.addRunArtifact(cli);
    run.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "run");
    run_step.dependOn(&run.step);
}
