const std = @import("std");
const lib_dir = "lib";

pub fn buildShaderc(b: *std.Build, module: *std.Build.Module, target: std.Build.ResolvedTarget) !void {
    module.addLibraryPath(b.path(lib_dir));

    const compile_shaderc = b.option(bool, "shaderc", "force compile shaderc") orelse false;
    // !(try checkLib(b, "shaderc"));
    if (compile_shaderc) {
        std.debug.print("compiling shaderc...(may take a long time)\n", .{});
        const shaderc_lib = try createShadercLib(b, target, .ReleaseSafe);
        installLib(b, shaderc_lib);
        module.linkLibrary(shaderc_lib);
    } else {
        module.linkSystemLibrary("shaderc", .{ .preferred_link_mode = .static });
    }
}
fn makeDir(path: []const u8) !void {
    std.fs.makeDirAbsolute(path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}
fn installLib(b: *std.Build, lib: *std.Build.Step.Compile) void {
    const install_lib = b.addInstallArtifact(
        lib,
        .{ .dest_dir = .{ .override = .{ .custom = "../" ++ lib_dir } } },
    );
    install_lib.step.dependOn(&lib.step);
    b.getInstallStep().dependOn(&install_lib.step);
}
fn checkLib(b: *std.Build, name: []const u8) !bool {
    return try checkLibInDir(b, b.path(lib_dir).getPath(b), name);
}
fn checkLibInDir(b: *std.Build, path: []const u8, name: []const u8) !bool {
    const prefixed = concat(b, "lib", name);
    var dir = try std.fs.openDirAbsolute(
        path,
        .{ .iterate = true },
    );
    defer dir.close();
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        switch (entry.kind) {
            .file => {
                const entry_name = entry.name[0..(std.mem.lastIndexOfScalar(u8, entry.name, '.') orelse continue)];
                if (strIn(entry_name, &.{ name, prefixed })) return true;
            },
            .directory => {
                if (try checkLibInDir(b, b.pathJoin(&.{ path, entry.name }), name)) return true;
            },
            else => continue,
        }
    }
    return false;
}
fn createShadercLib(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) !*std.Build.Step.Compile {
    const shaderc_lib = b.addLibrary(.{
        .name = "shaderc",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libcpp = true,
            .link_libc = true,
            .sanitize_c = .off,
        }),
    });

    //NOTE:|=======Link Shaderc========|
    const shaderc_dep = b.dependency("shaderc", .{});
    shaderc_lib.addIncludePath(shaderc_dep.path("libshaderc/include"));
    shaderc_lib.addIncludePath(shaderc_dep.path("libshaderc_util/include"));
    try linkDirContentsCpp(
        b,
        shaderc_lib,
        shaderc_dep.path("").getPath(b),
        &.{ "libshaderc", "libshaderc_util", "src" },
        &.{"cc"},
        &.{ "-std=c++17", "-DENABLE_HLSL" },
        &.{},
    );
    //NOTE:|=======Link Glslang========|
    const glslang_dep = b.dependency("glslang", .{});
    shaderc_lib.addIncludePath(glslang_dep.path(""));
    shaderc_lib.addIncludePath(glslang_dep.path("SPIRV"));
    try linkDirContentsCpp(
        b,
        shaderc_lib,
        glslang_dep.path("").getPath(b),
        &.{ "SPIRV", "glslang", "MachineIndependent", "preprocessor", "HLSL", "GenericCodeGen" },
        &.{"cpp"},
        &.{ "-std=c++17", "-DENABLE_HLSL" },
        &.{},
    );
    //NOTE:|=====Link Spirv-Tools=======|
    const spirv_tools_dep = b.dependency("spirv-tools", .{});
    shaderc_lib.addIncludePath(spirv_tools_dep.path("include"));
    shaderc_lib.addIncludePath(spirv_tools_dep.path(""));
    try linkDirContentsCpp(
        b,
        shaderc_lib,
        spirv_tools_dep.path("source").getPath(b),
        &.{ "opt", "val", "util" },
        &.{"cpp"},
        &.{"-std=c++17"},
        &.{"mimalloc"},
    );
    const spirv_headers_dep = b.dependency("spirv-headers", .{});
    shaderc_lib.addIncludePath(spirv_headers_dep.path("include"));

    //NOTE: create buildtime generated headers
    const buildtime_path = shaderc_dep.path("buildtime").getPath(b);
    try makeDir(buildtime_path);
    const buildtime_glslang_path = b.pathJoin(&.{ buildtime_path, "glslang" });
    try makeDir(buildtime_glslang_path);
    shaderc_lib.addIncludePath(.{ .cwd_relative = buildtime_path });

    const unified1 = "include/spirv/unified1/";
    // shaderc_lib.addIncludePath(spirv_headers_dep.path(unified1));
    //NOTE:(1) generate glslang/build_info.h
    const pass_1 = b.addSystemCommand(&.{
        "python",
        glslang_dep.path("build_info.py").getPath(b),
        glslang_dep.path("").getPath(b),
        "-i",
        glslang_dep.path("build_info.h.tmpl").getPath(b),
        "-o",
        b.pathJoin(&.{ buildtime_glslang_path, "build_info.h" }),
    });
    shaderc_lib.step.dependOn(&pass_1.step);

    //NOTE:(2) generate debug info headers
    const debug_info_grammar_names: []const []const u8 = &.{
        "extinst.debuginfo.grammar.json",
        "extinst.opencl.debuginfo.100.grammar.json",
        "extinst.nonsemantic.shader.debuginfo.100.grammar.json",
    };
    inline for (&[_][2][]const u8{
        .{ "DebugInfo", debug_info_grammar_names[0] },
        .{ "OpenCLDebugInfo100", debug_info_grammar_names[1] },
        .{ "NonSemanticShaderDebugInfo100", debug_info_grammar_names[2] },
    }) |grammar| {
        const pass_2 = b.addSystemCommand(&.{
            "python",
            spirv_tools_dep.path("utils/generate_language_headers.py").getPath(b),
            concat(b, "--extinst-grammar=", spirv_headers_dep.path(unified1 ++ grammar[1]).getPath(b)),
            concat(b, "--extinst-output-path=", b.pathJoin(&.{ buildtime_path, grammar[0] ++ ".h" })),
        });
        shaderc_lib.step.dependOn(&pass_2.step);
    }

    //NOTE:(3) generate core_tables_header.inc, core_tables_body.inc
    const pass_3 = b.addSystemCommand(&.{
        "python",
        spirv_tools_dep.path("utils/ggt.py").getPath(b),
        concat(b, "--core-tables-body-output=", b.pathJoin(&.{ buildtime_path, "core_tables_body.inc" })),
        concat(b, "--core-tables-header-output=", b.pathJoin(&.{ buildtime_path, "core_tables_header.inc" })),
        concat(b, "--spirv-core-grammar=", spirv_headers_dep.path(unified1 ++ "spirv.core.grammar.json").getPath(b)),
        //
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ "extinst.glsl.std.450.grammar.json").getPath(b)),
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ "extinst.opencl.std.100.grammar.json").getPath(b)),
        concat(b, "--extinst=CLDEBUG100_,", spirv_headers_dep.path(unified1 ++ debug_info_grammar_names[1]).getPath(b)),
        concat(b, "--extinst=SHDEBUG100_,", spirv_headers_dep.path(unified1 ++ debug_info_grammar_names[2]).getPath(b)),
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ "extinst.spv-amd-shader-explicit-vertex-parameter.grammar.json").getPath(b)),
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ "extinst.spv-amd-shader-trinary-minmax.grammar.json").getPath(b)),
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ "extinst.spv-amd-gcn-shader.grammar.json").getPath(b)),
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ "extinst.spv-amd-shader-ballot.grammar.json").getPath(b)),
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ debug_info_grammar_names[0]).getPath(b)),
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ "extinst.nonsemantic.clspvreflection.grammar.json").getPath(b)),
        concat(b, "--extinst=,", spirv_headers_dep.path(unified1 ++ "extinst.nonsemantic.vkspreflection.grammar.json").getPath(b)),
        concat(b, "--extinst=TOSA_,", spirv_headers_dep.path(unified1 ++ "extinst.tosa.001000.1.grammar.json").getPath(b)),
    });
    shaderc_lib.step.dependOn(&pass_3.step);

    //NOTE:(4) generate build-version.inc
    const pass_4 = b.addSystemCommand(&.{
        "python",
        spirv_tools_dep.path("utils/update_build_version.py").getPath(b),
        spirv_tools_dep.path("CHANGES").getPath(b),
        b.pathJoin(&.{ buildtime_path, "build-version.inc" }),
    });
    shaderc_lib.step.dependOn(&pass_4.step);

    //NOTE:(5) generate generators.inc
    const pass_5 = b.addSystemCommand(&.{
        "python",
        spirv_tools_dep.path("utils/generate_registry_tables.py").getPath(b),
        concat(b, "--xml=", spirv_headers_dep.path("include/spirv/spir-v.xml").getPath(b)),
        concat(b, "--generator-output=", b.pathJoin(&.{ buildtime_path, "generators.inc" })),
    });
    shaderc_lib.step.dependOn(&pass_5.step);
    return shaderc_lib;
}
fn linkDirContentsCpp(
    b: *std.Build,
    target: *std.Build.Step.Compile,
    path: []const u8,
    subdirs: []const []const u8,
    extensions: []const []const u8,
    flags: []const []const u8,
    file_blacklist: []const []const u8,
) !void {
    var dir = try std.fs.openDirAbsolute(path, .{ .iterate = true });
    defer dir.close();
    // std.debug.print("opeded dir: {s}\n", .{path});
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        switch (entry.kind) {
            .file => {
                const file_path = b.pathJoin(&.{ path, entry.name });

                const dot_index = (std.mem.lastIndexOfScalar(u8, entry.name, '.') orelse continue) + 1;
                //test file
                if (dot_index > 4 and strEql(entry.name[dot_index - 5 .. dot_index - 1], "test")) continue;

                const file_extension = entry.name[dot_index..];

                if (!strIn(file_extension, extensions)) continue;
                if (strIn(entry.name[0..dot_index -| 1], file_blacklist)) continue;
                // std.debug.print("accepted file: {s}\n", .{file_path});
                target.addCSourceFile(.{
                    .file = .{ .cwd_relative = file_path },
                    .flags = flags,
                    .language = .cpp,
                });
            },
            .directory => {
                if (!strIn(entry.name, subdirs)) continue;
                const next_path = b.pathJoin(&.{ path, entry.name });
                try linkDirContentsCpp(
                    b,
                    target,
                    next_path,
                    subdirs,
                    extensions,
                    flags,
                    file_blacklist,
                );
            },
            else => continue,
        }
    }
}

fn concat(b: *std.Build, lhs: []const u8, rhs: []const u8) []u8 {
    return std.mem.concat(b.allocator, u8, &.{ lhs, rhs }) catch unreachable;
}
fn strIn(a: []const u8, s: []const []const u8) bool {
    for (s) |mem| {
        if (strEql(mem, a)) return true;
    }
    return false;
}
fn strEql(a: []const u8, b: []const u8) bool {
    return if (a.len == b.len) std.mem.eql(u8, a, b) else false;
}
