const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "mainZig",
        .root_source_file = b.path("mainZig.zig"),
        .target = target,
        .optimize = optimize,
    });

    const clap = b.dependency("clap", .{
        .target = target,
        .optimize = optimize,
    });

    exe.root_module.addImport("clap", clap.module("clap"));

    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
