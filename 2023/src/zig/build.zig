const std = @import("std");

pub fn build(b: *std.Build) void 
{
	const exe = b.addExecutable(.{
		.name = "mainZig",
		.root_source_file = .{ .path = "mainZig.zig" }
	});

	b.installArtifact(exe);
}
