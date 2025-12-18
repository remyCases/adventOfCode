const std = @import("std");
const clap = @import("clap");

pub fn main() !void {
    var stdout_buf: [1024]u8 = undefined;
    var stdout_writer: std.fs.File.Writer = std.fs.File.stdout().writer(&stdout_buf);

    // get general allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // clap elements
    const params = comptime clap.parseParamsComptime(
        \\-d, --day <usize>      An option parameter, which takes a value.
        \\-p, --part <usize>     An option parameter which can be specified multiple times.
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    // resolve parsed arguments
    if (res.args.day) |d| {
        if (res.args.part) |p| {
            switch (d) {
                else => try stdout_writer.interface.print("Incorrect combination of day and part. Day {} and part {} does not exist (yet).\n", .{ d, p }),
            }
        } else {
            return error.NullInput;
        }
    } else {
        return error.NullInput;
    }

    try stdout_writer.interface.flush();
}
