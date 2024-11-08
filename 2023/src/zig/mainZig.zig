const std = @import("std");
const clap = @import("clap");
const dayOne = @import("dayOne.zig");

pub fn main() !void {
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
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    // resolve parsed arguments
    if (res.args.day) |d| {
        if (res.args.part) |p| {
            switch (d) {
                1 => try dayOne.main(p),
                else => return error.InvalidInput,
            }
        } else {
            return error.NullInput;
        }
    } else {
        return error.NullInput;
    }
}
