const std = @import("std");

pub fn main() !void 
{
    var out = std.io.getStdOut().writer();
    try out.print("Hello World !\n", .{});
}
