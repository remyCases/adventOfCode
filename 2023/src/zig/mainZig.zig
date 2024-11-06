const std = @import("std");

pub fn main() !void 
{
    var out = std.io.getStdOut().writer();
    
    // get general allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // parse args into string array
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    for(args) |arg| 
    {
        if (std.mem.eql(u8, arg, "--day"))
        {
            
        }
        else if (std.mem.eql(u8, arg, "--part"))
        {

        }
        try out.print(" {s}\n", .{arg});
    }
}
