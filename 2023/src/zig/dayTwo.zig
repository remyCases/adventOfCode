const std = @import("std");

fn parse(line: []u8) usize {
    var result: usize = 0;
    for (line) |l| {
        if (l >= '0' and l <= '9') {
            result += (l - '0') * 10;
            break;
        }
    }

    var i: usize = 0;
    while (i < line.len) : (i += 1) {
        const item = line[line.len - i - 1];
        if (item >= '0' and item <= '9') {
            result += (item - '0');
            break;
        }
    }

    return result;
}

pub fn main(part: usize) !void {
    var out = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(
        "2023/data/input_day_two",
        .{},
    );
    defer file.close();

    var calibration_value: usize = 0;

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        switch (part) {
            1 => calibration_value += parse(line),
            2 => calibration_value += parse(line),
            else => {
                return error.InvalidPart;
            },
        }
    }

    try out.print("POWER OF GAMES: {}\n", .{calibration_value});
}
