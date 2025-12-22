const std = @import("std");

fn parseDigits(line: []u8) u64 {
    var result: u64 = 0;
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

fn parseStringAsDigit(x: []u8) u64 {
    if (std.mem.eql(u8, x, "1") or std.mem.eql(u8, x, "one")) {
        return 1;
    } else if (std.mem.eql(u8, x, "2") or std.mem.eql(u8, x, "two")) {
        return 2;
    } else if (std.mem.eql(u8, x, "3") or std.mem.eql(u8, x, "three")) {
        return 3;
    } else if (std.mem.eql(u8, x, "4") or std.mem.eql(u8, x, "four")) {
        return 4;
    } else if (std.mem.eql(u8, x, "5") or std.mem.eql(u8, x, "five")) {
        return 5;
    } else if (std.mem.eql(u8, x, "6") or std.mem.eql(u8, x, "six")) {
        return 6;
    } else if (std.mem.eql(u8, x, "7") or std.mem.eql(u8, x, "seven")) {
        return 7;
    } else if (std.mem.eql(u8, x, "8") or std.mem.eql(u8, x, "eight")) {
        return 8;
    } else if (std.mem.eql(u8, x, "9") or std.mem.eql(u8, x, "nine")) {
        return 9;
    }

    return 0;
}

fn parseStringAsDigits(line: []u8) u64 {
    var result: u64 = 0;
    var low: usize = 0;
    var high: usize = low;
    const len: usize = line.len;
    var digit: u64 = 0;

    while (low < len) {
        digit = parseStringAsDigit(line[low..(high + 1)]);
        if (digit > 0) {
            result += digit * 10;
            break;
        }

        high = high + 1;
        if (high == len or high - low > 6) { // there is no element bigger than a six letters string
            low = low + 1;
            high = low;
        }
    }

    low = len;
    high = low;
    while (high >= 0) {
        digit = parseStringAsDigit(line[low..(high + 1)]);
        if (digit > 0) {
            result += digit;
            break;
        }

        if (low == 0 or high - low > 6) {
            high = high - 1;
            low = high;
        } else {
            low = low - 1;
        }
    }

    return result;
}

pub fn main(part: u64) !void {
    var stdout_buf: [1024]u8 = undefined;
    var stdout_writer: std.fs.File.Writer = std.fs.File.stdout().writer(&stdout_buf);

    const file = try std.fs.cwd().openFile(
        "2023/data/input_day_one",
        .{ .mode = .read_only },
    );
    defer file.close();
    var file_buffer: [1024]u8 = undefined;
    var file_reader: std.fs.File.Reader = file.reader(&file_buffer);

    var calibration_value: u64 = 0;

    while (file_reader.interface.takeDelimiter('\n')) |line| {
        if (line == null) {
            break;
        }

        switch (part) {
            1 => calibration_value += parseDigits(line.?),
            2 => calibration_value += parseStringAsDigits(line.?),
            else => {
                return error.InvalidPart;
            },
        }
    } else |err| {
        std.debug.print("An error occured: {any}\n", .{err});
    }

    try stdout_writer.interface.print("CALIBRATION VALUE: {}\n", .{calibration_value});
    try stdout_writer.interface.flush();
}
