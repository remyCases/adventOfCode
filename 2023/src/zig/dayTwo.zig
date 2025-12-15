const std = @import("std");

const MAX_RED = 12;
const MAX_GREEN = 13;
const MAX_BLUE = 14;

const Game = struct {
    id: u32,
    green: u32,
    red: u32,
    blue: u32,
};

fn parse(input: []const u8) error{ OutOfMemory, InvalidParsing }!Game {
    var i: usize = 0;
    var g: Game = Game{ .id = 0, .green = 0, .red = 0, .blue = 0 };
    var value: u32 = 0;

    while (i < input.len) {
        switch (input[i]) {
            '0'...'9' => {
                const start: usize = i;
                while (i < input.len and input[i] >= '0' and input[i] <= '9') {
                    i += 1;
                }
                value = std.fmt.parseInt(u32, input[start..i], 10) catch |err| {
                    std.debug.print("Error found {any}\n", .{err});
                    return error.InvalidParsing;
                };
            },
            'r' => {
                while (i < input.len and input[i] >= 'a' and input[i] <= 'z') {
                    i += 1;
                }
                if (g.red < value) g.red = value;
            },
            'g' => {
                while (i < input.len and input[i] >= 'a' and input[i] <= 'z') {
                    i += 1;
                }
                if (g.green < value) g.green = value;
            },
            'b' => {
                while (i < input.len and input[i] >= 'a' and input[i] <= 'z') {
                    i += 1;
                }
                if (g.blue < value) g.blue = value;
            },
            'G' => {
                while (i < input.len and input[i] != ' ') {
                    i += 1;
                }
            },
            ':' => {
                g.id = value;
                i += 1;
            },
            else => {
                i += 1;
            },
        }
    }

    return g;
}

fn compute_id(g: Game) u32 {
    if (g.red <= MAX_RED and g.blue <= MAX_BLUE and g.green <= MAX_GREEN) {
        return g.id;
    }
    return 0;
}

fn compute_power(g: Game) u32 {
    return g.red * g.green * g.blue;
}

pub fn main(part: usize) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var allocating_writer = std.Io.Writer.Allocating.init(allocator);
    defer allocating_writer.deinit();

    var stdout_buf: [1024]u8 = undefined;
    var stdout_writer: std.fs.File.Writer = std.fs.File.stdout().writer(&stdout_buf);

    const file = try std.fs.cwd().openFile(
        "2023/data/input_day_two",
        .{ .mode = .read_only },
    );
    defer file.close();
    var file_buffer: [1024]u8 = undefined;
    var file_reader: std.fs.File.Reader = file.reader(&file_buffer);

    var sum_id: u32 = 0;

    while (file_reader.interface.takeDelimiter('\n')) |line| {
        if (line == null) {
            break;
        }

        const g = try parse(line.?);
        switch (part) {
            1 => sum_id += compute_id(g),
            2 => sum_id += compute_power(g),
            else => {
                return error.InvalidPart;
            },
        }
        allocating_writer.clearRetainingCapacity();
    } else |err| {
        std.debug.print("An error occured: {any}\n", .{err});
    }

    try stdout_writer.interface.print("POWER OF GAMES: {}\n", .{sum_id});
    try stdout_writer.interface.flush();
}
