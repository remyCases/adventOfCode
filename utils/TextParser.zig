// from https://devlog.hexops.com/2021/zig-parser-combinators-and-why-theyre-awesome/

const std = @import("std");

pub const Error = error{
    EndOfStream,
    Utf8InvalidStartByte,
} || std.fs.File.ReadError || std.fs.File.SeekError || std.mem.Allocator.Error;

pub fn Parser(comptime Value: type, comptime Reader: type) type {
    return struct {
        const Self = @This();
        _parse: fn (self: *Self, allocator: *Allocator, src: *Reader) callconv(.Inline) Error!?Value,
        pub inline fn parse(self: *Self, allocator: *Allocator, src: *Reader) Error!?Value {
            return self._parse(self, allocator, src);
        }
    };
}

const parser: Parser([]u8, @TypeOf(reader)) = .{
    ._parse = myParse,
};
