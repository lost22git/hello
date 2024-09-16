///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const testing = std.testing;
const io = std.io;

test "slice => reader" {
    const s = "hello";

    var fixedBufferStream = io.fixedBufferStream(s);
    var reader = fixedBufferStream.reader();

    try testing.expectEqualDeep('h', reader.readByte());
    try testing.expectEqualDeep('e', reader.readByte());
    try testing.expectEqualDeep('l', reader.readByte());
    try testing.expectEqualDeep('l', reader.readByte());
    try testing.expectEqualDeep('o', reader.readByte());
    try testing.expectError(error.EndOfStream, reader.readByte());
}

test "arraylist => reader" {
    const allocator = testing.allocator;

    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    var writer = list.writer();
    try writer.writeAll("hello");

    var fixedBufferStream = io.fixedBufferStream(list.items);
    var reader = fixedBufferStream.reader();

    try testing.expectEqualDeep('h', reader.readByte());
    try testing.expectEqualDeep('e', reader.readByte());
    try testing.expectEqualDeep('l', reader.readByte());
    try testing.expectEqualDeep('l', reader.readByte());
    try testing.expectEqualDeep('o', reader.readByte());
    try testing.expectError(error.EndOfStream, reader.readByte());
}

test "array => arraylist => writer" {
    var inner_buffer: [22]u8 = undefined;
    var list = std.ArrayListUnmanaged(u8).initBuffer(&inner_buffer);
    // NOTE: 无需 defer list.deinit();

    const count = try list.fixedWriter().write("hello");

    try testing.expectEqual(22, list.capacity);
    try testing.expectEqual(count, list.items.len);
}
