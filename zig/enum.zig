///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const testing = std.testing;

test "enum <=> int" {
    const Color = enum(u8) { red = 10, green, blue };

    // @intFromEnum
    try testing.expectEqual(10, @intFromEnum(Color.red));
    try testing.expectEqual(11, @intFromEnum(Color.green));
    try testing.expectEqual(12, @intFromEnum(Color.blue));

    // @enumFromInt
    try testing.expectEqual(Color.red, enumFromInt(Color, 10));
    try testing.expectEqual(Color.green, enumFromInt(Color, 11));
    try testing.expectEqual(Color.blue, enumFromInt(Color, 12));

    // std.meta.intToEnum: safer version of @enumFromInt
    try testing.expectEqual(Color.red, try std.meta.intToEnum(Color, 10));
    try testing.expectEqual(Color.green, try std.meta.intToEnum(Color, 11));
    try testing.expectEqual(Color.blue, try std.meta.intToEnum(Color, 12));
}

fn enumFromInt(comptime T: type, int: anytype) T {
    const e: T = @enumFromInt(int);
    return e;
}

test "enum <=> string" {
    const Color = enum { red, green, blue };

    // @tagName
    try testing.expectEqualStrings("red", @tagName(Color.red));
    try testing.expectEqualStrings("green", @tagName(Color.green));
    try testing.expectEqualStrings("blue", @tagName(Color.blue));

    // @tagName safer version:  std.enums.tagName
    try testing.expectEqualStrings("red", std.enums.tagName(Color, Color.red).?);
    try testing.expectEqualStrings("green", std.enums.tagName(Color, Color.green).?);
    try testing.expectEqualStrings("blue", std.enums.tagName(Color, Color.blue).?);

    // comptime: std.enums.nameCast
    try testing.expectEqual(Color.red, std.enums.nameCast(Color, "red"));
    try testing.expectEqual(Color.green, std.enums.nameCast(Color, "green"));
    try testing.expectEqual(Color.blue, std.enums.nameCast(Color, "blue"));

    // std.meta.stringToEnum: runtime version std.enum.nameCast
    try testing.expectEqual(Color.red, std.meta.stringToEnum(Color, "red").?);
    try testing.expectEqual(Color.green, std.meta.stringToEnum(Color, "green").?);
    try testing.expectEqual(Color.blue, std.meta.stringToEnum(Color, "blue").?);
}

test "enum values" {
    const Color = enum { red, green, blue };

    const colors = std.enums.values(Color);

    try testing.expectEqual(Color.red, colors[0]);
    try testing.expectEqual(Color.green, colors[1]);
    try testing.expectEqual(Color.blue, colors[2]);
}

test "enum indexer" {
    const Color = enum(u8) { red = 10, green = 5, blue = 8 };

    // 根据 value 从小到大 index
    const Indexer = std.enums.EnumIndexer(Color);
    std.enums.ensureIndexer(Indexer);

    try testing.expectEqual(Color, Indexer.Key);
    try testing.expectEqual(3, Indexer.count);

    // indexOf
    try testing.expectEqual(@as(usize, 2), Indexer.indexOf(Color.red));
    try testing.expectEqual(@as(usize, 0), Indexer.indexOf(Color.green));
    try testing.expectEqual(@as(usize, 1), Indexer.indexOf(Color.blue));

    // keyForIndex
    try testing.expectEqual(Color.green, Indexer.keyForIndex(0));
    try testing.expectEqual(Color.blue, Indexer.keyForIndex(1));
    try testing.expectEqual(Color.red, Indexer.keyForIndex(2));
}
