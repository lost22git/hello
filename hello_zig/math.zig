///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const testing = std.testing;

test "div / mod" {
    try testing.expectEqual(3.0 / 2.0, 1.5);
    try testing.expectEqual(3 / 2, 1);
    try testing.expectEqual(-3 / -2, 1);
    try testing.expectEqual(-3 / 2, -1);
    try testing.expectEqual(3 / -2, -1);

    // `%` 操作数只能是正数
    try testing.expectEqual(3 % 2, 1);

    // @divTrunc and @rem
    // x = @divTrunc(x,y) * y + @rem(x,y)
    try testing.expectEqual(@divTrunc(3, 2), 1);
    try testing.expectEqual(@divTrunc(-3, -2), 1);
    try testing.expectEqual(@divTrunc(-3, 2), -1);
    try testing.expectEqual(@divTrunc(3, -2), -1);

    try testing.expectEqual(@rem(3, 2), 1);
    try testing.expectEqual(@rem(-3, -2), -1);
    try testing.expectEqual(@rem(-3, 2), -1);
    try testing.expectEqual(@rem(3, -2), 1);

    // @divFloor and @mod
    // x = @divFloor(x,y) * y + @mod(x,y)
    try testing.expectEqual(@divFloor(3, 2), 1);
    try testing.expectEqual(@divFloor(-3, -2), 1);
    try testing.expectEqual(@divFloor(-3, 2), -2);
    try testing.expectEqual(@divFloor(3, -2), -2);

    try testing.expectEqual(@mod(3, 2), 1);
    try testing.expectEqual(@mod(-3, -2), -1);
    try testing.expectEqual(@mod(-3, 2), 1);
    try testing.expectEqual(@mod(3, -2), -1);
}

test "use circular value when overflow" {
    try testing.expectEqual(@as(u8, 255) +% 1, 0);
    try testing.expectEqual(@as(i8, 127) +% 1, -128);
    try testing.expectEqual(@as(i8, -128) -% 1, 127);

    try testing.expectEqual(@addWithOverflow(@as(u8, 254), 1), .{ 255, 0 });
    try testing.expectEqual(@addWithOverflow(@as(u8, 255), 1), .{ 0, 1 });
    try testing.expectEqual(@addWithOverflow(@as(i8, 127), 1), .{ -128, 1 });
    try testing.expectEqual(@subWithOverflow(@as(i8, -128), 1), .{ 127, 1 });
}

test "use limit value when overflow" {
    try testing.expectEqual(@as(u8, 255) +| 1, 255);
    try testing.expectEqual(@as(i8, 127) +| 1, 127);
    try testing.expectEqual(@as(i8, -128) -| 1, -128);
}
