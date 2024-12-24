///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const testing = std.testing;

test "orelse" {
    var a: ?u8 = 10;

    try testing.expectEqual(a orelse 100, 10);

    a = null;

    try testing.expectEqual(a orelse 100, 100);
}

test "`.?`: orelse unreachable" {
    const a: ?u8 = 10;

    try testing.expectEqual(a.?, 10);
}

test "orelse return null" {
    try testing.expectEqual(orelse_return_null(), null);
}

fn orelse_return_null() ?u8 {
    const a: ?u8 = null;

    _ = a orelse return null;

    unreachable;
}

test "optional to error union" {
    const a: ?u8 = null;

    const b: anyerror!u8 = a orelse error.MyError;

    try testing.expectError(error.MyError, b);
}

test "if capture" {
    var a: ?u8 = 10;

    if (a) |*v| {
        v.* += 1;
    } else {
        try testing.expect(false);
    }

    if (a) |v| {
        try testing.expectEqual(v, 11);
    } else {
        try testing.expect(false);
    }
}
