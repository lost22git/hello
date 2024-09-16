///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const testing = std.testing;

test "catch" {
    const a: anyerror!u8 = error.MyError;

    // catch and handle error when error
    a catch |e| try testing.expectEqual(error.MyError, e);

    // default value when error
    const b: u8 = a catch 10;
    try testing.expectEqual(b, 10);

    // convert to another error when error
    const c: anyerror!u8 = a catch error.YourError;
    try testing.expectError(error.YourError, c);
}

test "`try`: catch |e| return e" {
    try testing.expectError(error.MyError, try_error_union());
}

fn try_error_union() anyerror!u8 {
    const a: anyerror!u8 = error.MyError;
    _ = try a;

    unreachable;
}

test "error union to optional" {
    const a: anyerror!u8 = error.MyError;
    const b: ?u8 = a catch null;

    try testing.expectEqual(b, null);
}

test "if capture" {
    var a: anyerror!u8 = 0;

    if (a) |v| {
        try testing.expectEqual(v, 0);
    } else |_| {
        try testing.expect(false);
    }

    a = error.MyError;

    if (a) |_| {
        try testing.expect(false);
    } else |e| {
        try testing.expectEqual(e, error.MyError);
    }
}
