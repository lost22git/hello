///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
///
///
/// - *T
/// - *const T
/// - *[N]T
/// - *const [N]T
/// - *[N:E]T
/// - *const [N:E]T
/// - [*]T
/// - [*]const T
/// - []T
/// - []const T
///
const std = @import("std");
const testing = std.testing;

test "const ptr" {
    const a: u8 = 10;
    // const ptr
    const const_ptr = &a;
    try testing.expectEqual(*const u8, @TypeOf(const_ptr));
    try testing.expectEqual(10, const_ptr.*);
}

test "mut ptr" {
    var b: u8 = 10;
    // mut ptr
    const mut_ptr = &b;
    // deref and mut value
    mut_ptr.* = 11;
    try testing.expectEqual(*u8, @TypeOf(mut_ptr));
    try testing.expectEqual(11, b);
}

test "ptr <=> int" {
    var b: u8 = 10;
    const ptr = &b;
    const ptr_2 = &b;
    // @intFromPtr
    // @ptrFromInt
    try testing.expectEqual(@intFromPtr(ptr), @intFromPtr(ptr_2));
    try testing.expectEqual(ptr, @as(*u8, @ptrFromInt(@intFromPtr(ptr_2))));
}

test "const slice" {
    const s = "hello";

    const const_slice: []const u8 = s[1..];

    try testing.expectEqual([]const u8, @TypeOf(const_slice));
    try testing.expectEqual(@as([*]const u8, s) + 1, const_slice.ptr);
    try testing.expectEqual(s.len - 1, const_slice.len);
}

test "mut slice" {
    var s = [_]u8{ 'h', 'e', 'l', 'l', 'o' };

    const mut_slice: []u8 = s[1..];

    try testing.expectEqual([]u8, @TypeOf(mut_slice));
    try testing.expectEqual(@as([*]u8, &s) + 1, mut_slice.ptr);
    try testing.expectEqual(s.len - 1, mut_slice.len);

    mut_slice[0] = 'a';

    try testing.expectEqualStrings("hallo", &s);
}

test "ptr to heap" {
    const allocator = testing.allocator;

    const slice = try allocator.alloc(u8, 10);
    defer allocator.free(slice);

    try testing.expectEqual([]u8, @TypeOf(slice));
    try testing.expectEqual(10, slice.len);

    for (slice) |*v| {
        v.* = 1;
    }

    try testing.expectEqualStrings(&[_]u8{1} ** 10, slice);
}
