///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const testing = std.testing;

test "primitive type" {
    try testing.expectEqual(@sizeOf(bool), 1);
    try testing.expectEqual(@alignOf(bool), 1);

    try testing.expectEqual(@sizeOf(u8), 1);
    try testing.expectEqual(@alignOf(u8), 1);

    try testing.expectEqual(@sizeOf(u16), 2);
    try testing.expectEqual(@alignOf(u16), 2);

    try testing.expectEqual(@sizeOf(u32), 4);
    try testing.expectEqual(@alignOf(u32), 4);

    try testing.expectEqual(@sizeOf(u64), 8);
    try testing.expectEqual(@alignOf(u64), 8);

    try testing.expectEqual(@sizeOf(u128), 16);
    try testing.expectEqual(@alignOf(u128), 8);

    try testing.expectEqual(@sizeOf(usize), 8);
    try testing.expectEqual(@alignOf(usize), 8);

    try testing.expectEqual(@sizeOf(u7), 1);
    try testing.expectEqual(@alignOf(u7), 1);

    try testing.expectEqual(@sizeOf(u24), 4);
    try testing.expectEqual(@alignOf(u24), 4);

    try testing.expectEqual(@sizeOf(f32), 4);
    try testing.expectEqual(@alignOf(f32), 4);

    try testing.expectEqual(@sizeOf(f64), 8);
    try testing.expectEqual(@alignOf(f64), 8);

    try testing.expectEqual(@sizeOf(f80), 16);
    try testing.expectEqual(@alignOf(f80), 16);

    try testing.expectEqual(@sizeOf(f128), 16);
    try testing.expectEqual(@alignOf(f128), 16);
}

test "zero bit type" {
    try testing.expectEqual(@sizeOf(u0), 0);
    try testing.expectEqual(@alignOf(u0), 1);

    try testing.expectEqual(@sizeOf(void), 0);
    try testing.expectEqual(@alignOf(void), 1);

    // zero element array
    try testing.expectEqual(@sizeOf([0]u8), 0);
    try testing.expectEqual(@alignOf([0]u8), 1);

    // 1 tag enum
    try testing.expectEqual(@sizeOf(enum { a }), 0);
    try testing.expectEqual(@alignOf(enum { a }), 1);

    // union with only 1 field which is a `zero bit type`
    try testing.expectEqual(@sizeOf(union { a: void }), 0);
    try testing.expectEqual(@alignOf(union { a: void }), 1);

    // struct with all fields which are `zero bit type`
    try testing.expectEqual(@sizeOf(struct { a: void, b: u0 }), 0);
    try testing.expectEqual(@alignOf(struct { a: void, b: u0 }), 1);
}

test "array" {
    try testing.expectEqual(@sizeOf([4]u32), 4 * 4);
    try testing.expectEqual(@alignOf([4]u32), 4);

    try testing.expectEqual(@sizeOf([4:0]u32), 4 * 5);
    try testing.expectEqual(@alignOf([4:0]u32), 4);
}

test "slice" {
    try testing.expectEqual(@sizeOf([]const u32), 8 + 8); // ptr + len
    try testing.expectEqual(@alignOf([]const u32), 8);

    try testing.expectEqual(@sizeOf([]u32), 8 + 8); // ptr + len
    try testing.expectEqual(@alignOf([]u32), 8);
}

test "pointer" {
    // single-item pointer
    try testing.expectEqual(@sizeOf(*const u8), 8);
    try testing.expectEqual(@alignOf(*const u8), 8);

    try testing.expectEqual(@sizeOf(*u8), 8);
    try testing.expectEqual(@alignOf(*u8), 8);

    try testing.expectEqual(@sizeOf(*const [3]u8), 8);
    try testing.expectEqual(@alignOf(*const [3]u8), 8);

    try testing.expectEqual(@sizeOf(*[3]u8), 8);
    try testing.expectEqual(@alignOf(*[3]u8), 8);

    try testing.expectEqual(@sizeOf(*const [3:0]u8), 8);
    try testing.expectEqual(@alignOf(*const [3:0]u8), 8);

    try testing.expectEqual(@sizeOf(*[3:0]u8), 8);
    try testing.expectEqual(@alignOf(*[3:0]u8), 8);

    // multi-items pointer
    try testing.expectEqual(@sizeOf([*]const u8), 8);
    try testing.expectEqual(@alignOf([*]const u8), 8);

    try testing.expectEqual(@sizeOf([*]u8), 8);
    try testing.expectEqual(@alignOf([*]u8), 8);
}

test "struct" {
    const Book = struct {
        id: u32,
        name: []const u8,
        balance: u32,
    };

    try testing.expectEqual(@sizeOf(Book), 24);
    try testing.expectEqual(@alignOf(Book), 8);
    try testing.expectEqual(@offsetOf(Book, "id"), 16);
    try testing.expectEqual(@offsetOf(Book, "name"), 0);
    try testing.expectEqual(@offsetOf(Book, "balance"), 20);
}

test "extern struct" {
    // FIX:  error: extern structs cannot contain fields of type '[]const u8'

    // const Book = extern struct {
    //     id: u32,
    //     name: []const u8,
    //     balance: u32,
    // };
    //
    // try testing.expectEqual(@sizeOf(Book), 24);
    // try testing.expectEqual(@alignOf(Book), 8);
    // try testing.expectEqual(@offsetOf(Book, "id"), 0);
    // try testing.expectEqual(@offsetOf(Book, "name"), 4);
    // try testing.expectEqual(@offsetOf(Book, "balance"), 20);
}

// https://ziglang.org/documentation/master/#packed-struct
test "packed struct" {
    // FIX: error: packed structs cannot contain fields of type '[]const u8'

    // const Book = packed struct {
    //     id: u32,
    //     name: []const u8,
    //     balance: u32,
    // };
    //
    // try testing.expectEqual(@sizeOf(Book), 24);
    // try testing.expectEqual(@alignOf(Book), 8);
    // try testing.expectEqual(@offsetOf(Book, "id"), 0);
    // try testing.expectEqual(@offsetOf(Book, "name"), 4);
    // try testing.expectEqual(@offsetOf(Book, "balance"), 20);
}

test "tuple" {
    const t: struct { u32, []const u8, f64 } = .{ 1, "你好", 1.0 };
    try testing.expectEqual(@sizeOf(@TypeOf(t)), 32);
    try testing.expectEqual(@alignOf(@TypeOf(t)), 8);
    try testing.expectEqual(@offsetOf(@TypeOf(t), "0"), 0);
    try testing.expectEqual(@offsetOf(@TypeOf(t), "1"), 8);
    try testing.expectEqual(@offsetOf(@TypeOf(t), "2"), 24);
}

test "enum" {
    const Color = enum {
        red,
        green,
        blue,
    };

    try testing.expectEqual(@sizeOf(Color), 1);
    try testing.expectEqual(@alignOf(Color), 1);

    const ColorV2 = enum(u32) {
        red,
        green,
        blue,
    };

    try testing.expectEqual(@sizeOf(ColorV2), 4);
    try testing.expectEqual(@alignOf(ColorV2), 4);
}

test "union" {

    // multiple fields: with tag
    const U = union(enum) { a: u32, b: u8 };
    try testing.expectEqual(@sizeOf(U), (1 + 3) + 4);
    try testing.expectEqual(@alignOf(U), 4);

    // multiple fields: with tag
    const U2 = union(enum) { a: u32, b: void };
    try testing.expectEqual(@sizeOf(U2), (1 + 3) + 4);
    try testing.expectEqual(@alignOf(U2), 4);

    // FIX: compile error ???
    // const U3 = union { a: void, b: void };
    // try testing.expectEqual(@sizeOf(U3), 1);
    // try testing.expectEqual(@alignOf(U3), 1);

    // single field: without tag
    const U4 = union(enum) { a: void };
    try testing.expectEqual(@sizeOf(U4), 0);
    try testing.expectEqual(@alignOf(U4), 1);

    // single field: without tag
    const U5 = union(enum) { a: u32 };
    try testing.expectEqual(@sizeOf(U5), 4);
    try testing.expectEqual(@alignOf(U5), 4);
}

test "extern union" {
    const U = extern union { a: u32, b: u8 };
    try testing.expectEqual(@sizeOf(U), 4);
    try testing.expectEqual(@alignOf(U), 4);

    const U2 = extern union { a: u32, b: void };
    try testing.expectEqual(@sizeOf(U2), 4);
    try testing.expectEqual(@alignOf(U2), 4);

    const U4 = extern union { a: void };
    try testing.expectEqual(@sizeOf(U4), 0);
    try testing.expectEqual(@alignOf(U4), 1);

    const U5 = extern union { a: u32 };
    try testing.expectEqual(@sizeOf(U5), 4);
    try testing.expectEqual(@alignOf(U5), 4);
}

test "packed union" {
    const U = packed union { a: u32, b: u8 };
    try testing.expectEqual(@sizeOf(U), 4);
    try testing.expectEqual(@alignOf(U), 4);

    const U2 = packed union { a: u32, b: void };
    try testing.expectEqual(@sizeOf(U2), 4);
    try testing.expectEqual(@alignOf(U2), 4);

    const U4 = packed union { a: void };
    try testing.expectEqual(@sizeOf(U4), 0);
    try testing.expectEqual(@alignOf(U4), 1);

    const U5 = packed union { a: u32 };
    try testing.expectEqual(@sizeOf(U5), 4);
    try testing.expectEqual(@alignOf(U5), 4);
}

test "error" {
    // error 是一个 u16 值

    try testing.expectEqual(@sizeOf(anyerror), 2);
    try testing.expectEqual(@alignOf(anyerror), 2);

    const FileOpenError = error{
        AccessDenied,
        OutOfMemory,
        FileNotFound,
    };
    try testing.expectEqual(@sizeOf(FileOpenError), 2);
    try testing.expectEqual(@alignOf(FileOpenError), 2);

    const SingleError = error{MyError};
    try testing.expectEqual(@sizeOf(SingleError), 2);
    try testing.expectEqual(@alignOf(SingleError), 2);

    const EmptyError = error{};
    try testing.expectEqual(@sizeOf(EmptyError), 2);
    // FIX: error: no align available for type 'error{}'
    // try testing.expectEqual(@alignOf(EmptyError), 2);
}

test "error union" {
    const FileOpenError = error{
        AccessDenied,
        OutOfMemory,
        FileNotFound,
    };

    try testing.expectEqual(@sizeOf(FileOpenError!u8), (1 + 1) + 2);
    try testing.expectEqual(@alignOf(FileOpenError!u8), 2);

    try testing.expectEqual(@sizeOf(FileOpenError!u32), (1 + 3) + 4);
    try testing.expectEqual(@alignOf(FileOpenError!u32), 4);

    // NOTE: without tag, 0 表示 ok
    try testing.expectEqual(@sizeOf(FileOpenError!u0), 2);
    try testing.expectEqual(@alignOf(FileOpenError!u0), 2);

    // NOTE: without tag? 0 表示 ok
    try testing.expectEqual(@sizeOf(FileOpenError!void), 2);
    try testing.expectEqual(@alignOf(FileOpenError!void), 2);

    // compare with union
    const MyErrorUnion = union { err: FileOpenError, ok: void };
    try testing.expectEqual(@sizeOf(MyErrorUnion), (1 + 1) + 2);
    try testing.expectEqual(@alignOf(MyErrorUnion), 2);
}

test "optional" {
    try testing.expectEqual(@sizeOf(?u8), 1 + 1);
    try testing.expectEqual(@alignOf(?u8), 1);

    try testing.expectEqual(@sizeOf(?u32), (1 + 3) + 4);
    try testing.expectEqual(@alignOf(?u32), 4);

    // NOTE: without tag, 0 来表示 none
    try testing.expectEqual(@sizeOf(?*u8), 8);
    try testing.expectEqual(@alignOf(?*u8), 8);

    // NOTE: without tag, 0 来表示 none
    try testing.expectEqual(@sizeOf(?error{MyError}), 2);
    try testing.expectEqual(@alignOf(?error{MyError}), 2);

    // compare with union
    const MyOptionalUnion = union(enum) { some: *u8, none: void };
    try testing.expectEqual(@sizeOf(MyOptionalUnion), (1 + 7) + 8);
    try testing.expectEqual(@alignOf(MyOptionalUnion), 8);
}

test "allocator" {
    try testing.expectEqual(16, @sizeOf(std.mem.Allocator));
    try testing.expectEqual(8, @alignOf(std.mem.Allocator));

    try testing.expectEqual(0, @offsetOf(std.mem.Allocator, "ptr")); //  *anyopaque,
    try testing.expectEqual(8, @offsetOf(std.mem.Allocator, "vtable")); // *const VTable

    try testing.expectEqual(24, @sizeOf(std.mem.Allocator.VTable));
    try testing.expectEqual(8, @alignOf(std.mem.Allocator.VTable));
    try testing.expectEqual(0, @offsetOf(std.mem.Allocator.VTable, "alloc")); // *const fn
    try testing.expectEqual(8, @offsetOf(std.mem.Allocator.VTable, "resize")); // *const fn
    try testing.expectEqual(16, @offsetOf(std.mem.Allocator.VTable, "free")); // *const fn
}

test "arraylist" {
    // ArrayList
    try testing.expectEqual(40, @sizeOf(std.ArrayList(u8)));
    try testing.expectEqual(8, @alignOf(std.ArrayList(u8)));
    try testing.expectEqual(0, @offsetOf(std.ArrayList(u8), "items")); // Slice
    try testing.expectEqual(16, @offsetOf(std.ArrayList(u8), "capacity")); // usize
    try testing.expectEqual(24, @offsetOf(std.ArrayList(u8), "allocator")); // Allocator

    // ArrayListUnmanaged
    try testing.expectEqual(24, @sizeOf(std.ArrayListUnmanaged(u8)));
    try testing.expectEqual(8, @alignOf(std.ArrayListUnmanaged(u8)));
    try testing.expectEqual(0, @offsetOf(std.ArrayListUnmanaged(u8), "items")); // Slice
    try testing.expectEqual(16, @offsetOf(std.ArrayListUnmanaged(u8), "capacity")); // usize
}

test "hashmap" {
    //
}
