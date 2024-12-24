///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const testing = std.testing;
const json = std.json;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Store = struct {
    id: u64,
    name: []const u8,
};

const Book = struct {
    const Self = @This();
    id: u64,
    name: []const u8,
    price: f64,
    tags: ArrayList([]const u8),
    store: ?Store,

    pub fn init(alloc: Allocator, id: u64, name: []const u8, price: f64, store: ?Store) Self {
        return .{
            .id = id,
            .name = name,
            .price = price,
            .tags = ArrayList([]const u8).init(alloc),
            .store = store,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tags.deinit();
        self.* = undefined;
    }

    pub fn clone(self: *const Self) !Self {
        return .{
            .id = self.id,
            .name = self.name,
            .price = self.price,
            .tags = try self.tags.clone(),
            .store = self.store,
        };
    }

    pub fn addTag(self: *Self, tag: []const u8) !void {
        try self.tags.append(tag);
    }

    /// custom json serialization
    pub fn jsonStringify(value: *const Self, jws: anytype) !void {
        try jws.beginObject();

        inline for (std.meta.fields(Self)) |fieldInfo| {
            const fname = fieldInfo.name;
            const ftype = fieldInfo.type;
            switch (@typeInfo(ftype)) {
                .Optional => {
                    if (@field(value, fname)) |v| {
                        try jws.objectField(fname);
                        try jws.write(v);
                    }
                },
                .Struct => {
                    // TODO: find a better way to get `ArrayList` from `ArrayList([]const u8)`
                    if (comptime std.mem.indexOf(u8, @typeName(ftype), "ArrayList")) |_| {
                        try jws.objectField(fname);
                        try jws.write(@field(value, fname).items);
                    } else {
                        try jws.objectField(fname);
                        try jws.write(@field(value, fname));
                    }
                },
                else => {
                    try jws.objectField(fname);
                    try jws.write(@field(value, fname));
                },
            }
        }

        try jws.endObject();

        // try jws.beginObject();
        // try jws.objectField("id");
        // try jws.write(value.id);
        // try jws.objectField("name");
        // try jws.write(value.name);
        // if (value.price) |price| {
        //     try jws.objectField("price");
        //     try jws.write(price);
        // }
        // try jws.objectField("tags");
        // try jws.write(value.tags.items);
        // try jws.endObject();
    }

    /// custom json deserialization
    pub fn jsonParseFromValue(alloc: Allocator, source: json.Value, options: json.ParseOptions) !Self {
        if (source != .object) return error.UnexpectedToken;

        var result = Self.init(alloc, 0, "", 0, null);

        var it = source.object.iterator();
        while (it.next()) |kv| {
            inline for (std.meta.fields(Self)) |fieldInfo| {
                const fname = fieldInfo.name;
                const ftype = fieldInfo.type;
                if (std.mem.eql(u8, fname, kv.key_ptr.*)) {
                    // TODO: find a better way to get `ArrayList` from `ArrayList([]const u8)`
                    if (comptime std.mem.indexOf(u8, @typeName(ftype), "ArrayList")) |_| {
                        const array_list_items_type = std.meta.fieldInfo(ftype, .items).type;
                        const array_list_items_child_type = @typeInfo(array_list_items_type).Pointer.child;
                        @field(result, fname).deinit();
                        const slice = try json.innerParseFromValue(array_list_items_type, alloc, kv.value_ptr.*, options);
                        @field(result, fname) = ArrayList(array_list_items_child_type).fromOwnedSlice(alloc, slice);
                    } else {
                        @field(result, fname) = try json.innerParseFromValue(ftype, alloc, kv.value_ptr.*, options);
                    }
                }
            }

            // if (std.meta.stringToEnum(std.meta.FieldEnum(Self), kv.key_ptr.*)) |k| {
            //     switch (k) {
            //         .id => {
            //             result.id = try json.innerParseFromValue(std.meta.fieldInfo(Self, .id).type, alloc, kv.value_ptr.*, options);
            //         },
            //         .name => {
            //             result.name = try json.innerParseFromValue(std.meta.fieldInfo(Self, .name).type, alloc, kv.value_ptr.*, options);
            //         },
            //         .price => {
            //             result.price = try json.innerParseFromValue(std.meta.fieldInfo(Self, .price).type, alloc, kv.value_ptr.*, options);
            //         },
            //         .tags => {
            //             result.tags.deinit(); // deinit old tags
            //             const slice = try json.innerParseFromValue([][]const u8, alloc, kv.value_ptr.*, options);
            //             result.tags = ArrayList([]const u8).fromOwnedSlice(alloc, slice);
            //         },
            //     }
            // }
        }

        // TODO: validate

        return result;
    }

    pub fn toJson(self: *const Self, alloc: Allocator) ![]u8 {
        return try json.stringifyAlloc(alloc, self, .{});
    }

    pub fn fromJsonLeaky(arena: *ArenaAllocator, json_string: []const u8) !Self {
        const alloc = arena.allocator();
        const parsedValue = try json.parseFromSliceLeaky(json.Value, alloc, json_string, .{});
        return try json.parseFromValueLeaky(Self, alloc, parsedValue, .{});
    }

    pub fn fromJson(alloc: Allocator, json_string: []const u8) !json.Parsed(Self) {
        const parsedValue = try json.parseFromSlice(json.Value, alloc, json_string, .{});
        defer parsedValue.deinit();
        return try json.parseFromValue(Self, alloc, parsedValue.value, .{});
    }
};

test "struct json (de)ser" {
    const allocator = testing.allocator;

    var book = Book.init(allocator, 10, "史记", 66.6, .{ .id = 1, .name = "新华书店" });
    defer book.deinit();

    try book.addTag("历史");
    try book.addTag("中国");

    std.debug.print("book: {}\n", .{json.fmt(&book, .{})});
    const json_string = try book.toJson(allocator);
    defer allocator.free(json_string);

    // var arena = ArenaAllocator.init(allocator);
    // defer arena.deinit();
    // var parsedBook = try Book.fromJsonLeaky(&arena, json_string);
    // defer parsedBook.deinit();

    var parsedBookParsed = try Book.fromJson(allocator, json_string);
    defer parsedBookParsed.deinit();
    var parsedBook = &parsedBookParsed.value;

    try parsedBook.addTag("畅销");

    try testing.expectEqual(book.id, parsedBook.id);
    try testing.expectEqualStrings(book.name, parsedBook.name);
    try testing.expectEqual(book.price, parsedBook.price);
    try testing.expect(book.tags.items.ptr != parsedBook.tags.items.ptr);
    try testing.expect(book.tags.items.len + 1 == parsedBook.tags.items.len);
    try testing.expectEqualStrings(book.tags.items[0], parsedBook.tags.items[0]);
    try testing.expectEqualStrings(book.tags.items[1], parsedBook.tags.items[1]);
    try testing.expectEqual(book.store.?.id, parsedBook.store.?.id);
    try testing.expectEqualStrings(book.store.?.name, parsedBook.store.?.name);
}

test "book clone" {
    const allocator = testing.allocator;
    var book = Book.init(allocator, 10, "史记", 66.6, null);
    defer book.deinit();

    try book.addTag("历史");
    try book.addTag("中国");

    var book2 = try book.clone();
    defer book2.deinit();
    book2.id = 11;
    try book2.addTag("畅销");

    try testing.expectEqual(10, book.id);
    try testing.expectEqual(11, book2.id);
    try testing.expect(book.tags.items.ptr != book2.tags.items.ptr);
}

test "reflection" {
    // - built-in functions
    //      - @TypeOf(val) type
    //      - @typeName(type) []const u8
    //      - @typeInfo(type) std.builtin.Type
    //      - @Type(std.builtin.Type) type
    //      - @field(type, "fieldname") field getter/setter
    // - std.meta functions
}
