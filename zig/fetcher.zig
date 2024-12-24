const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const fifo = std.fifo;
const http = std.http;
const json = std.json;
const Uri = std.Uri;

pub const FIFO_BUFFER_SIZE = 16 * 1024;
pub const RESPONSE_HEADER_BUFFER_SIZE = 16 * 1024;
pub const RESPONSE_BODY_BUFFER_SIZE = 2 * 1024 * 1024;

pub const FetchStreamedResult = struct {
    request: http.Client.Request,

    const Self = @This();

    pub fn init(request: http.Client.Request) Self {
        return .{
            .request = request,
        };
    }

    pub fn deinit(self: *Self) void {
        self.request.deinit();
        self.* = undefined;
    }

    pub fn response(self: *const Self) *const http.Client.Response {
        return &self.request.response;
    }

    pub fn status(self: *const Self) http.Status {
        return self.request.response.status;
    }

    pub fn headers(self: *const Self) http.HeaderIterator {
        return self.request.response.iterateHeaders();
    }

    pub fn bodyReader(self: *Self) http.Client.Request.Reader {
        return self.request.reader();
    }

    pub fn bodyEmpty(self: *Self) void {
        self.request.response.skip = true;
        std.assert(try self.request.transferRead(&.{}) == 0); // No buffer is necessary when skipping.
    }

    pub fn bodyStreamTo(self: *Self, dest_writer: anytype) !void {
        var pipe = fifo.LinearFifo(u8, .{ .Static = FIFO_BUFFER_SIZE }).init();
        defer pipe.deinit();
        try pipe.pump(self.request.reader(), dest_writer);
    }

    pub fn bodyStreamToFile(self: *Self, dest_file: fs.File) !void {
        defer dest_file.close();
        self.bodyStreamTo(dest_file.writer());
    }

    /// NOTE: allocator must be arena-like
    /// support for struct custom `fn jsonParse(allocator, source, options) !Self`
    ///
    pub fn bodyJsonParseLeaky(self: *Self, comptime T: type, allocator: mem.Allocator) !T {
        var reader = json.reader(allocator, self.request.reader());
        return try json.parseFromTokenSourceLeaky(T, allocator, &reader, .{
            .ignore_unknown_fields = true,
        });
    }

    /// NOTE: allocator must be arena-like
    /// support for struct custom `fn jsonParseFromValue(allocator: Allocator, source: json.Value, options: json.ParseOptions) !Self`
    ///
    pub fn bodyJsonParseLeaky2(self: *Self, comptime T: type, allocator: mem.Allocator) !T {
        var reader = json.reader(allocator, self.request.reader());
        var value = try json.parseFromTokenSource(json.Value, allocator, &reader, .{});
        defer value.deinit();
        return try json.parseFromValueLeaky(T, allocator, value, .{
            .ignore_unknown_fields = true,
        });
    }
};

pub fn fetchStreamed(client: *http.Client, uri: Uri, method: http.Method, options: http.Client.RequestOptions, send_data_reader: anytype) !FetchStreamedResult {
    var request = try client.open(method, uri, options);
    errdefer request.deinit();

    try request.send();

    if (send_data_reader != null) {
        var lfifo = fifo.LinearFifo(u8, .{ .Static = FIFO_BUFFER_SIZE }).init();
        defer lfifo.deinit();
        try lfifo.pump(send_data_reader, request.writer());
    }

    try request.finish();

    try request.wait();

    return FetchStreamedResult.init(request);
}

pub fn fetchBuffered(client: *http.Client, options: http.Client.FetchOptions) !FetchBufferedResult {
    const result = try client.fetch(options);
    return FetchBufferedResult.init(options, result);
}

const FetchBufferedResult = struct {
    options: http.Client.FetchOptions,
    result: http.Client.FetchResult,
    body_buffer_optional: ?[]const u8,

    const Self = @This();

    pub fn init(options: http.Client.FetchOptions, result: http.Client.FetchResult) Self {
        const body_buffer_optional = switch (options.response_storage) {
            .static => |list| list.items,
            .dynamic => |list| list.items,
            .ignore => null,
        };

        return .{
            .options = options,
            .result = result,
            .body_buffer_optional = body_buffer_optional,
        };
    }

    pub fn status(self: *const Self) http.Status {
        return self.result.status;
    }

    pub fn headers(self: *const Self) http.HeaderIterator {
        return http.HeaderIterator.init(self.options.server_header_buffer);
    }

    /// NOTE: allocator must be arena-like
    /// support for struct custom `fn jsonParse(allocator, source, options) !Self`
    ///
    pub fn bodyJsonParseLeaky(self: *const Self, comptime T: type, allocator: mem.Allocator) !T {
        if (self.body_buffer_optional) |body_buffer| {
            return try json.parseFromSliceLeaky(T, allocator, body_buffer, .{
                .allocate = .alloc_always,
                .ignore_unknown_fields = true,
            });
        } else {
            return error.BodyEmpty;
        }
    }

    /// NOTE: allocator must be arena-like
    /// support for struct custom `fn jsonParseFromValue(allocator: Allocator, source: json.Value, options: json.ParseOptions) !Self`
    ///
    pub fn bodyJsonParseLeaky2(self: *const Self, comptime T: type, allocator: mem.Allocator) !T {
        if (self.body_buffer_optional) |body_buffer| {
            var value = try json.parseFromSlice(json.Value, allocator, body_buffer, .{});
            defer value.deinit();
            return try json.parseFromValueLeaky(T, allocator, value, .{
                .allocate = .alloc_always,
                .ignore_unknown_fields = true,
            });
        } else {
            return error.BodyEmpty;
        }
    }
};
