const std = @import("std");
const mem = std.mem;
const heap = std.heap;
const http = std.http;
const Uri = std.Uri;
const json = std.json;
const log = std.log;
const fmt = std.fmt;
const builtin = @import("builtin");
const fetcher = @import("./fetcher.zig");

pub const std_options = .{
    .log_level = switch (builtin.mode) {
        .Debug => .debug,
        .ReleaseSafe => .info,
        .ReleaseFast, .ReleaseSmall => .info,
    },
};

// {
//   "by" : "herbertl",
//   "descendants" : 0,
//   "id" : 39699333,
//   "score" : 26,
//   "time" : 1710376895,
//   "title" : "Care",
//   "type" : "story",
//   "url" : "https://johan.hal.se/wrote/2024/02/28/care/"
// }
const Story = struct {
    by: ?[]const u8 = null,
    descendants: ?u32 = null,
    id: u32,
    score: ?u32 = null,
    time: ?u32 = null,
    title: []const u8,
    type: ?[]const u8 = null,
    url: ?[]const u8 = null,
};

pub fn main() !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var client = http.Client{
        .allocator = allocator,
    };
    defer client.deinit();

    log.info("INIT CLIENT OK", .{});

    try client.initDefaultProxies(allocator);

    log.info("INIT PROXIES OK", .{});

    var arena = heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    if (try getTopStoryList(arena.allocator(), &client)) |items| {
        for (items) |item| {
            var story_arena = heap.ArenaAllocator.init(allocator);
            defer story_arena.deinit();

            if (try getStory(story_arena.allocator(), &client, item)) |*story| {
                log.info("STORY: {}", .{json.fmt(story, .{ .whitespace = .indent_2 })});
            }
        }
    }
}

fn getTopStoryList(allocator: mem.Allocator, client: *http.Client) !?[]const u32 {
    const uri = try Uri.parse("https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty");

    log.info("FETCHING: {}", .{uri});

    var server_header_buffer: [fetcher.RESPONSE_HEADER_BUFFER_SIZE]u8 = undefined;
    var body_buffer = std.ArrayList(u8).init(allocator);
    defer body_buffer.deinit();

    const result = try fetcher.fetchBuffered(client, .{ .location = .{ .uri = uri }, .method = .GET, .server_header_buffer = &server_header_buffer, .response_storage = .{ .dynamic = &body_buffer } });

    const status = result.status();
    switch (status.class()) {
        .success => {
            log.info("GOT OK RESPONSE", .{});
            return try result.bodyJsonParseLeaky([]const u32, allocator);
        },
        else => {
            log.err("GOT FAILED RESPONSE: status: {}", .{status});
            log.err("GOT FAILED RESPONSE: body: {?s}", .{result.body_buffer_optional});
            return null;
        },
    }
}

fn getStory(allocator: mem.Allocator, client: *http.Client, item: u32) !?Story {
    var uri_buffer: [100]u8 = undefined;
    const uri_formatted = try fmt.bufPrint(&uri_buffer, "https://hacker-news.firebaseio.com/v0/item/{d}.json?print=pretty", .{item});
    const uri = try Uri.parse(uri_formatted);

    log.info("FETCHING: {}", .{uri});

    var server_header_buffer: [fetcher.RESPONSE_HEADER_BUFFER_SIZE]u8 = undefined;
    var body_buffer = std.ArrayList(u8).init(allocator);
    defer body_buffer.deinit();

    const result = try fetcher.fetchBuffered(client, .{ .location = .{ .uri = uri }, .method = .GET, .server_header_buffer = &server_header_buffer, .response_storage = .{ .dynamic = &body_buffer } });

    const status = result.status();
    switch (status.class()) {
        .success => {
            log.info("GOT OK RESPONSE", .{});
            return try result.bodyJsonParseLeaky(Story, allocator);
        },
        else => {
            log.err("GOT FAILED RESPONSE: status: {}", .{status});
            log.err("GOT FAILED RESPONSE: body: {?s}", .{result.body_buffer_optional});
            return null;
        },
    }
}
