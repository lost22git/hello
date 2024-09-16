///usr/bin/env zig run -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const log = std.log;

pub const FIFO_BUFFER_SIZE = 16 * 1024;
pub const SERVER_REQUEST_MAX_SIZE = 1024 * 1024;
pub const CLIENT_RESPONSE_HEADER_MAX_SIZE = 16 * 1024;
pub const SERVER_RESPONSE_SEND_BUFFER_SIZE = 16 * 1024;

pub const std_options = .{
    .http_disable_tls = false,
    .log_level = .debug,
    .logFn = myLogFn,
};

pub fn myLogFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = scope;
    const csi = "\x1b[";
    const bold_red = csi ++ "1;31m";
    const bold_green = csi ++ "1;32m";
    const bold_yellow = csi ++ "1;33m";
    const bold_blue = csi ++ "1;34m";
    const reset = csi ++ "m";

    const level_str = switch (level) {
        .debug => bold_blue ++ "D" ++ reset,
        .info => bold_green ++ "I" ++ reset,
        .warn => bold_yellow ++ "W" ++ reset,
        .err => bold_red ++ "E" ++ reset,
    };
    std.debug.print(" [" ++ level_str ++ "] " ++ format ++ "\n", args);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() != std.heap.Check.leak);
    const allocator = gpa.allocator();

    try startServer(allocator, try std.net.Address.parseIp4("127.0.0.1", 8000));
}

fn startServer(allocator: std.mem.Allocator, addr: std.net.Address) !void {
    var server = try addr.listen(.{ .reuse_address = true });
    log.info("Proxy server is running on {any}", .{addr});
    while (server.accept()) |conn| {
        log.debug("Accepted a connection: {any}", .{conn.address});
        _ = try std.Thread.spawn(.{}, handleConnection, .{ allocator, conn });
    } else |err| {
        log.err("Error while accepting connection, err: {any}", .{err});
    }
}

fn handleConnection(allocator: std.mem.Allocator, conn: std.net.Server.Connection) !void {
    defer conn.stream.close();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    log.debug("Handling connection...", .{});
    const request_buf = try arena.allocator().alloc(u8, SERVER_REQUEST_MAX_SIZE);
    defer arena.allocator().free(request_buf);
    var httpserver = std.http.Server.init(conn, request_buf);

    while (httpserver.state == .ready) {
        log.debug("Receiving request...", .{});
        var request = try httpserver.receiveHead();
        handleRequest(arena.allocator(), &request) catch |err| {
            try handleRequestError(&request, err);
        };
    }
}

fn handleRequestError(request: *std.http.Server.Request, err: anyerror) !void {
    var error_content_buf: [100]u8 = undefined;
    try request.respond(try std.fmt.bufPrint(&error_content_buf, "Error while handling request, err: {any}\n", .{err}), .{ .status = .internal_server_error });
}

fn createProxy() std.http.Client.Proxy {
    return .{ .host = "127.0.0.1", .port = 55556, .protocol = .plain, .supports_connect = false, .authorization = null };
}

fn handleRequest(allocator: std.mem.Allocator, request: *std.http.Server.Request) !void {
    log.debug("Handling request...", .{});

    // new client
    var proxy = createProxy();
    var client = std.http.Client{
        .allocator = allocator,
        .http_proxy = &proxy,
        .https_proxy = &proxy,
    };
    defer client.deinit();

    // make client request
    const uri_str = try std.fmt.allocPrint(allocator, "https://github.com{s}", .{request.head.target});
    defer allocator.free(uri_str);
    const response_header_buf = try allocator.alloc(u8, CLIENT_RESPONSE_HEADER_MAX_SIZE);
    defer allocator.free(response_header_buf);
    var client_request = blk: {
        const uri = try std.Uri.parse(uri_str);
        const method = request.head.method;
        log.info("Do client request [{s}] {any}", .{ @tagName(method), uri });
        break :blk try client.open(method, uri, .{ .server_header_buffer = response_header_buf });
    };
    defer client_request.deinit();

    // send client request and wait for client response
    {
        try client_request.send();
        var fifo = std.fifo.LinearFifo(u8, .{ .Static = FIFO_BUFFER_SIZE }).init();
        defer fifo.deinit();
        try fifo.pump(try request.reader(), client_request.writer());
        try client_request.finish();
        try client_request.wait();
    }

    log.info("Client response status => {any}", .{client_request.response.status});
    log.info("Client response content-length => {any}", .{client_request.response.content_length});
    log.info("Client response transfer_encoding => {any}", .{client_request.response.transfer_encoding});

    // send client response as server response
    {
        const send_buf = try allocator.alloc(u8, SERVER_RESPONSE_SEND_BUFFER_SIZE);
        defer allocator.free(send_buf);
        var response = request.respondStreaming(.{
            .send_buffer = send_buf,
            .content_length = client_request.response.content_length,
            .respond_options = .{
                .status = client_request.response.status,
                .version = client_request.response.version,
                .reason = client_request.response.reason,
                .keep_alive = client_request.response.keep_alive,
            },
        });
        try response.flush(); // pre-flush headers
        var fifo = std.fifo.LinearFifo(u8, .{ .Static = FIFO_BUFFER_SIZE }).init();
        defer fifo.deinit();
        try fifo.pump(client_request.reader(), response.writer());
        try response.end();
    }
}
