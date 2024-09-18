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

const CSI = struct {
    const Style = enum(u8) {
        bold = 1,
        italic = 2,
        fgRed = 31,
        fgGreen = 32,
        fgYellow = 33,
        fgBlue = 34,
    };

    const csi = "\x1b[";

    inline fn stringFromInt(comptime int: u32, comptime buf: []u8) ![]const u8 {
        return try std.fmt.bufPrint(buf, "{}", .{int});
    }

    pub inline fn fmt(comptime s: []const u8, comptime styles: []const Style) []const u8 {
        comptime var result: []const u8 = csi;
        {
            inline for (styles, 0..) |style, i| {
                const code = comptime blk: {
                    var buf: [2]u8 = undefined;
                    break :blk try stringFromInt(@intFromEnum(style), &buf);
                };
                result = result ++ code;
                if (i < styles.len - 1) {
                    result = result ++ ";";
                }
            }
            result = result ++ "m";
        } // add styles
        result = result ++ s; // add s
        result = result ++ csi ++ "m"; // add reset
        return result;
    }
};

pub fn myLogFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = scope;
    const level_str = switch (level) {
        .debug => CSI.fmt("D", &.{ .bold, .fgBlue }),
        .info => CSI.fmt("I", &.{ .bold, .fgGreen }),
        .warn => CSI.fmt("W", &.{ .bold, .fgYellow }),
        .err => CSI.fmt("E", &.{ .bold, .fgRed }),
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

fn handleRequest(arena: std.mem.Allocator, req: *std.http.Server.Request) !void {
    log.debug("Handling request...", .{});

    // new client
    var client = std.http.Client{
        .allocator = arena,
    };
    defer client.deinit();
    try std.http.Client.initDefaultProxies(&client, arena);

    // make target request
    const target = try getProxyTarget(req.head.target);
    const target_res_header_buf = try arena.alloc(u8, CLIENT_RESPONSE_HEADER_MAX_SIZE);
    defer arena.free(target_res_header_buf);
    var target_req = blk: {
        const uri = try std.Uri.parse(target);
        const host = uri.host.?.percent_encoded;
        log.debug("target host: {s}", .{host});
        const method = req.head.method;
        log.info("Fetching [{s}] {any}", .{ @tagName(method), uri });
        break :blk try client.open(method, uri, .{ .server_header_buffer = target_res_header_buf, .headers = .{ .host = .{ .override = host }, .user_agent = .{ .override = "curl" } } });
    };
    defer target_req.deinit();

    // send target request and wait for target response
    {
        log.debug(">> host => {any}", .{target_req.headers.host});
        log.debug(">> user_agent => {any}", .{target_req.headers.user_agent});
        log.debug(">> connection => {any}", .{target_req.headers.connection});
        log.debug(">> authorization => {any}", .{target_req.headers.authorization});
        log.debug(">> content_type => {any}", .{target_req.headers.content_type});
        log.debug(">> accept_encoding => {any}", .{target_req.headers.accept_encoding});
        try target_req.send();
        try ioCopy(try req.reader(), target_req.writer());
        try target_req.finish();
        try target_req.wait();
    }

    log.debug("<< status => {any}", .{target_req.response.status});
    log.debug("<< content-length => {any}", .{target_req.response.content_length});
    log.debug("<< transfer_encoding => {any}", .{target_req.response.transfer_encoding});

    // respond with target response
    {
        const res_send_buf = try arena.alloc(u8, SERVER_RESPONSE_SEND_BUFFER_SIZE);
        defer arena.free(res_send_buf);
        var res = req.respondStreaming(.{
            .send_buffer = res_send_buf,
            .content_length = target_req.response.content_length,
            .respond_options = .{
                .status = target_req.response.status,
                .version = target_req.response.version,
                .reason = target_req.response.reason,
                .keep_alive = target_req.response.keep_alive,
            },
        });
        try res.flush(); // pre-flush headers
        try ioCopy(target_req.reader(), res.writer());
        try res.end();
    }
}

fn ioCopy(reader: anytype, writer: anytype) !void {
    var fifo = std.fifo.LinearFifo(u8, .{ .Static = FIFO_BUFFER_SIZE }).init();
    defer fifo.deinit();
    try fifo.pump(reader, writer);
}

fn getProxyTarget(request_target: []const u8) ![]const u8 {
    log.debug("[getProxyTarget] request_target: {s}", .{request_target});
    if (std.mem.indexOf(u8, request_target, "?")) |index| {
        const query = request_target[(index + 1)..];
        var iter = std.mem.tokenizeScalar(u8, query, '&');
        while (iter.next()) |kv| {
            if (std.mem.startsWith(u8, kv, "target=")) {
                return kv[("target=".len)..];
            }
        }
    }
    return error.ProxyTargetNotFound;
}
