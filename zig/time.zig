///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const testing = std.testing;

test "time now" {
    const now = try std.time.Instant.now();
    std.debug.print("now: {}\n", .{now});

    const now2 = try std.time.Instant.now();
    std.debug.print("now2: {}\n", .{now2});

    const span = now2.since(now);
    std.debug.print("span: {d}\n", .{span});
}

test "time now unix timestamp" {
    const ts = std.time.timestamp();
    std.debug.print("ts: {d}\n", .{ts});

    const ts_ms = std.time.milliTimestamp();
    std.debug.print("ts_ms: {d}\n", .{ts_ms});
}

test "timer" {
    var timer = try std.time.Timer.start();
    std.time.sleep(2 * std.time.ns_per_s);
    const lap = timer.lap();

    try testing.expectEqual(2, lap / std.time.ns_per_s);
}
