///usr/bin/env zig test -freference-trace "$0" "$@" ; exit $?
const std = @import("std");
const mem = std.mem;

test "identifier" {
    const @"const" = "你好";
    try std.testing.expect(mem.eql(u8, @"const", "你好"));
}
