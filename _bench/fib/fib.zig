const std = @import("std");

pub fn main() void {
    const n: u64 = 40;
    std.debug.print("{d}\n", .{fib(n)});
}

fn fib(n: u64) u64 {
    if (n < 2) {
        return 1;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}
