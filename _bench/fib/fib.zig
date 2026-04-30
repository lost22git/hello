const std = @import("std");

pub fn main() void {
    const n = 40;
    std.debug.print("{d}\n", .{fib(n)});
}

fn fib(n: usize) usize {
    if (n < 2) {
        return 1;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}
