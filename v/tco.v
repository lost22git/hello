#!/usr/bin/env -S v run

// Check Tail Call Optimization support

// fib: recur variant
fn fib_recur(n int) int {
	if n <= 1 {
		return 1
	}

	return fib_recur(n - 1) + fib_recur(n - 2)
}

// fib: tail recur variant
fn fib_tail_recur(n int) int {
	return visit(n, 1, 0)
}

fn visit(i int, a int, b int) int {
	if i == 0 {
		return a
	}
	return visit(i - 1, a + b, a)
}

// fib: iterate variant
fn fib_iterate(n int) int {
	mut a, mut b := int(1), int(1)
	for _ in 1 .. n {
		a, b = a + b, a
	}
	return a
}

// Main

n := int(1111)
println('fib_tail_recur(${n}) = ${fib_tail_recur(n)}')
println('fib_iterate(${n}) = ${fib_iterate(n)}')
println('fib_recur(${n}) = ${fib_recur(n)}')
