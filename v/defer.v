#!/usr/bin/env -S v run

fn defer_in_for() {
	println('=== defer_in_for ===')
	for _ in 1 .. 10 {
		defer { println('defer') } // function scope, not block scope
	}
	println('end')
}

fn defer_lazy_eval() {
	println('=== defer_lazy_eval ===')
	mut a := 1
	println('init ${a}')
	defer {
		println('defer ${a}') // lazy eval
	}
	a = 2
	println('end ${a}')
}

defer_in_for()

defer_lazy_eval()
