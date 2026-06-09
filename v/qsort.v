#!/usr/bin/env -S v -d trace_http_request -d trace_http_response run

import net.http
import encoding.binary

fn main() {
	run_qsort()
}

fn run_qsort() {
	mut a := []int{len: 4, init: 0}
	// random ints concurrently
	ch := chan int{cap: a.len}
	defer { ch.close() }
	for _ in 0 .. a.len {
		go fn (ch chan int) {
			ch <- (random_int() % 100)
		}(ch)
	}
	for mut v in a {
		v = <-ch
	}
	// qsort
	println(a)
	qsort(mut a)
	println(a)
}

fn qsort[T](mut a []T) {
	if a.len < 2 { return }
	p := partition(mut a)
	if p > 0 {
		qsort(mut a[..p])
	}
	if p < a.len - 1 {
		qsort(mut a[(p + 1)..])
	}
}

fn partition[T](mut a []T) int {
	r := a.len - 1
	pivot := a[r]
	mut p := 0
	for mut v in a {
		if v < pivot {
			a[p], v = v, a[p]
			p += 1
		}
	}
	a[p], a[r] = a[r], a[p]
	return p
}

fn random_int() int {
	bytes := random_bytes(4) or { return 0 }
	return int(binary.big_endian_u32(bytes))
}

fn random_bytes(n u32) ![]u8 {
	url := 'https://httpbin.org/bytes/${n}'
	mut header := http.new_header()
	header.add(http.CommonHeader.accept, 'application/octet-stream')
	response := http.fetch(
		method: .get
		url:    url
		header: header
	)!
	if response.status() == .ok {
		body_str := response.body
		return body_str.bytes()
	} else {
		return error('Error http status: code=${response.status_code}')
	}
}
