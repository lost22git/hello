#!/usr/bin/env -S v run

fn hello[T](v T) {
	println('HELLO ${v}')
}

fn test_generic() {
	hello('Vlang')
	hello(101)
}

interface Socket {
	fd int
mut:
	send(data []u8) !int
	recv(mut data []u8) !int
}

struct TcpSocket {
	fd int
}

fn (mut sock TcpSocket) send(data []u8) !int {
	println('send: ${data}')
	return data.len
}

fn (mut sock TcpSocket) recv(mut data []u8) !int {
	for mut b in data {
		b = 0x10
	}
	return data.len
}

fn send_recv(mut sock Socket) {
	mut data := []u8{len: 10, init: 0x01}
	_ := sock.send(data) or { return }
	_ := sock.recv(mut data) or { return }
	println('recv: ${data}')
}

fn test_interface() {
	mut sock := TcpSocket{
		fd: 1
	}
	send_recv(mut sock)
}

fn test_array() {
	// dynamic array
	mut da := []string{len: 0, cap: 10}
	println('sizeof da: ${sizeof(da)}')
	da << 'vlang'
	println('da: ${da.len} ${da.cap} ${da}')
	unsafe {
		println('da:  ${&da[0]:p}')
	}
	da2 := da // safe: da2 can't mutate shared buffer, da2 may be a immutable pointer of da
	unsafe {
		println('da2: ${&da2[0]:p}')
	}
	unsafe {
		mut da3 := da // unsafe: da3 may mutate shared buffer , causing share buffer to be inconsistent with metadata of da
		println('da3: ${&da3[0]:p}')
	}
	mut da4 := da.clone() // safe: da4 cloned from da, no shared buffer
	unsafe {
		println('da4: ${&da4[0]:p}')
		assert da[0] == da4[0]
	}
	// fix array
	fa := ['hello', 'world']!
	println('fa: ${typeof(fa).name} ${fa.len} ${fa}')
}

test_generic()
test_interface()
test_array()
