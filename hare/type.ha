///usr/bin/env hare test "$0" "$@" ; exit $?

use fmt;
use endian;

@test fn test_int() void = {
	assert(size(size) == 8);
	assert(align(size) == 8);

	assert(size(int) == 4);
	assert(align(int) == 4);
	assert(size(uint) == 4);
	assert(align(uint) == 4);

	assert(size(i64) == 8);
	assert(align(i64) == 8);
	assert(size(u64) == 8);
	assert(align(u64) == 8);

	assert(size(i32) == 4);
	assert(align(i32) == 4);
	assert(size(u32) == 4);
	assert(align(u32) == 4);

	assert(size(i16) == 2);
	assert(align(i16) == 2);
	assert(size(u16) == 2);
	assert(align(u16) == 2);

	assert(size(i8) == 1);
	assert(align(i8) == 1);
	assert(size(u8) == 1);
	assert(align(u8) == 1);
};

@test fn test_float() void = {
	assert(size(f32) == 4);
	assert(align(f32) == 4);

	assert(size(f64) == 8);
	assert(align(f64) == 8);
};

@test fn test_bool() void = {
	assert(size(bool) == 1);
	assert(align(bool) == 1);
};

@test fn test_arr() void = {
	assert(size([4]int) == 4*4);
	assert(align([4]int) == 4);
};

@test fn test_str() void = {
	// struct 
	// {
	// 	data: nullable*[*]const u8,
	// 	length: size,
	// 	capacity: size,
	// }
	assert(size(str) == 3*8);
	assert(align(str) == 8);
};

@test fn test_slice() void = {
	// struct {
	// 	data: nullable*opaque, // See notes
	// 	length: size,
	// 	capacity: size,
	// }
	assert(size([]int) == 3*8);
	assert(align([]int) == 8);
};

type level = enum i8 {
	DBG,
	INF,
	WAR,
	ERR,
};

@test fn test_enum() void = {
	assert(size(level) == 1);
	assert(align(level) == 1);
};

type book = struct {
	id: int,
	name: str,
	price: int,
};


type book_packed = struct @packed {
	id: int,
	name: str,
	price: int,
};

type book2 = struct {
	name: str,
	id: int,
	price: int,
};

@test fn test_struct() void = {
	// book
	assert(size(book) == (4+4)+(24)+(4+4));
	assert(align(book) == 8);
	const b = book{
		id = 10,
		name = "",
		price = 10,
	};
	assert(offset(b.id) == 0);
	assert(offset(b.name) == 8);
	assert(offset(b.price) == 32);

	// book_packed
	assert(size(book_packed) == 4+24+4);
	assert(align(book_packed) == 8); // TODO why not 1 ?
	const b = book_packed{
		id = 10,
		name = "",
		price = 10,
	};
	assert(offset(b.id) == 0);
	assert(offset(b.name) == 4);
	assert(offset(b.price) == 28);

	
	// book2
	assert(size(book2) == 24+4+4);
	assert(align(book2) == 8);
	const b = book2{
		id = 10,
		name = "",
		price = 10,
	};
	assert(offset(b.name) == 0);
	assert(offset(b.id) == 24);
	assert(offset(b.price) == 28);
};

@test fn test_union() void = {
	assert(size((str|int)) == 8 + 24); // tag + max size of fields 
	assert(align((str|int)) == 8);
};

@test fn test_pointer() void = {
	assert(size(*str) == 8);
	assert(align(*str) == 8);

	assert(size(nullable *str) == 8);
	assert(align(nullable *str) == 8);
};

@test fn test_endian() void = {
	let bytes: [_]u8 = [1,0,0,0]; // 1le
	const v = endian::little.getu32(bytes[..]);
	assert(v == 1);

	endian::big.putu32(bytes[..], v);
	const expected: [4]u8 = [0,0,0,1];

	for (let i = 0z; i < len(bytes); i += 1) {
		assert(bytes[i] == expected[i]);
	};
};

