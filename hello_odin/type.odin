///usr/bin/env odin test "$0" -file "$@" ; exit $?

package main

import "base:runtime"
import "core:encoding/endian"
import "core:log"
import "core:mem"
import "core:testing"


@(test)
test_int :: proc(t: ^testing.T) {
	testing.expect_value(t, size_of(uint), 8)
	testing.expect_value(t, align_of(uint), 8)
	testing.expect_value(t, size_of(int), 8)
	testing.expect_value(t, align_of(int), 8)

	testing.expect_value(t, size_of(u8), 1)
	testing.expect_value(t, align_of(u8), 1)
	testing.expect_value(t, size_of(i8), 1)
	testing.expect_value(t, align_of(i8), 1)

	testing.expect_value(t, size_of(u16), 2)
	testing.expect_value(t, align_of(u16), 2)
	testing.expect_value(t, size_of(i16), 2)
	testing.expect_value(t, align_of(i16), 2)

	testing.expect_value(t, size_of(u32), 4)
	testing.expect_value(t, align_of(u32), 4)
	testing.expect_value(t, size_of(i32), 4)
	testing.expect_value(t, align_of(i32), 4)

	testing.expect_value(t, size_of(u64), 8)
	testing.expect_value(t, align_of(u64), 8)
	testing.expect_value(t, size_of(i64), 8)
	testing.expect_value(t, align_of(i64), 8)

	testing.expect_value(t, size_of(u128), 16)
	testing.expect_value(t, align_of(u128), 16)
	testing.expect_value(t, size_of(i128), 16)
	testing.expect_value(t, align_of(i128), 16)
}

@(test)
test_float :: proc(t: ^testing.T) {
	testing.expect_value(t, size_of(f32), 4)
	testing.expect_value(t, align_of(f32), 4)

	testing.expect_value(t, size_of(f64), 8)
	testing.expect_value(t, align_of(f64), 8)
}

@(test)
test_ptr :: proc(t: ^testing.T) {
	testing.expect_value(t, size_of(rawptr), 8)
	testing.expect_value(t, align_of(rawptr), 8)
}

@(test)
test_allocator :: proc(t: ^testing.T) {
	// see base/runtime/core.odin Allocator
	testing.expect_value(t, size_of(runtime.Allocator), 16) // data_ptr + proc_ptr
	testing.expect_value(t, align_of(runtime.Allocator), 8)
	testing.expect_value(t, offset_of(runtime.Allocator, procedure), 0)
	testing.expect_value(t, offset_of(runtime.Allocator, data), 8)
}

@(test)
test_array :: proc(t: ^testing.T) {
	testing.expect_value(t, size_of([4]int), 4 * 8)
	testing.expect_value(t, align_of([4]int), 8)
}

@(test)
test_matrix :: proc(t: ^testing.T) {
	testing.expect_value(t, size_of(matrix[2, 3]int), 2 * 3 * 8)
	testing.expect_value(t, align_of(matrix[2, 3]int), 2 * 8)

	testing.expect_value(t, size_of(matrix[3, 3]int), 3 * 3 * 8)
	testing.expect_value(t, align_of(matrix[3, 3]int), 8) // why not 3*8? how to calcuate it
}

@(test)
test_string :: proc(t: ^testing.T) {
	// see base/runtime/core.odin Raw_String 
	testing.expect_value(t, size_of(string), 8 + 8) // data_ptr + len
	testing.expect_value(t, align_of(string), 8)
	testing.expect_value(t, offset_of(runtime.Raw_String, data), 0)
	testing.expect_value(t, offset_of(runtime.Raw_String, len), 8)
}

@(test)
test_cstring :: proc(t: ^testing.T) {
	// see base/runtime/core.odin Raw_Cstring 
	testing.expect_value(t, size_of(cstring), 8)
	testing.expect_value(t, align_of(cstring), 8)
	testing.expect_value(t, offset_of(runtime.Raw_Cstring, data), 0)
}

@(test)
test_slice :: proc(t: ^testing.T) {
	// see base/runtime/core.odin Raw_Slice
	testing.expect_value(t, size_of([]int), 8 + 8) // data_ptr + len
	testing.expect_value(t, align_of([]int), 8)
	testing.expect_value(t, offset_of(runtime.Raw_Slice, data), 0)
	testing.expect_value(t, offset_of(runtime.Raw_Slice, len), 8)
}

@(test)
test_dunamic_array :: proc(t: ^testing.T) {
	// see base/runtime/core.odin Raw_Dynamic_Array
	testing.expect_value(t, size_of([dynamic]int), 8 + 8 + 8 + 16) // data_ptr + len + cap + allocator
	testing.expect_value(t, align_of([dynamic]int), 8)
	testing.expect_value(t, offset_of(runtime.Raw_Dynamic_Array, data), 0)
	testing.expect_value(t, offset_of(runtime.Raw_Dynamic_Array, len), 8)
	testing.expect_value(t, offset_of(runtime.Raw_Dynamic_Array, cap), 16)
	testing.expect_value(t, offset_of(runtime.Raw_Dynamic_Array, allocator), 24)
}

@(test)
test_map :: proc(t: ^testing.T) {
	// see base/runtime/core.odin Raw_Map
	testing.expect_value(t, size_of(map[string]int), 8 + 8 + 16) // data_ptr + len + allocator
	testing.expect_value(t, align_of(map[string]int), 8)
	testing.expect_value(t, offset_of(runtime.Raw_Map, data), 0)
	testing.expect_value(t, offset_of(runtime.Raw_Map, len), 8)
	testing.expect_value(t, offset_of(runtime.Raw_Map, allocator), 16)
}

@(test)
test_context :: proc(t: ^testing.T) {
	// see base/runtime/core.odin Context
	// Context :: struct {
	// 	 allocator:              Allocator,
	// 	 temp_allocator:         Allocator,
	// 	 assertion_failure_proc: Assertion_Failure_Proc,
	// 	 logger:                 Logger,
	// 	 random_generator:       Random_Generator,
	// 
	// 	 user_ptr:   rawptr,
	// 	 user_index: int,
	// 
	// 	 // Internal use only
	// 	 _internal: rawptr,
	// }
	testing.expect_value(t, size_of(runtime.Context), 16 + 16 + 8 + 32 + 16 + 8 + 8 + 8) // 112
	testing.expect_value(t, align_of(runtime.Context), 8)
	testing.expect_value(t, offset_of(runtime.Context, allocator), 0)
	testing.expect_value(t, offset_of(runtime.Context, temp_allocator), 16)
	testing.expect_value(t, offset_of(runtime.Context, assertion_failure_proc), 32)
	testing.expect_value(t, offset_of(runtime.Context, logger), 40)
	testing.expect_value(t, offset_of(runtime.Context, random_generator), 72)
	testing.expect_value(t, offset_of(runtime.Context, user_ptr), 88)
	testing.expect_value(t, offset_of(runtime.Context, user_index), 96)
	testing.expect_value(t, offset_of(runtime.Context, _internal), 104)
}

Book :: struct {
	id:    u32,
	name:  string,
	price: f32,
}

Book2 :: struct {
	name:  string,
	id:    u32,
	price: f32,
}

Book_Packed :: struct #packed {
	id:    u32,
	name:  string,
	price: f32,
}

@(test)
test_struct :: proc(t: ^testing.T) {
	testing.expect_value(t, size_of(Book), (4 + 4) + 16 + (4 + 4))
	testing.expect_value(t, align_of(Book), 8)
	testing.expect_value(t, offset_of(Book, id), 0)
	testing.expect_value(t, offset_of(Book, name), 8)
	testing.expect_value(t, offset_of(Book, price), 24)


	testing.expect_value(t, size_of(Book2), 16 + 4 + 4)
	testing.expect_value(t, align_of(Book2), 8)
	testing.expect_value(t, offset_of(Book2, name), 0)
	testing.expect_value(t, offset_of(Book2, id), 16)
	testing.expect_value(t, offset_of(Book2, price), 20)

	testing.expect_value(t, size_of(Book_Packed), 4 + 16 + 4)
	testing.expect_value(t, align_of(Book_Packed), 1)
	testing.expect_value(t, offset_of(Book_Packed, id), 0)
	testing.expect_value(t, offset_of(Book_Packed, name), 4)
	testing.expect_value(t, offset_of(Book_Packed, price), 20)
}

@(test)
test_union :: proc(t: ^testing.T) {
	testing.expect_value(t, size_of(union {
				int,
				string,
			}), 24)
	testing.expect_value(t, align_of(union {
				int,
				string,
			}), 8)
}

@(test)
test_offset_of_group :: proc(t: ^testing.T) {
	testing.expect_value(
		t,
		offset_of(runtime.Raw_Slice, len),
		offset_of_by_string(runtime.Raw_Slice, "len"),
	)

	s := []int{1, 2, 3, 4}
	rs := transmute(runtime.Raw_Slice)s
	testing.expect_value(t, offset_of(rs.len), offset_of(runtime.Raw_Slice, len))
}

@(test)
test_u32be_u32le :: proc(t: ^testing.T) {
	a := u32le(1)
	testing.expect_value(t, a, 1)
	testing.expect_value(t, transmute([4]u8)a, [4]u8{1, 0, 0, 0})

	// le -> be, mutate inner bytes
	b := u32be(a)

	testing.expect_value(t, b, 1)
	testing.expect_value(t, transmute([4]u8)b, [4]u8{0, 0, 0, 1})

	i: u32
	bytes := [4]u8{0, 0, 0, 1} // 1be
	{
		i = u32(transmute(u32be)bytes)
		testing.expect_value(t, i, 1)
	}
	// equals to the former
	{
		v, _ := endian.get_u32(bytes[:], .Big)
		i = u32(v)
		testing.expect_value(t, i, 1)
	}
	// equals to the former
	{
		v := endian.unchecked_get_u32be(bytes[:])
		i = u32(v)
		testing.expect_value(t, i, 1)
	}
}

@(test)
test_raw_data :: proc(t: ^testing.T) {
	s := "hello"
	testing.expect_value(t, raw_data(s), raw_data(transmute([]u8)s))

	a := []int{1, 1, 1, 1}
	testing.expect_value(t, raw_data(a), raw_data(a[0:1]))
}

@(test)
test_expand_values :: proc(t: ^testing.T) {
  b := Book{id=1, name="Odin lang sepcification", price=0}
  id, name, price := expand_values(b)

  testing.expect_value(t, id, 1)
  testing.expect_value(t, name, "Odin lang sepcification")
  testing.expect_value(t, price, 0)
}
