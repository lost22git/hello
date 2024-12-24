package main

import "core:slice"
import "core:testing"


@(test)
test_slice_sort :: proc(t: ^testing.T) {
	source := []string{"foo", "bar", "zoo", "baz"}
	slice.sort(source)
	expect_slice(t, source, []string{"bar", "baz", "foo", "zoo"})
}

@(test)
test_slice_slicing :: proc(t: ^testing.T) {
	source := []string{"foo", "bar", "zoo", "baz"}
  slicing := source[1:4] // [1,4)
  expect_slice(t, slicing, []string{"bar", "zoo", "baz"})
}

@(test)
test_slice_multi_assignment :: proc(t: ^testing.T) {
	source := []string{"foo", "bar", "zoo", "baz"}
  source[1], source[2] = "BAR", "ZOO"
  expect_slice(t, source, []string{"foo", "BAR", "ZOO", "baz"})
}
