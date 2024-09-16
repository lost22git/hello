package main

import "core:slice"
import "core:testing"

@(test)
test_dynamic_array_make :: proc(t: ^testing.T) {
  source := make([dynamic]string, 10, 10)
  defer delete(source)
  testing.expect_value(t, len(source), 10)
  testing.expect_value(t, cap(source), 10)
  testing.expect_value(t, source[5], "")
}

@(test)
test_dynamic_array_append :: proc(t: ^testing.T) {
  source: [dynamic]string
  defer delete(source)
  append(&source, "foo")
  append(&source, "bar")

  expect_slice(t, source[:], []string{"foo","bar"})
  testing.expect_value(t, len(source), 2)
}

@(test)
test_dynamic_array_remove :: proc(t: ^testing.T) {
  source: [dynamic]string
  defer delete(source)
  
  testing.expect_value(t, pop_safe(&source) or_else "", "")

  append(&source, "foo", "bar")
  testing.expect_value(t, pop_front(&source), "foo")
  testing.expect_value(t, pop_front(&source), "bar")
  testing.expect_value(t, pop_front_safe(&source) or_else "", "")

  append(&source, "foo", "bar", "zoo", "baz")
  ordered_remove(&source, 0)
  expect_slice(t, source[:], []string{"bar", "zoo", "baz"})
  unordered_remove(&source, 0)
  expect_slice(t, source[:], []string{"baz", "zoo"})
}

@(test)
test_dynamic_array_sort :: proc(t: ^testing.T) {
  source := [dynamic]string{"foo", "bar"}
  defer delete(source)
  slice.sort(source[:])
  expect_slice(t, source[:], []string{"bar", "foo"})
}

@(test)
test_assign_at :: proc(t: ^testing.T) {
  source: [dynamic]string
  defer delete(source)
  assign_at(&source, 2, "foo", "bar")
  expect_slice(t, source[:], []string{"", "", "foo", "bar"})
}
