package main

import "core:testing"
import "core:reflect"

Direction :: enum {
  Up,
  Down,
  Left,
  Right,
}

@(test)
test_map_ops:: proc(t: ^testing.T) {
  m: map[Direction]string
  defer delete(m)

  // map add key value
  for field in reflect.enum_fields_zipped(Direction) {
    m[Direction(field.value)] = field.name
  }

  testing.expect_value(t, len(m), 4)

  // iterator map entries
  for k, v in m {
    testing.expect_value(t, reflect.enum_string(k), v)
  }
}
