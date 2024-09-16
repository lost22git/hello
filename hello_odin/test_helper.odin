package main

import "base:intrinsics"
import "core:testing"

expect_slice :: proc(t: ^testing.T, a, b: []$T) where intrinsics.type_is_comparable(T) {
	len := len(a)
	for i := 0; i < len; i += 1 {
		testing.expect_value(t, a[i], b[i])
	}
}
