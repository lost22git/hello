package main

import "core:testing"

Permission :: enum {
	Read,
	Write,
	Execute,
}

Permissions :: bit_set[Permission]

@(test)
test_bit_set :: proc(t: ^testing.T) {
	p := Permissions{.Read}
	testing.expect(t, Permission.Read in p)
	testing.expect(t, Permission.Write not_in p)

  testing.expect_value(t,  ~p, Permissions{ .Write, .Execute})
	testing.expect_value(t, Permissions{.Read,.Write} & p, Permissions{.Read})
	testing.expect_value(t, Permissions{.Write} & p, nil)
	testing.expect_value(t, Permissions{.Write} + p, Permissions{.Read, .Write})
  testing.expect_value(t,  Permissions{.Read, .Write} - p, Permissions{ .Write})
}
