
package main

import "core:testing"
import "core:log"
import "core:time"

@(test)
test_time_unix_time :: proc(t: ^testing.T) {
  log.warn("unix time:", time.to_unix_nanoseconds(time.now()))
}
