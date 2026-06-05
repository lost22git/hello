package main

import "core:fmt"

main :: proc() {
  n: u64 = 40
  fmt.println(fib(n))
}

fib :: proc(n: u64) -> u64 {
  if n < 2 {
    return 1
  } else {
    return fib(n-1) + fib(n-2)
  }
}

