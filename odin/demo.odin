///usr/bin/env odin run "$0" -file "$@" ; exit $?

package main

import "core:fmt"
import "core:math/rand"

main :: proc () {
  run_fib()
  run_qsort()
  run_generic_and_overload()
}

run_fib :: proc() {
  fmt.println("=== run_fib ===")
  n :u64 = 40
  fmt.println(fib(n))
}

fib :: proc(n: u64) -> u64 {
  if n < 2 {
    return 1
  } else {
    return fib(n-1) + fib(n-2)
  }
}

run_qsort :: proc() {
  fmt.println("=== run_qsort ===")
  n :: 10
  a : [n]int
  for i in 0..<n {
    a[i] = int(rand.uint64()) % 100
  }
  fmt.println("a =", a)
  qsort(a[:], 0, len(a) - 1)
  fmt.println("a =", a)
}

qsort :: proc(a: []$T, l, r: int) {
  if l >= r { return }
  p := partition(a, l, r)
  qsort(a, l, p - 1)
  qsort(a, p + 1, r)

  partition :: proc(a: []$T, l, r: int) -> int {
    pivot := a[r]
    p := l
    for i in l..<r {
      if a[i] < pivot {
        a[i], a[p] = a[p], a[i]
        p += 1
      }
    }
    a[r], a[p] = a[p], a[r]
    return p
  }
}

run_generic_and_overload :: proc() {
  fmt.println("=== run_generic_and_overload ===")

  a := A{x=1}
  b := B{x=1}

  inc_generic(&a, 1)
  inc_generic(&b, 1)

  inc_overload(&a, 1)
  inc_overload(&b, 1)

  fmt.println(a)
  fmt.println(b)
}

A :: struct {
  x: int
}

B :: struct {
  x: int
}

inc_generic :: proc(m: ^$T, d: int) {
  fmt.println("[inc_generic]", m^)
  m.x += d

  // inc_overload(m, d)
}

inc_overload :: proc{inc_a, inc_b}
inc_a :: proc(m: ^A, d: int) {
  fmt.println("[inc_a]", m^)
  m.x += d
}
inc_b :: proc(m: ^B, d: int) {
  fmt.println("[inc_b]", m^)
  m.x += d
}

