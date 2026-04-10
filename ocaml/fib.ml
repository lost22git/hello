#!/usr/bin/env ocaml

open Printf

let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | n when n < 0 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let fib_rec n =
  let rec loop a b c = if c >= n then a else loop (a + b) a (c + 1) in
  loop 1 0 0

let fib_for n =
  let ab = ref (1, 0) in
  for i = 1 to n do
    let a, b = !ab in
    ab := (a + b, a)
  done;
  fst !ab

let test n =
  printf "fib     %2d = %d\n" n (fib n);
  printf "fib_for %2d = %d\n" n (fib_for n);
  printf "fib_rec %2d = %d\n" n (fib_rec n)

let () =
  test (-1);
  test 0;
  test 1;
  test 2;
  test 11
