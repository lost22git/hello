open Printf

let () = print_endline "=== tco ==="
let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

let fib_tail_recur n =
  let () = assert (n >= 0) in
  let rec visit i a b = if i == 0 then a else visit (i - 1) (a + b) a in
  visit n 1 0

let () = printf "fib_tail_recur 0 = %d\n" (fib_tail_recur 0)
let () = printf "fib_tail_recur 1 = %d\n" (fib_tail_recur 1)
let () = printf "fib_tail_recur 11 = %d\n" (fib_tail_recur 11)
let () = printf "fib_tail_recur 111 = %d\n" (fib_tail_recur 111)
let () = printf "fib_tail_recur 1111 = %d\n" (fib_tail_recur 1111)

let fib_loop n =
  let () = assert (n >= 0) in
  let ab = ref (1, 0) in
  for _ = 1 to n do
    let a, b = !ab in
    ab := (a + b, a)
  done;
  fst !ab

let () = printf "fib_loop 0 = %d\n" (fib_loop 0)
let () = printf "fib_loop 1 = %d\n" (fib_loop 1)
let () = printf "fib_loop 11 = %d\n" (fib_loop 11)
let () = printf "fib_loop 111 = %d\n" (fib_loop 111)
let () = printf "fib_loop 1111 = %d\n" (fib_loop 1111)
