let rec fib = function 0 -> 1 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)

let () =
  let n = 40 in
  print_endline @@ string_of_int @@ fib n
