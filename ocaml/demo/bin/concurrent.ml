open Printf

let () = print_endline "=== concurrent ==="

let time f =
  let starttime = Unix.time () in
  Fun.protect
    ~finally:(fun () -> printf "time used: %.2fs\n" (Unix.time () -. starttime))
    f

let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

let string_of_int_list =
  Fun.compose (String.concat ",") (List.map string_of_int)

let do_work tasklist =
  let () = printf "task list is %s\n" (string_of_int_list tasklist) in
  let result =
    List.map (fun n () -> fib n) tasklist
    |> List.map Domain.spawn |> List.map Domain.join
    |> (fun rs ->
    printf "task result is %s\n" (string_of_int_list rs);
    rs)
    |> List.fold_left ( + ) 0
  in
  printf "folded result is %d\n" result

let () = time (fun () -> do_work [ 11; 22; 33; 44 ])
