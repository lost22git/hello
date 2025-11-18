open Printf

let () = print_endline "=== exception ==="

exception Error_foo
exception Error_bar of (int * string)

let handle_errors f =
  try f () with
  | Error_foo -> print_endline "got error: Error_foo"
  | Error_bar (code, msg) ->
      printf "got error: Error_bar code=%d msg=%S\n" code msg
  | _ -> print_endline "got unknown error"

let () =
  handle_errors (fun () -> raise Error_foo);
  handle_errors (fun () -> raise (Error_bar (404, "NOT FOUND")))

type errors = Foo | Bar of (int * string) | Unknown

let to_result f =
  try f () with
  | Error_foo -> Error Foo
  | Error_bar (code, msg) -> Error (Bar (code, msg))
  | _ -> Error Unknown

let handle_result = function
  | Ok _ -> print_endline "OK"
  | Error Foo -> print_endline "got error: Foo"
  | Error (Bar (code, msg)) -> printf "got error: Bar code=%d msg=%S\n" code msg
  | Error Unknown -> print_endline "got error: Unknown"

let () =
  handle_result (to_result (fun () -> raise Error_foo));
  handle_result (to_result (fun () -> raise (Error_bar (404, "NOT FOUND"))))
