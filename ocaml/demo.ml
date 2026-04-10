#!/usr/bin/env ocaml

let () = print_endline "HELLO, OCAML."
let () = Printf.printf "HELLO, %s.\n" "OCAML"
let () = print_endline @@ "HELLO, " ^ "OCAML."

let () =
  let list = [ 1; 2; 3 ] @ [ 4; 5; 6 ] in
  list |> List.map Int.to_string |> String.concat "," |> print_endline
