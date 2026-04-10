#!/usr/bin/env ocaml

open Printf;;

(* const *)
assert (1 = (Fun.const 1) 10);;

(* id *)
assert (1 = Fun.id 1);;

(* negate *)
assert ((Fun.negate (Fun.const false)) 1);;

(* flip *)
assert (3 = (Fun.flip ( / )) 3 10);;

(* compose *)
assert (11 = (Fun.compose ((Fun.flip ( / )) 2) (( * ) 2)) 11);;

(* function with named arguments *)
(* NOTE: append () as the last argument when the last is optional, since curry *)
let rec greet name ?(times = 1) () =
  if times > 0 then begin
    print_endline @@ "HELLO, " ^ name ^ ".";
    greet name ~times:(times - 1) ()
  end

let () = greet "OCAML" ()
let () = greet "OCAML" ~times:2 ()

(* NOTE: not need () when the last argument is not optional *)
let foo ?(bar = "") baz = print_endline @@ bar ^ baz
let () = foo "baz"
let () = foo ~bar:"bar" "baz"

(* NOTE: not support varargs since curry *)
