#!/usr/bin/env ocaml

type user = { name : string; age : int }

(* record creation *)
let () =
  let name = "janet" in
  let old_u = { name; age = 31 } in
  let new_u = { old_u with name = "julia" } in
  assert (new_u.name = "julia");
  assert (new_u.age = 31)

(*  record destructuring *)
let () =
  let u = { name = "janet"; age = 31 } in
  let { name; _ } = u in
  assert (name = "janet")
