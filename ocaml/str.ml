#!/usr/bin/env ocaml

let () =
  let info = {|
{
  "name": "janet",
  "age": 23
}
  |} in
  print_endline info
