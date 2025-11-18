[@@@ocaml.warning "-a"]

open Printf

let () = print_endline "=== memory_layout ==="
let typeof v = Obj.repr v

(** get words count of payload of instance on heap *)
let instance_payload_words v = Obj.size (Obj.repr v)

(** get bytes count of payload of instance on heap *)
let instance_payload_bytes v = Sys.word_size / 8 * instance_payload_words v

let instance_words v = Obj.reachable_words (Obj.repr v)
let instance_bytes v = Sys.word_size / 8 * instance_words v

(* list *)
(* list is a cons cell *)
(* current 1 word + rest 1 word *)
let v = [ true; false; false ];;

assert (instance_payload_words v == 2);;
assert (instance_payload_bytes v == 2 * 8);;

(* array *)
(* fixed-length *)
(* each element ocuppied at least 1 word *)
let v = [| true; false |];;

assert (instance_payload_words v == 2);;
assert (instance_payload_bytes v == 2 * 8);;

(* string *)
(* immutable *)
(* fixed-length *)
let v = "1";;

assert (instance_payload_words v == 1);;
assert (instance_payload_bytes v == 1 * 8);;

let v = "22";;

assert (instance_payload_words v == 1);;
assert (instance_payload_bytes v == 1 * 8);;

let v = "7777777";;

assert (instance_payload_words v == 1);;
assert (instance_payload_bytes v == 1 * 8);;

let v = "88888888";;

assert (instance_payload_words v == 2);;
assert (instance_payload_bytes v == 2 * 8);;

(* bytes *)
(* fixed-length *)
let v = Bytes.create 1;;

assert (instance_payload_words v == 1);;
assert (instance_payload_bytes v == 1 * 8);;

let v = Bytes.create 7;;

assert (instance_payload_words v == 1);;
assert (instance_payload_bytes v == 1 * 8);;

let v = Bytes.create 8;;

assert (instance_payload_words v == 2);;
assert (instance_payload_bytes v == 2 * 8);;

(* buffer *)
(* growable *)
(* cap 1 word + len 1 word + data 1 word *)
let v = Buffer.create 1;;

assert (instance_payload_words v == 3);;
assert (instance_payload_bytes v == 3 * 8);;

let v = Buffer.create 8;;

assert (instance_payload_words v == 3);;
assert (instance_payload_bytes v == 3 * 8);;

let v = Buffer.create 1024;;

assert (instance_payload_words v == 3);;
assert (instance_payload_bytes v == 3 * 8);;

(* tuple *)
(* immutable *)
(* each element ocuppied at least 1 word  *)
let v = (true, false, false);;

assert (instance_payload_words v == 3);;
assert (instance_payload_bytes v == 3 * 8);;

(* hashtbl *)
(* mutable *)
let v = Hashtbl.create 2;;

Hashtbl.add v "compiler" "ocaml";;
Hashtbl.add v "formatter" "ocamlformat";;
assert (instance_payload_words v == 4);;
assert (instance_payload_bytes v == 4 * 8);;

(* map *)
(* immutable and persistent *)
module StringMap = Map.Make (String)

let v = StringMap.(empty |> add "a" 1);;

assert (instance_payload_words v == 5);;
assert (instance_payload_bytes v == 5 * 8);;

let v2 = StringMap.add "a" 2 v;;

assert (StringMap.find "a" v == 1);;
assert (StringMap.find "a" v2 == 2);;

(* record *)
(* each field ocuppied at least 1 word  *)
type user = { id : int32; name : string; coins : int32 }

let v = { id = 1l; name = "acb"; coins = 1l };;

assert (instance_payload_words v == 3);;
assert (instance_payload_bytes v == 3 * 8);;

(* boxed into heap *)
let a = 1
let v = ref a;;

assert (instance_payload_words v == 1);;
assert (instance_payload_bytes v == 1 * 8);;

(* option *)
let v = Some "foobarbaz";;

assert (instance_payload_words v == 1);;
assert (instance_payload_bytes v == 1 * 8);;

(* result *)
let v = Ok "foobarbaz";;

assert (instance_payload_words v == 1);;
assert (instance_payload_bytes v == 1 * 8);;

(* function *)
let v = fun a -> a;;

assert (instance_payload_words v == 2);;
assert (instance_payload_bytes v == 2 * 8);;

let v = fun a b -> a + b;;

assert (instance_payload_words v == 3);;
assert (instance_payload_bytes v == 3 * 8);;

let v = fun a b c -> a + b + c;;

assert (instance_payload_words v == 3);;
assert (instance_payload_bytes v == 3 * 8);;

(* error *)
exception Error_foo

let v = Error_foo;;

assert (instance_payload_words v == 2);;
assert (instance_payload_bytes v == 2 * 8);;

exception Error_bar of (string * int)

let v = Error_bar ("bar", -100);;

assert (instance_payload_words v == 2);;
assert (instance_payload_bytes v == 2 * 8)
