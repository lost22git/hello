#!/usr/bin/env ocaml

open Effect.Deep

type _ Effect.t +=
  | Log : string -> unit Effect.t
  | Ask : string -> string Effect.t

let guess max_number =
  let () = Random.self_init () in
  let number = Random.int max_number in
  let rec loop () =
    let prompt = "Guess a number [0-" ^ string_of_int max_number ^ "): " in
    let answer = Effect.perform @@ Ask prompt in
    let guess_number = try int_of_string answer with e -> -1 in
    if guess_number = number then
      Effect.perform @@ Log "Congratulation, you guess right."
    else loop ()
  in
  loop ()

let log s = print_endline s

let ask s =
  print_string s;
  read_line ()

let handlers () =
  {
    effc =
      (fun (type c) (eff : c Effect.t) ->
        match eff with
        | Log s -> Some (fun (k : (c, _) continuation) -> log s |> continue k)
        | Ask s -> Some (fun (k : (c, _) continuation) -> ask s |> continue k)
        | _ -> None);
    exnc = (function e -> raise e);
    retc = (fun _ -> ());
  }

let () = match_with guess 10 @@ handlers ()
