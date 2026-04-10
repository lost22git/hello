let () = print_endline "=== regex ==="

type pattern = Raw of string | Re of Str.regexp

(** Make a `Str.regexp`. *)
let re_of_pattern pattern =
  match pattern with Raw s -> Str.regexp s | Re r -> r

(** Find a match in string. *)
let re_find re str ?(from = 0) (f : (int -> string) * (int -> int * int) -> 'a)
    =
  try
    let _ = Str.search_forward re str from in
    let epos = Str.match_end () in
    let group_str n = Str.matched_group n str in
    let group_pos n = (Str.group_beginning n, Str.group_end n) in
    let v = f (group_str, group_pos) in
    Some (v, epos)
  with Not_found -> None

(** Make a sequence of matches in string. *)
let re_seq pattern str ?(from = 0)
    (f : (int -> string) * (int -> int * int) -> 'a) =
  let re = re_of_pattern pattern in
  let rec make_seq pos () =
    match re_find re str ~from:pos f with
    | Some (v, epos) -> Seq.Cons (v, make_seq epos)
    | None -> Seq.Nil
  in
  make_seq from

(* test *)

type ip_list = (string * string) list [@@deriving show]

let () =
  let data = "110.120.0.1/24 110.120.1.1/24" in
  let pattern = Raw "\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)/\\([0-9]+\\)" in
  let seq =
    re_seq pattern data (fun (group_str, _) -> (group_str 1, group_str 2))
  in
  seq |> List.of_seq |> show_ip_list |> print_endline
