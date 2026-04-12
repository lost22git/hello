#!/usr/bin/env ocaml

(** Open file channel to output. *)
let with_output_file ?(mode = [ Open_creat; Open_trunc; Open_wronly ])
    ?(perm = 0o666) ~f path =
  let out = open_out_gen mode perm path in
  Fun.protect ~finally:(fun () -> close_out out) @@ fun () -> f out

(** Spit string to file. *)
let spit ?(mode = [ Open_creat; Open_trunc; Open_wronly ]) path str =
  with_output_file ~mode path ~f:(fun out -> output_string out str)

(** Open file channel to input. *)
let with_input_file ~f path =
  let in_c = open_in_gen [ Open_rdonly ] 0o666 path in
  Fun.protect ~finally:(fun () -> close_in in_c) @@ fun () -> f in_c

(** Slurp string of file. *)
let slurp path =
  with_input_file path ~f:(fun in_c ->
      begin
        let len = in_channel_length in_c in
        really_input_string in_c len
      end)

(** Make a line sequence of input channel. *)
let line_seq in_c =
  let rec make_seq in_c () =
    try
      let line = input_line in_c in
      Seq.Cons (line, make_seq in_c)
    with End_of_file -> Seq.Nil
  in
  make_seq in_c

(* test *)

let () =
  let data =
    {|
{
  "name": "OCaml",
  "desc": "a ML programming language."
}
  |}
  in
  let path = Filename.get_temp_dir_name () ^ "/foo.json" in
  (* split *)
  spit path data;
  (* slurp  *)
  slurp path |> print_endline;
  (* line_seq *)
  with_input_file path ~f:(fun in_c ->
      begin
        let lines = line_seq in_c in
        lines |> Seq.iter print_endline
      end)
