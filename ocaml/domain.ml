#!/usr/bin/env -S ocaml -I +unix

#load "unix.cma"

(** At most, you can allocate (2^7 - 1) domain, so maybe this limit is stored in
    1 Byte. *)
let () =
  let total = 127 in
  let finished = Atomic.make 0 in
  let start_time = Unix.time () in
  for i = 1 to total do
    let _ =
      Domain.spawn (fun () ->
          Unix.sleep 3;
          let _ = Atomic.fetch_and_add finished 1 in
          ())
    in
    ()
  done;
  while Atomic.get finished < total do
    Unix.sleepf 0.1
  done;
  let end_time = Unix.time () in
  Printf.printf "time-total: %f\n" (end_time -. start_time)
