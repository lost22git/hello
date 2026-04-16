#!/usr/bin/env -S poly --script

PolyML.make "WaitGroup.sml";

open Util;
open WaitGroup;

infix 0 |>

fun work2 i = (
    sleep 1;
    println ("worker-" ^ (Int.toString i) ^ " is done.")
  )
  
val () = 
  let val wg = wait_group ()
      val run = fn i => (fork wg (fn () => work2 i); ())
   in 
     range 1 10 |> List.app run;
     println "=== wait-group is waiting.";
     wait wg;
     println "=== wait-group waiting is end."
   end

