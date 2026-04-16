#!/usr/bin/env -S poly --script

PolyML.make "WaitGroup.sml";

open Util;

infix 0 |>
  
(* function *)

fun greet name times =
  if times > 0 then
    let val () = println ("Hello, " ^ name)
    in
      greet name (times-1)
    end
  else ()

val () = greet "SML" 2

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2)

val () = println ("fib 11 => " ^ (Int.toString (fib 11)))
 

val () = ["polyml", "smlnj", "mlton"]
  |> List.map (String.map Char.toUpper)
  |> String.concatWith "; "
  |> println

(* exception handling *)

exception Error_foo of string
exception Error_bar of (string * string)

fun foobar () = 
    (
      let 
        val () = raise (Error_foo "foo")
        val () = raise (Error_bar ("bar", "bar"))
      in 
        "OK"
      end
    )
    handle Error_foo a => "Error_foo: " ^ a
      | Error_bar (a,b) => "Error_bar: " ^ a ^ ", " ^ b
  
val () = let val x = foobar ()
         in 
           println x
         end
         
(* IO *)
 
val () = let val tmpdir = env "TMPDIR" "."
             val path = tmpdir ^ "/foo"
             val output_text = "HELLO WORLD\n"
             val () = with_output_file File_open_mode_truncate path (fn out => TextIO.output (out, output_text))
             val input_text = with_input_file path (fn in_c => TextIO.inputAll in_c)
          in 
            println ("output: " ^ output_text);
            println ("input: " ^ input_text)
          end
