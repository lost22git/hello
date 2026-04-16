#!/usr/bin/env -S poly --script

PolyML.make "Util.sml";

open Util;

fun work mu cv i =
  with_lock mu (fn () =>
    let val () = Thread.ConditionVar.wait (cv, mu)
    in 
      println ("worker-" ^ (Int.toString i) ^ " is end.")
    end)
  
val () = 
  let val mu = Thread.Mutex.mutex () 
      val cv = Thread.ConditionVar.conditionVar ()
      val run = fn i => (Thread.Thread.fork ((fn () => work mu cv i), []); ())
  in
    List.app run (range 1 10);
    sleep 1;
    Thread.ConditionVar.broadcast cv;
    sleep 1;
    ()
  end
