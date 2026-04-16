
signature UTIL =
sig
  datatype file_open_mode = File_open_mode_append 
                          | File_open_mode_truncate
                          
  val |> : 'a * ('a->'b) -> 'b
  val env : string -> string -> string
  val println : string -> unit
  val protect : (unit->unit) -> (unit->'a) -> 'a
  val range : int -> int -> int list
  val sleep : LargeInt.int -> unit
  val sleep_ms : LargeInt.int -> unit
  val time_it : string -> (unit->'a) -> 'a
  val with_lock : Thread.Mutex.mutex -> (unit->'a) -> 'a
  val with_input_file : string -> (TextIO.instream->'a) -> 'a
  val with_output_file : file_open_mode -> string -> (TextIO.outstream->'a) -> 'a
  val to_rfc3339 : Time.time -> string
end;

structure Util : UTIL =
struct
  datatype file_open_mode = File_open_mode_append
                          | File_open_mode_truncate
  
  infix 0 |>
  fun x |> f = f x
  
  fun env name default_value = 
    Option.getOpt ((OS.Process.getEnv name), default_value)
    
  fun println str = (print str; print "\n")

  fun protect finally f =
    let val result = f () handle exn => (finally (); raise exn)
    in 
      finally ();
      result
    end

  fun range start stop = 
    if start > stop then []
    else start :: (range (start+1) stop)

  fun sleep n = 
    (Posix.Process.sleep (Time.fromSeconds n); ())
  
  fun sleep_ms n = 
    (Posix.Process.sleep (Time.fromMilliseconds n); ())

  fun time_it desc f =
    let val start = Time.now ()
        val finally = fn () => println (desc ^ " (elapsed: " ^ (Time.toString (Time.now () - start)) ^ ")")
    in
      protect finally f
    end

  fun with_lock mu f = 
    let val () = Thread.Mutex.lock mu
        val finally = fn () => Thread.Mutex.unlock mu
    in
      protect finally f
    end

  fun with_input_file path f =
    let val in_c = TextIO.openIn path
        val cleanup = fn () => TextIO.closeIn in_c
        val run = fn () => f in_c
    in
      protect cleanup run
    end

  fun with_output_file file_open_mode path f = 
    let val out = case file_open_mode of
      File_open_mode_append => TextIO.openAppend path
    | File_open_mode_truncate =>  TextIO.openOut path
        val cleanup = fn () => TextIO.closeOut out
        val run = fn () => f out
    in
      protect cleanup run
    end
  
  fun to_rfc3339 time = 
    Date.fmt "%Y-%m-%dT%H:%M:%SZ" (Date.fromTimeUniv time)
end;
