signature WAITGROUP =
sig
  type t
  
  val wait_group : unit -> t
  val add : t -> unit
  val done : t -> unit
  val wait : t -> unit
  val fork : t -> (unit->unit) -> Thread.Thread.thread
end;

structure WaitGroup: WAITGROUP =
struct
  open Util;
  open Thread;
  
  type t = { mu: Mutex.mutex
           , cv: ConditionVar.conditionVar
           , count: int ref
           }
           
  fun wait_group () = {mu = Mutex.mutex (), cv = ConditionVar.conditionVar (), count = ref 0}

  fun add wg = 
    let val {mu, cv, count} = wg
    in
      with_lock mu (fn () => count:= (!count) + 1)
    end

  fun done wg =
    let val {mu, cv, count} = wg
    in
      with_lock mu (fn () => (
        count := (!count) - 1;
        if (!count) = 0 
        then ConditionVar.broadcast cv
        else ()
      ))
    end

  fun wait wg = 
    let val {mu, cv, count} = wg
    in
      with_lock mu (fn () => ConditionVar.wait (cv, mu))
    end

  fun fork wg f =
    let val run = fn () => (add wg; protect (fn () => done wg) f)
    in
      Thread.fork (run, [])
    end
end;

