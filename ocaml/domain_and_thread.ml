#!/usr/bin/env -S ocaml -I +unix -I +threads

(** A thread pool executor implementation inspired by
    [Java ThreadPoolExecutor](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/ThreadPoolExecutor.html)
*)

#load "unix.cma"

#load "threads.cma"

open Printf

type backend = Backend_domain | Backend_thread

type executor = {
  taskq : (unit -> unit) Queue.t;
  taskq_lock : Mutex.t;
  workers : (int, unit -> unit) Hashtbl.t;
  workers_lock : Mutex.t;
  mutable alive_workers : int;
  (* config *)
  max_workers : int;
  max_queue_size : int;
  backend : backend;
}

let make_executor ~backend ~max_queue_size ~max_workers =
  let taskq : (unit -> unit) Queue.t = Queue.create () in
  let taskq_lock = Mutex.create () in
  let workers : (int, unit -> unit) Hashtbl.t = Hashtbl.create max_workers in
  let workers_lock = Mutex.create () in
  {
    backend;
    max_queue_size;
    alive_workers = 0;
    max_workers;
    taskq;
    taskq_lock;
    workers;
    workers_lock;
  }

let current_worker_id backend =
  match backend with
  | Backend_domain -> Domain.self_index ()
  | Backend_thread -> Thread.id @@ Thread.self ()

let spawn backend f =
  match backend with
  | Backend_domain ->
      let _ = Domain.spawn f in
      ()
  | Backend_thread ->
      let _ = Thread.create f () in
      ()

let rec run_worker_loop executor is_worker_loop_stopped =
  if is_worker_loop_stopped () then ()
  else
    (* execute task which taken from taskq *)
    let lock = executor.taskq_lock in
    let task_opt =
      Mutex.protect lock @@ fun () -> Queue.take_opt executor.taskq
    in
    let () =
      match task_opt with
      | Some task -> begin try task () with _exn -> () end
      | None -> ()
    in
    (* continue loop *)
    if Option.is_none task_opt then Unix.sleepf 0.01;
    run_worker_loop executor is_worker_loop_stopped

(** Perform some preparation on worker started. Return true if worker state is
    fine, otherwise false (should kill worker on next step). *)
let prepare_worker executor worker_id stop_worker_loop =
  (* add worker into workers *)
  let lock = executor.workers_lock in
  Mutex.protect lock @@ fun () ->
  if executor.alive_workers < executor.max_workers then begin
    Hashtbl.add executor.workers worker_id stop_worker_loop;
    executor.alive_workers <- executor.alive_workers + 1;
    true
  end
  else false

let on_worker_dying executor worker_id =
  (* remove worker from wokers *)
  let lock = executor.workers_lock in
  Mutex.protect lock @@ fun () ->
  begin
    Hashtbl.remove executor.workers worker_id;
    executor.alive_workers <- executor.alive_workers - 1
  end

let start_worker executor =
  let _ =
    spawn executor.backend @@ fun () ->
    begin
      let worker_loop_is_stopped = ref false in
      let stop_worker_loop () = worker_loop_is_stopped := true in
      let is_worker_loop_stopped () = !worker_loop_is_stopped in
      (* on worker started *)
      let current_worker_id = current_worker_id executor.backend in
      let worker_state_is_fine =
        prepare_worker executor current_worker_id stop_worker_loop
      in
      if worker_state_is_fine then
        (* run worker loop *)
        Fun.protect ~finally:(fun () ->
            on_worker_dying executor current_worker_id)
        @@ fun () -> run_worker_loop executor is_worker_loop_stopped
    end
  in
  ()

let rec execute task executor =
  (* add task into taskq *)
  let taskq_lock = executor.taskq_lock in
  let taskq_is_full =
    Mutex.protect taskq_lock @@ fun () ->
    let len = Queue.length executor.taskq in
    if len < executor.max_queue_size then begin
      Queue.add task executor.taskq;
      false
    end
    else true
  in
  let workers_lock = executor.workers_lock in
  let more_worker_is_needed =
    Mutex.protect workers_lock @@ fun () ->
    begin
      executor.alive_workers = 0
      || (taskq_is_full && executor.alive_workers < executor.max_workers)
    end
  in
  (* start a worker if needed *)
  if more_worker_is_needed then start_worker executor;
  (* retry it later if queue is full, because we have already started one more worker to take tasks. *)
  if taskq_is_full then begin
    Unix.sleepf 0.01;
    execute task executor
  end
  else ()

(* TODO *)
let shutdown _executor =
  (* stop to add task to taskq *)
  (* wait taskq is empty *)
  (* stop workers loop *)
  (* wait workers is empty *)
  ()

(* test *)

let do_slow_work id task_finished =
  Unix.sleep 1;
  (* let () = printf "task-%d is finished.\n" id in *)
  (* let () = flush stdout in *)
  let _ = Atomic.fetch_and_add task_finished 1 in
  ()

let () =
  let executor =
    make_executor ~backend:Backend_thread ~max_queue_size:10 ~max_workers:5
  in
  let task_total = 30 in
  let task_finished = Atomic.make 0 in
  let start_time = Unix.time () in
  (* add tasks in an isolate thread *)
  spawn executor.backend (fun () ->
      for i = 1 to task_total do
        Unix.sleepf 0.3;
        executor |> execute @@ fun () -> do_slow_work i task_finished
      done);
  (* show progress *)
  let progress_total = 30 in
  print_endline "";
  while Atomic.get task_finished < task_total do
    let finished = Atomic.get task_finished in
    let progress_current = finished * progress_total / task_total in
    printf "\r[%s%s] t:%02d | q:%02d | w:%02d"
      (String.make progress_current '=')
      (String.make (progress_total - progress_current) ' ')
      finished
      (Queue.length executor.taskq)
      executor.alive_workers;
    flush stdout;
    Unix.sleepf 0.1
  done;
  printf "\r[%s] t:%02d | q:%02d | w:%02d"
    (String.make progress_total '=')
    task_total
    (Queue.length executor.taskq)
    executor.alive_workers;
  flush stdout;
  print_endline "";
  let end_time = Unix.time () in
  printf "task-total: %d, task-finished: %d, time-total: %f\n" task_total
    (Atomic.get task_finished) (end_time -. start_time)
