import gleam/dict.{type Dict}
import gleam/erlang/process.{
  type ExitReason, type Monitor, type Pid, type Subject,
}
import gleam/list
import gleam/option.{type Option}
import gleam/otp/actor

pub type Message {
  Get(reply: Subject(Option(String)), key: String)
  Put(reply: Subject(Option(String)), key: String, val: String)
  Del(reply: Subject(Option(String)), key: String)
  Sub(reply: Subject(Nil), pid: Pid, subject: Subject(Message))
  ProcessDown(monitor: Monitor, pid: Pid, reason: ExitReason)
  Discard
}

type State {
  State(bucket: Dict(String, String), subscribers: Dict(Pid, Subject(Message)))
}

pub type Bucket =
  Subject(Message)

/// Starts a new bucket
/// NOTE: gleam/otp has no DynamicSupervisor support, so we have no Supervisor for Bucket
pub fn start(name: String) -> Bucket {
  let assert Ok(actor) =
    actor.new_with_initialiser(100, on_init)
    |> actor.on_message(handle_message)
    |> actor.named(process.new_name(name))
    |> actor.start()
  process.unlink(actor.pid)
  actor.data
}

fn on_init(subject: Subject(Message)) {
  let selector =
    process.new_selector()
    |> process.select(subject)
    |> process.select_monitors(decode_process_down)

  let state = State(bucket: dict.new(), subscribers: dict.new())

  actor.initialised(state)
  |> actor.selecting(selector)
  |> actor.returning(subject)
  |> Ok
}

fn decode_process_down(down: process.Down) {
  case down {
    process.ProcessDown(monitor:, pid:, reason:) ->
      ProcessDown(monitor:, pid:, reason:)
    _ -> Discard
  }
}

/// Gets a value from `bucket` by `key` 
pub fn get(bucket: Bucket, key: String) -> Option(String) {
  actor.call(bucket, waiting: 100, sending: Get(_, key))
}

/// Puts the `value` for the given `key` in the `bucket`
pub fn put(bucket: Bucket, key: String, val: String) -> Option(String) {
  actor.call(bucket, waiting: 100, sending: Put(_, key, val))
}

/// Deletes the value from `bucket`
/// Returns the current value of `key`, if `key` exists.
pub fn del(bucket: Bucket, key: String) -> Option(String) {
  actor.call(bucket, waiting: 100, sending: Del(_, key))
}

/// Subscribes `bucket`
pub fn sub(bucket: Bucket, subject: Subject(Message)) {
  actor.call(bucket, waiting: 100, sending: Sub(
    _,
    pid: process.self(),
    subject:,
  ))
}

fn handle_message(state: State, msg: Message) -> actor.Next(State, Message) {
  case msg {
    Put(reply:, key:, val:) -> {
      let old_val = dict.get(state.bucket, key) |> option.from_result()
      let new_state =
        State(..state, bucket: dict.insert(state.bucket, key, val))
      actor.send(reply, old_val)
      broadcast(state, msg)
      actor.continue(new_state)
    }
    Get(reply:, key:) -> {
      let val = dict.get(state.bucket, key) |> option.from_result()
      actor.send(reply, val)
      actor.continue(state)
    }
    Del(reply:, key:) -> {
      let val = dict.get(state.bucket, key) |> option.from_result()
      let new_state = State(..state, bucket: dict.delete(state.bucket, key))
      actor.send(reply, val)
      broadcast(state, msg)
      actor.continue(new_state)
    }
    Sub(reply:, pid:, subject:) -> {
      let new_state =
        State(
          ..state,
          subscribers: dict.insert(state.subscribers, pid, subject),
        )
      process.monitor(pid)
      actor.send(reply, Nil)
      actor.continue(new_state)
    }
    ProcessDown(monitor: _, pid:, reason: _) -> {
      let new_state =
        State(..state, subscribers: dict.delete(state.subscribers, pid))
      actor.continue(new_state)
    }
    Discard -> actor.continue(state)
  }
}

fn broadcast(state: State, msg: Message) {
  dict.values(state.subscribers)
  |> list.each(process.send(_, msg))
}
