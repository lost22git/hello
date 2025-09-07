import gleam/dict.{type Dict}
import gleam/erlang/process.{
  type ExitReason, type Monitor, type Pid, type Subject,
}
import gleam/list
import gleam/option.{type Option}
import gleam/otp/actor

pub type Message {
  Get(pid: Subject(Option(String)), key: String)
  Put(pid: Subject(Option(String)), key: String, val: String)
  Del(pid: Subject(Option(String)), key: String)
  Sub(pid: Pid, subject: Subject(Message))
  ProcessDown(monitor: Monitor, pid: Pid, reason: ExitReason)
}

type State {
  State(bucket: Dict(String, String), subscribers: Dict(Pid, Subject(Message)))
}

pub type Bucket =
  Subject(Message)

/// Starts a new bucket
/// TODO gleam/otp has no DynamicSupervisor support, so we have no Supervisor for Bucket
pub fn start(name: String) -> Bucket {
  let assert Ok(actor) =
    actor.new(State(bucket: dict.new(), subscribers: dict.new()))
    |> actor.on_message(handle_message)
    |> actor.named(process.new_name(name))
    |> actor.start()
  process.unlink(actor.pid)
  actor.data
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
  actor.send(bucket, Sub(pid: process.self(), subject:))
}

fn handle_message(state: State, msg: Message) -> actor.Next(State, Message) {
  case msg {
    Put(pid:, key:, val:) -> {
      let old_val = dict.get(state.bucket, key) |> option.from_result()
      let new_state =
        State(..state, bucket: dict.insert(state.bucket, key, val))
      actor.send(pid, old_val)
      broadcast(state, msg)
      actor.continue(new_state)
    }
    Get(pid:, key:) -> {
      let val = dict.get(state.bucket, key) |> option.from_result()
      actor.send(pid, val)
      actor.continue(state)
    }
    Del(pid:, key:) -> {
      let val = dict.get(state.bucket, key) |> option.from_result()
      let new_state = State(..state, bucket: dict.delete(state.bucket, key))
      actor.send(pid, val)
      broadcast(state, msg)
      actor.continue(new_state)
    }
    Sub(pid:, subject:) -> {
      let new_state =
        State(
          ..state,
          subscribers: dict.insert(state.subscribers, pid, subject),
        )
      actor.continue(new_state)
    }
    ProcessDown(monitor: _, pid:, reason: _) -> {
      let new_state =
        State(..state, subscribers: dict.delete(state.subscribers, pid))
      actor.continue(new_state)
    }
  }
}

fn broadcast(state: State, msg: Message) {
  dict.values(state.subscribers)
  |> list.each(process.send(_, msg))
}
