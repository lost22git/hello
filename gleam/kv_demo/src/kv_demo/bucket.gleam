import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}
import gleam/otp/actor

type State {
  State(bucket: Dict(String, String))
}

pub type Message {
  Get(pid: Subject(Option(String)), key: String)
  Put(pid: Subject(Option(String)), key: String, val: String)
  Del(pid: Subject(Option(String)), key: String)
}

pub type Bucket =
  Subject(Message)

/// Starts a new bucket
/// TODO gleam/otp has no DynamicSupervisor support, so we have no Supervisor for Bucket
pub fn start(name: String) -> Bucket {
  let assert Ok(actor) =
    actor.new(State(bucket: dict.new()))
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

fn handle_message(state: State, msg: Message) -> actor.Next(State, Message) {
  case msg {
    Put(pid:, key:, val:) -> {
      let old_val = dict.get(state.bucket, key) |> option.from_result()
      let new_state = State(bucket: dict.insert(state.bucket, key, val))
      actor.send(pid, old_val)
      actor.continue(new_state)
    }
    Get(pid:, key:) -> {
      let val = dict.get(state.bucket, key) |> option.from_result()
      actor.send(pid, val)
      actor.continue(state)
    }
    Del(pid:, key:) -> {
      let val = dict.get(state.bucket, key) |> option.from_result()
      let new_state = State(bucket: dict.delete(state.bucket, key))
      actor.send(pid, val)
      actor.continue(new_state)
    }
  }
}
