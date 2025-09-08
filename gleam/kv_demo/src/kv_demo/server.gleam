import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process.{type Subject}
import gleam/option.{Some}
import gleam/result
import gleam/string
import glisten.{type Connection, type Message, Packet, User}
import kv_demo/bucket
import kv_demo/bucket_store.{type BucketStore}
import kv_demo/command

type State {
  State(bucket_store: BucketStore, subject: Subject(bucket.Message))
}

/// Starts a server
pub fn start(port: Int, bucket_store: BucketStore) {
  glisten.new(init_connection(bucket_store, _), handle_message)
  |> glisten.with_close(on_connection_close)
  |> glisten.bind("0.0.0.0")
  |> glisten.start(port)
}

fn on_connection_close(_state: State) {
  Nil
}

fn init_connection(bucket_store: BucketStore, _conn: Connection(b)) {
  // subject to subscribe bucket messages
  let subject = process.new_subject()
  let state = State(bucket_store:, subject:)
  // add selector to select the subject
  let selector =
    process.new_selector()
    |> process.select(for: subject)
  #(state, Some(selector))
}

fn handle_message(
  state: State,
  msg: Message(bucket.Message),
  conn: Connection(bucket.Message),
) {
  case msg {
    Packet(msg) -> handle_tcp_message(state, msg, conn)
    User(msg) -> handle_user_message(state, msg, conn)
  }
}

fn handle_tcp_message(state: State, msg: BitArray, conn: Connection(a)) {
  case bit_array.to_string(msg) {
    Error(_) -> glisten.stop_abnormal("SERVER RECEIVED AN INVALID UTF-8 DATA")
    Ok(line) -> {
      let data = handle_command(line, state)
      let _ = glisten.send(conn, bytes_tree.from_string(data))
      glisten.continue(state)
    }
  }
}

fn handle_user_message(state: State, msg: bucket.Message, conn: Connection(a)) {
  let data = encode_bucket_message(msg)
  let _ = glisten.send(conn, bytes_tree.from_string(data))
  glisten.continue(state)
}

fn handle_command(line: String, state: State) {
  line
  |> decode_command
  |> echo
  |> result.try(command.run(_, state.bucket_store, state.subject))
  |> encode_command_result
  |> echo
}

fn decode_command(line: String) {
  line
  |> string.trim
  |> command.parse
}

fn encode_command_result(
  res: Result(command.CommandResult, command.CommandError),
) {
  case res {
    Error(cmd_error) ->
      case cmd_error {
        command.BucketNotFound -> "BUCKET NOT FOUND\r\n"
        command.UnknownCommand -> "UNKNOWN COMMAND\r\n"
      }
    Ok(cmd_result) ->
      case cmd_result {
        command.DelResult(v) -> option.unwrap(v, "nil") <> "\r\nOK\r\n"
        command.GetResult(v) -> option.unwrap(v, "nil") <> "\r\nOK\r\n"
        command.NewResult -> "OK\r\n"
        command.PutResult(v) -> option.unwrap(v, "nil") <> "\r\nOK\r\n"
        command.SubResult -> ""
      }
  }
}

fn encode_bucket_message(msg: bucket.Message) {
  case msg {
    bucket.Del(reply: _, key:) -> key <> " DELETED\r\n"
    bucket.Put(reply: _, key:, val:) -> key <> " SET TO " <> val <> "\r\n"
    _ -> ""
  }
}
