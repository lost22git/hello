import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
import gleam/option.{None}
import gleam/result
import gleam/string
import glisten.{type Connection, Packet}
import kv_demo/bucket_store.{type BucketStore}
import kv_demo/command.{type CommandError, type CommandResult}

type State =
  BucketStore

/// Starts a server
pub fn start(port: Int, state: State) {
  glisten.new(init_connection(state, _), handle_tcp_message)
  |> glisten.bind("0.0.0.0")
  |> glisten.start(port)
}

fn init_connection(
  state: State,
  _conn: Connection(b),
) -> #(State, option.Option(process.Selector(b))) {
  #(state, None)
}

fn handle_tcp_message(
  state: State,
  msg: glisten.Message(a),
  conn: Connection(a),
) -> glisten.Next(State, glisten.Message(a)) {
  echo msg
  let assert Packet(msg) = msg
  let assert Ok(line) = bit_array.to_string(msg)

  let _ =
    line
    |> string.trim
    |> command.parse
    |> echo
    |> result.try(command.run(_, state))
    |> echo
    |> write_result(conn)

  glisten.continue(state)
}

fn write_result(
  result: Result(CommandResult, CommandError),
  conn: Connection(a),
) {
  let data = case result {
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
      }
  }
  glisten.send(conn, bytes_tree.from_string(data))
}
