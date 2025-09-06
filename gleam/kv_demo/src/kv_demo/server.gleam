import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
import gleam/option.{None}
import gleam/result
import gleam/string
import glisten.{type Connection, Packet}
import kv_demo/bucket_store.{type BucketStore}
import kv_demo/command.{type Command, type CommandError, type CommandResult}

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
  let assert Packet(msg) = echo msg
  let assert Ok(line) = bit_array.to_string(msg)
  let data = handle(line, state)
  let assert Ok(_) = glisten.send(conn, bytes_tree.from_string(data))
  glisten.continue(state)
}

fn handle(line: String, state: State) -> String {
  line
  |> decode_command
  |> echo
  |> result.try(command.run(_, state))
  |> encode_command_result
  |> echo
}

fn decode_command(line: String) -> Result(Command, CommandError) {
  line
  |> string.trim
  |> command.parse
}

fn encode_command_result(res: Result(CommandResult, CommandError)) -> String {
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
      }
  }
}
