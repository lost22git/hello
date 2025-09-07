import gleam/erlang/process
import gleam/int
import gleam/io
import kv_demo/bucket_store
import kv_demo/server

pub fn main() {
  let assert Ok(bs) = bucket_store.start()
  let port = 8080
  let assert Ok(_) = server.start(port, bs.data)
  io.println("Server is on :" <> int.to_string(port))
  process.sleep_forever()
}
