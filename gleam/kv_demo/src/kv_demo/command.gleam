import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}
import gleam/string
import kv_demo/bucket
import kv_demo/bucket_store.{type BucketStore}

pub type Command {
  New(bucket: String)
  Put(bucket: String, key: String, val: String)
  Get(bucket: String, key: String)
  Del(bucket: String, key: String)
}

pub type CommandResult {
  NewResult
  PutResult(Option(String))
  GetResult(Option(String))
  DelResult(Option(String))
}

pub type CommandError {
  UnknownCommand
  BucketNotFound
}

/// Pasrses the `line` into `Command`
pub fn parse(line: String) -> Result(Command, CommandError) {
  case string.split(line, on: " ") {
    ["NEW", bucket] -> Ok(New(bucket:))
    ["PUT", bucket, key, val] -> Ok(Put(bucket:, key:, val:))
    ["GET", bucket, key] -> Ok(Get(bucket:, key:))
    ["DEL", bucket, key] -> Ok(Del(bucket:, key:))
    _ -> Error(UnknownCommand)
  }
}

/// Runs the `command`
pub fn run(
  cmd: Command,
  bucket_store: BucketStore,
) -> Result(CommandResult, CommandError) {
  case cmd {
    New(bucket:) -> start_and_index_bucket(bucket_store, bucket)
    Put(bucket:, key:, val:) ->
      lookup_bucket(bucket_store, bucket, fn(b) {
        Ok(PutResult(bucket.put(b, key, val)))
      })
    Get(bucket:, key:) ->
      lookup_bucket(bucket_store, bucket, fn(b) {
        Ok(GetResult(bucket.get(b, key)))
      })
    Del(bucket:, key:) ->
      lookup_bucket(bucket_store, bucket, fn(b) {
        Ok(DelResult(bucket.del(b, key)))
      })
  }
}

fn start_and_index_bucket(
  bucket_store: BucketStore,
  name: String,
) -> Result(CommandResult, CommandError) {
  let b = bucket.start(name)
  bucket_store.index(bucket_store, name, b)
  Ok(NewResult)
}

fn lookup_bucket(
  bucket_store: BucketStore,
  name: String,
  callback: fn(Subject(bucket.Message)) -> Result(CommandResult, CommandError),
) {
  case bucket_store.get(bucket_store, name) {
    Error(_) -> Error(BucketNotFound)
    Ok(bucket) -> callback(bucket)
  }
}
