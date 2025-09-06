import chip
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/otp/supervision
import kv_demo/bucket

pub type Id =
  String

pub type BucketStore =
  chip.Registry(bucket.Message, Id)

/// Starts a BucketStore 
pub fn start() -> Result(actor.Started(BucketStore), actor.StartError) {
  chip.start(chip.Unnamed)
}

/// Returns childspec of BucketStore for supervisor
pub fn childspec() {
  supervision.worker(start)
}

/// Indexs bucket 
pub fn index(store: BucketStore, id: Id, subject: Subject(bucket.Message)) {
  chip.register(store, id, subject)
}

/// Gets bucket 
pub fn get(store: BucketStore, id: Id) -> Result(Subject(bucket.Message), Nil) {
  case chip.members(store, id, 2) {
    [] -> Error(Nil)
    [subject] -> Ok(subject)
    [_, ..] -> panic as "Shouldn't insert more than 1 subject."
  }
}
