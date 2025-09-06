import gleam/option.{None, Some}
import kv_demo/bucket

pub fn store_test() {
  let bucket = bucket.start("shopping")

  assert bucket.put(bucket, "milk", "10") == None
  assert bucket.get(bucket, "milk") == Some("10")

  assert bucket.del(bucket, "milk") == Some("10")
  assert bucket.get(bucket, "milk") == None
}
