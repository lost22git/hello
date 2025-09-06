import kv_demo/command.{Del, Get, New, Put, UnknownCommand}

pub fn parse_test() {
  assert command.parse("NEW shopping") == Ok(New(bucket: "shopping"))

  assert command.parse("PUT shopping milk 10")
    == Ok(Put(bucket: "shopping", key: "milk", val: "10"))

  assert command.parse("GET shopping milk")
    == Ok(Get(bucket: "shopping", key: "milk"))

  assert command.parse("DEL shopping milk")
    == Ok(Del(bucket: "shopping", key: "milk"))

  assert command.parse("del shopping milk") == Error(UnknownCommand)
}
