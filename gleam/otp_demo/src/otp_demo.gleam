import gleam/erlang
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/otp/actor
import gleam/otp/task

const timeout = 9999

fn unix_ms() -> Int {
  erlang.system_time(erlang.Millisecond)
}

fn timeit(f: fn() -> void) {
  let st = unix_ms()
  f()
  io.println("Elapsed: " <> int.to_string(unix_ms() - st) <> "ms")
}

pub fn main() {
  // task_demo
  timeit(fn() { echo task_demo() })

  // actor_demo
  timeit(fn() {
    let peer = actor_demo()
    actor.send(peer, Add(100))
    actor.send(peer, Add(42))
    actor.send(peer, Sub(100))
    echo actor.call(peer, Get, timeout)
    actor.send(peer, Stop)
  })
}

fn task_demo() -> Int {
  let t1 =
    task.async(fn() {
      process.sleep(1000)
      1
    })
  let t2 =
    task.async(fn() {
      process.sleep(2000)
      2
    })
  case task.try_await2(t1, t2, timeout) {
    #(Ok(a), Ok(b)) -> a + b
    _ -> panic as "t1 or t2 failed"
  }
}

pub type ActorMsg(a) {
  Stop
  Get(Subject(a))
  Add(a)
  Sub(a)
}

fn actor_demo() {
  let msg_handle = fn(msg: ActorMsg(Int), state: Int) {
    case msg {
      Stop -> actor.Stop(process.Normal)
      Get(peer) -> {
        actor.send(peer, state)
        actor.continue(state)
      }
      Add(v) -> actor.continue(state + v)
      Sub(v) -> actor.continue(state - v)
    }
  }
  let assert Ok(peer) = actor.start(0, msg_handle)
  peer
}
