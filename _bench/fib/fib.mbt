///|
fn main {
  let n: UInt64 = 40
  println(fib(n))
}

///|
fn fib(n : UInt64) -> UInt64 {
  if n < 2 {
    return 1
  } else {
    return fib(n - 1) + fib(n - 2)
  }
}
