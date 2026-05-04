///|
fn main {
  let n = 40
  println(fib(n))
}

///|
fn fib(n : Int) -> Int {
  if n < 2 {
    return 1
  } else {
    return fib(n - 1) + fib(n - 2)
  }
}
