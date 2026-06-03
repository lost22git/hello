@main def main(): Unit =
  val n = 40
  println(fib(n))

def fib(n: Long): Long =
  if n < 2 then 1
  else fib(n - 1) + fib(n - 2)
