fun main()
  val n = 40
  fib(n).println()

fun fib(n: int): div int
  if n < 2 then
    1
  else
    fib(n - 1) + fib(n - 2)
