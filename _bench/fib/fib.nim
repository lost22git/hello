proc fib(n: int): int =
  if n < 2:
    1
  else:
    fib(n - 1) + fib(n - 2)

let n = 40
echo fib(n)
