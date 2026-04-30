when defined(nimHasQuirky):
  {.push quirky: on.}

proc fib(n: int): int =
  if n < 2:
    1
  else:
    fib(n - 1) + fib(n - 2)

when defined(nimHasQuirky):
  {.pop.}

let n = 40
echo fib(n)
