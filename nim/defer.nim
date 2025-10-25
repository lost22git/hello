proc foo() =
  for i in 1 .. 10:
    var n = i
    defer:
      echo n
    n = n * 10

foo()
