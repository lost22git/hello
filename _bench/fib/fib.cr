def fib(n : Int32) : Int32
  if n < 2
    return 1
  else
    return fib(n - 1) + fib(n - 2)
  end
end

n = 40
puts fib(n)
