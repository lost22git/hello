def fib(n : UInt64) : UInt64
  if n < 2
    return 1.to_u64
  else
    return (fib(n - 1) + fib(n - 2))
  end
end

n : UInt64 = 40
puts fib(n)
