# Check Tail Call Optimization support

# fib: recur variant
def fib_recur(n : Int) : Int
  return 1 unless n > 1
  return fib_recur(n - 1) &+ fib_recur(n - 2)
end

# fib: tail recur variant
def fib_tail_recur(n : Int) : Int
  return visit(n, 1, 0)
end

# this would check if TCO
private def visit(i : Int, a : Int, b : Int) : Int
  return a unless i > 0
  return visit(i - 1, a &+ b, a)
end

# fib: iterate variant
def fib_iterate(n : Int) : Int
  a = b = 1
  (1...n).each { a, b = a &+ b, a }
  return a
end

n = 1111

puts "fib_iterate(#{n}) = #{fib_iterate(n)}"
puts "fib_tail_recur(#{n}) = #{fib_tail_recur(n)}"
puts "fib_recur(#{n}) = #{fib_recur(n)}"
