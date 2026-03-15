#!/usr/bin/env julia

"""
fib 
"""
fib(x) = x > 1 ? fib(x - 1) + fib(x - 2) : 1

"""
fib (recursive version)
"""
fib_recur(x) = begin
    aux(a, b, n) = n == 0 ? a : aux(a + b, a, n - 1)
    aux(1, 0, x)
end

"""
fib (loop version)
"""
fib_loop(x) = begin
    a, b = 1, 0
    for i in 1:x
        a, b = a + b, a
    end
    a
end

"""
fib (reduce version)
"""
fib_reduce(x) = first(
    reduce(1:x, init = (1, 0)) do (a, b), _
        (a + b, a)
    end
)


@show fib(11)

@show fib_recur(11)
@show fib_recur(111)

@show fib_loop(11)
@show fib_loop(111)

@show fib_reduce(11)
@show fib_reduce(111)
