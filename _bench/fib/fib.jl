#!/usr/bin/env julia

fib(x::UInt64) = x > 1 ? fib(x - 1) + fib(x - 2) : 1

n::UInt64 = 40
println(fib(n))
