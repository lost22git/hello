#!/usr/bin/env julia

fib(x) = x > 1 ? fib(x - 1) + fib(x - 2) : 1

n = 40
println(fib(n))
