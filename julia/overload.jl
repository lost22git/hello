#!/usr/bin/env julia

foo(x::Int) = println("[Int] $(x).")
foo(x::String) = println("[String] $(x).")

test(x) = foo(x)

x = something(tryparse(Int, ARGS[1]), ARGS[1])
test(x)
