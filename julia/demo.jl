#!/usr/bin/env julia

hello(name) = println("Hello, $(name).")
hello(name, times) = for i in 1:times
    println("Hello, $(name)!")
end

flip(f) = (xs...) -> f(Iterators.reverse(xs)...)

olleh = flip(hello)

olleh("Julia")
olleh(3, "Julia")
