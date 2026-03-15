#!/usr/bin/env julia

struct FibIterator
    n::Int
end

Base.iterate(it::FibIterator, state = (curr = 1, prev = 0, count = 0)) = begin
    if state.count == it.n
        nothing
    else
        (state.curr, (curr = state.curr + state.prev, prev = state.curr, count = state.count + 1))
    end
end

Base.length(it::FibIterator) = it.n

for i in FibIterator(10)
    println(i)
end

FibIterator(10) |> collect |> show
