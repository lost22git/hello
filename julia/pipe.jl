#!/usr/bin/env julia

collect(
    [1:10;]
        |> (x -> Iterators.filter(isodd, x))
        |> (x -> Iterators.map(y -> y + 10, x))
)

# Base.Fix = partial apply
collect(
    [1:10;]
        |> Base.Fix1(Iterators.filter, isodd)
        |> Base.Fix1(Iterators.map, Base.Fix1(+, 10))
)

# @chain
using Chain

collect(
    @chain [1:10;] begin
        Iterators.filter(isodd, _)
        Iterators.map(x -> x + 10, _)
    end
)
