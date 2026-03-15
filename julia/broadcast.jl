#!/usr/bin/env julia

eq(x, xs...) = reduce(xs) do acc, e
    x == e ? true : Reduced(false)
end

pow2(x) = x * x

eq(
    map(pow2, [1:10;]),
    # f.()
    pow2.([1:10;]),
    # broadcast
    broadcast(pow2, [1:10;]),
    # .|> f
    ([1:10;] .|> pow2)
)

eq(
    (@. [1:10;] * 2 + 10),
    [1:10;] .* 2 .+ 10
)
