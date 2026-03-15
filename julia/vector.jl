#!/usr/bin/env julia

# create
zeros(10)
ones(10)
fill(2, 10)
[1:10;]
[2 * i for i in 1:10]

v = Vector{Int}(undef, 10)
fill!(v, 3)

# add
[1:10;] + [1:10;]

# subtract
[1:10;] - [1:10;]

# scalar multiply
[1:10;] * 2

# scalar divide
[1:10;] / 2

# sub
[1:10;][2:4]

# slice
@view [1:10;][2:4]

# vcat
vcat([1:10;], [11:20;]) == append!([1:10;], [11:20;]) == [1:10;11:20;]

# hcat to matrix
hcat([1:10;], [11:20;])
transpose(hcat([1:10;], [11:20;]))
