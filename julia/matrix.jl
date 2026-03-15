#!/usr/bin/env julia

# Vector: 1 dimension Array

@show a = [1, 2]

# Matrix: 2 dimensions Array

@show b = [1 2; 3 4;]

@show a .+ b

@show a .* b

@show b + b

@show b * 2

# Matrix multiply
@show [1 2; 3 4] * [1 3; 2 4]

# Matrix transpose

@show b * transpose(b)

# Matrix inv

@show inv(b)

@show b * inv(b)

# determinant

@show det(b)

# adjugate
@show inv(b) * det(b)

# Matrix create
using LinearAlgebra

@show rand(3, 2)
@show zeros(3, 2)
@show ones(3, 2)
@show I(3)
