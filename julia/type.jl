#!/usr/bin/env julia

sizeof(Bool) == 1
sizeof(Int32) == 4
# vector
sizeof(typeof([1, 2, 3])) == 3 * 8
# matrix
sizeof(typeof([1 2 3; 4 5 6;])) == 32
# tuple
sizeof(typeof((1, 2, 3))) == 3 * 8
# pair
sizeof(typeof("a" => 1)) == 16
# range
sizeof(typeof(1:10)) == 16
# dict
sizeof(typeof(Dict("a" => 1))) == 64

struct Book
    id::Int32
    name::String
    price::Float32
end

sizeof(Book) == (4 + 4) + 8 + (4 + 4)

struct Book2
    id::Int32
    price::Float32
    name::String
end

sizeof(Book2) == 4 + 4 + 8
