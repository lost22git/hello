#!/usr/bin/env julia

# macro define

macro unless(cond, body)
    return quote
        if !$cond
            $body
        end
    end
end

@unless 1 + 1 == 3 begin
    println("unless body calling.")
end

# macro expand

@macroexpand @unless 1 + 1 == 3 begin
    println("unless body calling.")
end

# esc: escape scope

macro escape_scope(expr)
    return esc(
        quote
            x = $expr
        end
    )
end

A = rand(Int32, 10)
@escape_scope A
println(x)
