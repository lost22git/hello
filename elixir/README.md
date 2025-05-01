# Elixir Lang Learning

## Resource

## Summary


### CONS

- Pattern Matching for Keyword List

It requires same count and order.

```elixir
[a: a] = [a: 1, b: 2] # not matched 
a = [a: 1, b: 2][:a] # use this

[a: a, b: b] = [a: 1] # not matched
{ a, b } = { [a: 1][:a], [a: 1][:b] || 0 } # use this

[b: b, a: a] = [a: 1, b: 2] # not matched
```

- Function: must be defined inside of a Module

- Why functions like `length`, `elem`... in `Kernel` rather than themselves' module?

