# Julia Lang Learning

## Tips

- List all symbols of a module

```julia
names(Base.Iterators, all=true)
```

- Search function

```julia
using InteractiveUtils
apropos("println")
```

- View documentation of a function

```julia
@doc println
```

## Method

Method: 
- a series of functions that have same name but different behaviors and args list
- multi-dispatch (run-time, args-count or args-type based dispatch)

Similar things in other languages:
- Clojure: multi-methods (run-time, args-value based dispatch)
- Elixir; multi-clauses (run-time, args-count or args-value based dispatch)
