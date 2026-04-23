# Julia Lang Learning

## Resources

- https://viralinstruction.com/posts/threads/

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
- CommonLisp: multi-methods (run-time, args-value or args-type based dispatch)
- Clojure: multi-methods (run-time, args-value based dispatch)
- Elixir; multi-clauses (run-time, args-count or args-value based dispatch)
