
# Fib

## Play

- Build

```sh
just clean
just build
```

- Bench

```sh
just bench_all
# or 
just bench_natives
# or 
just bench_scripts
# or 
just bench_bytecodes
```

## Note

Allowed optimization methods:

- For native - `-O3`
- For lisp - `fixnum`
- For dynamic type languages - `type-hints`
