# [OCaml](https://ocaml.org/docs/installing-ocaml) Lang Learning

## Installation

https://ocaml.org/docs/installing-ocaml

```shell
nix-env -iA nixpkgs.opam

opam init -y

eval $(opam env --switch=default)

cat <<'EOF' >> ~/.bashrc

# ocaml opam
eval $(opam env --switch=default)

EOF

opam install ocaml-lsp-server odoc ocamlformat
```

## Resources

- https://ocaml.org/docs/modules
- https://ocaml.org/docs/metaprogramming
- https://ocaml.org/docs/memory-representation
- https://ocaml.org/docs/garbage-collector

## Summary

- Paradigm
    - [x] Procedural Programming
    - [x] Functional Programming
    - [x] Object Oriented Programming
- Types
    - [x] Static Type
    - [x] Structural Type
    - [x] Type auto inference
    - [x] Value Types
    - [x] Reference Types
    - [ ] Pointer Types
    - [x] Sum Types
    - [x] Range Type
    - [ ] Slice Type
    - [ ] Matrix Type
    - [ ] BitSet Type
    - [ ] BitField Type
    - [ ] BitArray Type
    - [x] Record Type
    - [ ] Arbitrary bit-width Integers
    - [ ] Endian Specific Number Type
    - [ ] Memory Layout Control
- String
    - [x] Raw String
    - [x] Multiline String
        - [x] Absolute Indentation
        - [ ] Relative Indentation
    - [ ] String interpolation
    - [ ] Null-Terminated String (cstring)
    - [x] Length-Based String
    - [x] Immutable String
    - [ ] Dynamic Length String
- Variables 
    - [ ] Static Local Variables
- Assignment
    - [x] Redefine same name variables
    - [x] Mutate variables
    - [x] Assign by-copy
    - [ ] Assign by-move
    - [ ] Assign by-ref
    - [ ] Assign by-auto
    - [x] Multiple values assignment
    - [x] Tuple unpacking
    - [ ] Struct unpacking
    - [x] Record unpacking
- Functions
    - [ ] UFCS (Uniform Function Call Style)
    - [ ] Varargs
    - [x] Named arguments
    - [x] Alias parameters
    - [x] Parameters default value
    - [ ] Multiple return values
    - [ ] Named return values
    - [x] Partial Application
- Operators
    - [x] Operators Overloading
    - [ ] Wrapping Operators
    - [ ] Saturating Operators
    - [x] Pipe Operator
    - [ ] Range Operator
- [x] Control Flow Expressions
- [x] Closures
- Null Handles
    - [ ] null/nil value
    - [x] Optional/Maybe Types
    - [ ] Nil Union Types
- Error Handles
    - [ ] try-catch throw/raise
    - [x] Result Type
    - [ ] Multiple return values
    - [ ] Error Union Types
- [x] Pattern Matching
- [x] Generic Types (Parametric Polymorphism)
- [x] SubType Polymorphism
- [ ] Usingnamespace
- [x] Meta Programming
    - [ ] Macros
    - [ ] Comptime
    - [x] Preprocessors and PPXs
- [ ] Custom Memory Allocators
    - [ ] Global
    - [ ] Contextual
- Memory Managements
    - [ ] Manual 
    - [ ] Single Ownership & Destructor
    - [ ] ReferenceCount
    - [x] TracingGC
    - [ ] Depends on Application Platform
- FFI
    - [x] C
    - [ ] C++
    - [ ] Embed C code
    - [ ] Embed C++ code
    - [ ] Embed ASM code
- [x] Conditional Compilation
- Compilation Backends
    - [ ] C
    - [ ] C++
    - [ ] Javascript
    - [ ] WASM
    - [ ] LLVM
    - [ ] JVM
    - [ ] BEAM
    - [x] Byte Code
    - [x] Native Code
- [x] IO Abstraction
- [ ] Without LibSSL or OpenSSL required
- [x] Multithreading
- [ ] Coroutines
    - [ ] Stackful
    - [ ] Stackless
    - [ ] Preemptive
