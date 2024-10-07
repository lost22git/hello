# Zig Lang Learning

## Installation

https://github.com/ziglang/zig/wiki/Building-Zig-From-Source

```shell
git clone git@github.com:ziglang/zig.git && cd zig
mkdir build
cd build
cmake ..
make install
ln -sf $(pwd)/stage3/bin/zig /usr/local/bin/zig
zig fmt
```

## Resources

- https://codeberg.org/ziglings/exercises/
- https://github.com/ziglang/zig-spec
- https://alic.dev/blog/dense-enums
- [ATTACK of the KILLER FEATURES](https://www.youtube.com/watch?v=dEIsJPpCZYg)
- https://github.com/SuperAuguste/zig-design-patterns
- [Andrew Kelley Practical Data Oriented Design (DoD)](https://www.youtube.com/watch?v=IroPQ150F6c)

## Summary

### Checklist

- Paradigm
    - [x] Procedural Programming
    - [ ] Functional Programming
    - [ ] Object Oriented Programming
- Types
    - [x] Static Type
    - [ ] Structural Type
    - [x] Type auto inference
    - [x] Value Types
    - [ ] Reference Types
    - [x] Pointer Types
    - [x] Sum Types
    - [x] Range Type
    - [x] Slice Type
    - [ ] Matrix Type
    - [ ] BitSet Type
    - [ ] BitField Type
    - [ ] BitArray Type
    - [ ] Record Type
    - [x] Arbitrary bit-width Integers
    - [ ] Endian Specific Number Type
    - [x] Memory Layout Control
- String
    - [x] Raw String
    - [x] Multiline String
        - [ ] Absolute Indentation 
        - [x] Relative Indentation 
    - [ ] String interpolation
    - [x] Null-Terminated String (cstring)
    - [x] Length-Based String
    - [x] Immutable String
    - [ ] Dynamic Length String
- Variables 
    - [x] Static Local Variables
- Assignment
    - [ ] Redefine same name variables
    - [x] Mutate variables
    - [x] Assign by-copy
    - [ ] Assign by-move
    - [ ] Assign by-ref
    - [x] Assign by-auto
    - [ ] Multiple values assignment
    - [x] Tuple unpacking
    - [ ] Struct unpacking
    - [ ] Record unpacking
- Functions
    - [ ] UFCS (Uniform Function Call Style)
    - [x] Varargs
    - [ ] Named arguments
    - [ ] Alias parameters
    - [ ] Parameters default value
    - [ ] Multiple return values
    - [ ] Named return values
    - [ ] Partial Application
- Operators
    - [ ] Operators Overloading
    - [x] Wrapping Operators
    - [x] Saturating Operators
    - [ ] Pipe Operator
    - [x] Range Operator
- [x] Control Flow Expressions
- [ ] Closures
- Null Handles
    - [x] null/nil value
    - [ ] Optional/Maybe Types
    - [x] Nil Union Types
- Error Handles
    - [ ] try-catch throw/raise
    - [ ] Result Type
    - [ ] Multiple return values
    - [x] Error Union Types
- [ ] Pattern Matching
- [x] Generic Types (Parametric Polymorphism)
- [ ] SubType Polymorphism
- [x] Usingnamespace
- [x] Meta Programming
    - [ ] Macros
    - [x] Comptime
- [x] Custom Memory Allocators
    - [x] Global
    - [x] Contextual
- Memory Managements
    - [x] Manual 
    - [ ] Single Ownership & Destructor
    - [ ] ReferenceCount
    - [ ] TracingGC
    - [ ] Depends on Application Platform
- FFI
    - [x] C
    - [ ] C++
    - [x] Embed C code
    - [ ] Embed C++ code
    - [x] Embed ASM code
- [x] Conditional Compilation
- Compilation Backends
    - [ ] C
    - [ ] C++
    - [ ] Javascript
    - [x] WASM
    - [x] LLVM
    - [ ] JVM
    - [ ] BEAM
- [x] IO Abstraction
- [x] Without LibSSL or OpenSSL required
- [x] Multithreading
- [ ] Coroutines
    - [ ] Stackful
    - [ ] Stackless
    - [ ] Preemptive

