# Odin Lang Learning

## Installation 

https://odin-lang.org/docs/install

### Debian

- Install llvm

https://apt.llvm.org

```shell
apt install llvm-<version>-dev clang-<version> -y
ln -sf /usr/bin/clang-<version> /usr/bin/clang
```

- Clone Odin source and build it

```shell
git clone git@github.com:odin-lang/Odin.git && cd Odin
make release-native
```

## Resources

- [memory-allocation-strategies](https://www.gingerbill.org/series/memory-allocation-strategies/)
- [odin_review](https://graphitemaster.github.io/odin_review/)

## Summary

PROS:
- Go&Pascal-like syntax
- [Procedures](https://odin-lang.org/docs/overview/#procedures)
    - param passing by-value
    - multiple results
    - named result
    - named arg
    - param default value
-  [ endian specific number type ](https://odin-lang.org/docs/overview/#basic-types)
- Type:
    - [slice](https://odin-lang.org/docs/overview/#slices)
    - [soa type](https://odin-lang.org/docs/overview/#soa-data-types)
    - [ matrix type ](https://odin-lang.org/docs/overview/#matrix-type)
- [ Implicit Context System ](https://odin-lang.org/docs/overview/#implicit-context-system)
- polymorphis 
    - [subtype-polymorphism](https://odin-lang.org/docs/overview/#subtype-polymorphism)
    - [parametric-polymorphism](https://odin-lang.org/docs/overview/#parametric-polymorphism) (`$` is a little ugly)
- [ Conditional Compilation ](https://odin-lang.org/docs/overview/#conditional-compilation)


CONS:
- [API documentaion lacks examples](https://pkg.odin-lang.org/)
- no control flow expression
- [ no closure ](https://odin-lang.org/docs/faq/#does-odin-have-closures) 

### Checklist

- Paradigm
    - [x] Procedural Programming
    - [ ] Functional Programming
    - [ ] Object Oriented Programming
- Types
    - [x] Static Type
    - [x] Type auto inference
    - [x] Value Types
    - [x] Reference Types
    - [x] Pointer Types
    - [x] Sum Types
    - [x] Range Type
    - [x] Slice Type
    - [x] Matrix Type
    - [x] BitSet Type
    - [x] BitField Type
    - [ ] BitArray Type
    - [ ] Record Type
    - [ ] Arbitrary bit-width Integers
    - [x] Endian Specific Number Type
    - [x] Memory Layout Control
- String
    - [x] Raw String
    - [x] Multiline String
        - [x] Absolute Indentation 
        - [ ] Relative Indentation 
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
    - [x] Multiple values assignment
    - [ ] Tuple unpacking
    - [x] Struct unpacking
    - [ ] Record unpacking
- Functions
    - [ ] UFCS (Uniform Function Call Style)
    - [x] Varargs
    - [x] Named arguments
    - [ ] Alias parameters
    - [x] Parameters default value
    - [x] Multiple return values
    - [x] Named return values
    - [ ] Partial Application
- Operators
    - [ ] Operators Overloading
    - [ ] Wrapping Operators
    - [ ] Saturating Operators
    - [ ] Pipe Operator
    - [x] Range Operator
- [ ] Control Flow Expressions
- [ ] Closures
- Null Handles
    - [x] null/nil value
    - [x] Optional/Maybe Types
    - [ ] Nil Union Types
- Error Handles
    - [ ] try-catch throw/raise
    - [ ] Result Type
    - [x] Multiple return values
- [ ] Pattern Matching
- [x] Generic Types (Parametric Polymorphism)
- [x] SubType Polymorphism
- [x] Usingnamespace
- [ ] Meta Programming
    - [ ] Macros
    - [ ] Comptime
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
    - [ ] Embed C code
    - [ ] Embed C++ code
    - [ ] Embed ASM code
- [x] Conditional Compilation
- Compilation Backends
    - [ ] C
    - [ ] C++
    - [ ] Javascript
    - [ ] WASM
    - [x] LLVM
    - [ ] JVM
    - [ ] BEAM
- [x] IO Abstraction
- [ ] LibSSL or OpenSSL required
- [x] Multithreading
- [ ] Coroutines
    - [ ] Stackful
    - [ ] Stackless
    - [ ] Preemptive
