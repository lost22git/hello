# Nim Lang Learning

## Resources

- https://nimprogramming.com/tutorials/all
- https://internet-of-tomohiro.netlify.app/nim/faq.en
- https://nim-lang.org/araq/
- https://forum.nim-lang.org/t/10080#66502 (streaming IO still an issue)

## Nim Script

| function       | nims                                   | nim                               |
|----------------|----------------------------------------|-----------------------------------|
| env            | system/nimscript.getEnv()              | std/envvar.getEnv()               |
| cmdline param  | system/nimscript.paramCount/paramStr() | std/cmdline.paramCount/paramStr() |
| fitler/toSeq   | std/sugar.collect()                    | std/sequtils                      |
| get cmd result | system.gorge()/gorgeEx()               | std/osproc                        |
| time           | get from shell                         | std/time                          |

> NOTE:
It is **not recommended** to use Nim script for heavy task, because the memory overhead of the VM is large, at least on Windows.

__Consider using Lua ;)__

## Summary

PROS:
- Python&Pascal-like syntax
- [Great manual](https://nim-lang.org/docs/manual.html)
- No GC: [ARC/ORC](https://nim-lang.org/blog/2020/10/15/introduction-to-arc-orc-in-nim.html) [MM](https://nim-lang.github.io/Nim/mm.html)
- [Destructors and Move Semantics](https://nim-lang.org/docs/destructors.html)
- Passing variables 
    - by-value (normal/ptr/ref/sink param)
    - by-address (var param/returnvalue, lent returnvalue)
    - [ by-auto ](https://forum.nim-lang.org/t/9663#63579) (for immutable variables)
    - [ {.bycopy.}/{.byref.}](https://nim-lang.org/docs/manual.html#foreign-function-interface-bycopy-pragma)(for FFI)
- [ Procedure ](https://nim-lang.org/docs/manual.html#procedures): [ varargs ](https://nim-lang.org/docs/manual.html#types-varargs), named arg, param default value
- [UFCS](https://nim-lang.github.io/Nim/manual.html#procedures-method-call-syntax) (Uniform Fcuntion Call Style)
- Operator overloading: [Subscript operator overloading](https://nim-lang.github.io/Nim/manual.html#procedures-overloading-of-the-subscript-operator)
- Type: Value type, [ Reference type, Pointer type ](https://nim-lang.org/docs/manual.html#types-reference-and-pointer-types)
- [ Type default value ](https://nim-lang.org/docs/manual.html#statements-and-expressions-var-statement), [ Object field default value ](https://nim-lang.org/docs/manual.html#types-default-values-for-object-fields)
- [ Tuple unpacking ](https://nim-lang.org/docs/manual.html#statements-and-expressions-tuple-unpacking)
- FP & OOP
- Powerful metaprogramming: [generic](https://nim-lang.github.io/Nim/manual.html#generics), [template](https://nim-lang.github.io/Nim/manual.html#templates), [macro](https://nim-lang.org/docs/macros.html#the-ast-in-nim) (which allows direct manipulation of the AST)
- [ Conditional Compilation, Compile-Time Evaluation ](https://nim-lang.org/docs/nimc.html#compiler-usage-compileminustime-symbols)
- Various backends: C/C++. Javascript
- [ FFI ](https://nim-lang.org/docs/manual.html#foreign-function-interface-importc-pragma), [ Embed C code ](https://nim-lang.org/docs/manual.html#implementation-specific-pragmas-emit-pragma), [ Embed ASM code ](https://nim-lang.org/docs/manual.html#statements-and-expressions-assembler-statement)

CONS:
- [ Poor API Documentation ](https://nim-lang.org/docs/lib.html)
- Poor IO Abstraction
- No crypto module in stdlib, needs OpenSSL (`-d:ssl`)
- Needs better [Sum Type](https://github.com/nim-lang/RFCs/issues/548)
- Needs better windows optimization (binary size, memory overhead)


### Checklist

- Paradigm
    - [x] Procedural Programming
    - [ ] Functional Programming
    - [x] Object Oriented Programming
- Types
    - [x] Static Type
    - [x] Type auto inference
    - [x] Value Types
    - [x] Reference Types
    - [x] Pointer Types
    - [ ] Sum Types
    - [x] Range Type
    - [ ] Slice Type
    - [ ] Matrix Type
    - [ ] BitSet Type
    - [ ] BitField Type
    - [ ] BitArray Type
    - [ ] Record Type
    - [ ] Arbitrary bit-width Integers
    - [ ] Endian Specific Number Type
    - [x] Memory Layout Control
- String
    - [x] Raw String
    - [x] Multiline String
        - [x] Absolute Indentation 
        - [ ] Relative Indentation 
    - [x] String interpolation
    - [x] Null-Terminated String (cstring)
    - [x] Length-Based String
    - [ ] Immutable String
    - [x] Dynamic Length String
- Variables 
    - [ ] Static Local Variables
- Assignment
    - [ ] Redefine same name variables
    - [x] Mutate variables
    - [x] Assign by-copy
    - [x] Assign by-move
    - [x] Assign by-ref
    - [x] Assign by-auto
    - [x] Multiple values assignment
    - [x] Tuple unpacking
    - [ ] Struct unpacking
    - [ ] Record unpacking
- Functions
    - [x] UFCS (Uniform Function Call Style)
    - [x] Varargs
    - [x] Named arguments
    - [ ] Alias parameters
    - [x] Parameters default value
    - [ ] Multiple return values
    - [x] Named return values
    - [ ] Partial Application
- Operators
    - [x] Operators Overloading
    - [x] Wrapping Operators
    - [ ] Saturating Operators
    - [ ] Pipe Operator
    - [x] Range Operator
- [x] Control Flow Expressions
- [x] Closures
- Null Handles
    - [x] null/nil value
    - [x] Optional/Maybe Types
    - [ ] Nil Union Types
- Error Handles
    - [x] try-catch throw/raise
    - [ ] Result Type
    - [ ] Multiple return values
    - [ ] Error Union Types
- [ ] Pattern Matching
- [x] Generic Types (Parametric Polymorphism)
- [x] SubType Polymorphism
- [x] Usingnamespace
- [x] Meta Programming
    - [x] Macros
    - [ ] Comptime
- [ ] Custom Memory Allocators
    - [ ] Global
    - [ ] Contextual
- Memory Managements
    - [x] Manual 
    - [x] Single Ownership & Destructor
    - [x] ReferenceCount
    - [x] TracingGC
    - [ ] Depends on Application Platform
- FFI
    - [x] C
    - [x] C++
    - [x] Embed C code
    - [x] Embed C++ code
    - [x] Embed ASM code
- [x] Conditional Compilation
- Compilation Backends
    - [x] C
    - [x] C++
    - [x] Javascript
    - [x] WASM
    - [ ] LLVM
    - [ ] JVM
    - [ ] BEAM
- [ ] IO Abstraction
- [ ] Without LibSSL or OpenSSL required
- [x] Multithreading
- [ ] Coroutines
    - [ ] Stackful
    - [ ] Stackless
    - [ ] Preemptive

