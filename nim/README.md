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

