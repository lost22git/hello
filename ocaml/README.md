# OCaml Lang Learning

## Resources

- https://cs3110.github.io/textbook/cover.html

## Summary

- Impure and Currying
- Type Erasure: type info only exists in compile-time, we can't get type info during runtime.
- Type Inference: powerful, almost no type annotations required
- ADT / GADT
- No Pointer Type
- Stackless Allocation: built-in types (Array,String,Bytes,Buffer,Tuple,Hashtbl,Map,Record...) are allocated on the heap.
  Allocate memory in units of one machine word (one word bytes = Sys.word_size / 8), even if the demand is less than one machine word. (e.g. the bool field in the record, ref bool...)
- Tracing GC
- Error Models: tag union / try-with / effect handler
- Native Code
- Fast Compiler
