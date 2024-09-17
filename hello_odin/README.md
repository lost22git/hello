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
- [ Pattern matching ](https://odin-lang.org/docs/overview/#switch-statement)
- [Procedures](https://odin-lang.org/docs/overview/#switch-statement)
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
