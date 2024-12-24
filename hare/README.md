# Hare Lang Learning

## Installation

https://harelang.org/documentation/install/bootstrap.html

Install [qbe](https://c9x.me/compile/code.html) (compiler backend)

```shell
git clone git://c9x.me/qbe.git && cd qbe
make && make install
```

Install scdoc

```shell
git clone https://git.sr.ht/~sircmpwn/scdoc && cd scdoc
make && make install
```

Install harec (compiler frontend)

```shell
git clone https://git.sr.ht/~sircmpwn/harec && cd harec
cp makefiles/linux.mk config.mk
make && make install
```

Install hare (stdlib and tools)

```shell
git clone https://git.sr.ht/~sircmpwn/hare && cd hare
cp makefiles/linux.mk config.mk
make && make install
```

## Resources

- [language spec](https://harelang.org/specification)

## NOTE

PROS: 
- [ proper error model ](https://harelang.org/tutorials/introduction#handling-errors)
- pattern matching
- [ function ](https://harelang.org/tutorials/introduction#functions-in-depth)
    - param default value
    - varargs
- [working-with-slice](https://harelang.org/tutorials/introduction#working-with-slices)

CONS:
- no multithreading
- no gerneric
- no map/table implementation in stdlib

