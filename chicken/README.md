# ChickenScheme Lang Learning

https://call-cc.org
https://api.call-cc.org
https://wiki.call-cc.org/man/5/Modules
https://wiki.call-cc.org/eggref/5/csm
https://wiki.call-cc.org/man/5/Extensions
https://wiki.call-cc.org/man/5/Extension%20tools

## Tips

- Search symbols by pattern

```sh
chicken-install apropos
```

```ss
(import apropos)
(apropos "print")
```

- Search module by pattern

```ss
(apropos "scheme" #:find #:module)
```

- View documentation of a symbol in REPL

```ss
,d print
```
