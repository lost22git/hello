# ChickenScheme Lang Learning

## Resources

- https://wiki.call-cc.org/man/5/The%20User%27s%20Manual
- https://api.call-cc.org/5/doc/
- https://api.call-cc.org/5/doc/scheme


## Tips

- Install eggs

```sh
chicken-install <egg-name>
```

- Uninstall eggs

```sh
chicken-uninstall <egg-name>
```

- List installed eggs

```sh
chicken-status
```

- Run a script file

```sh
chicken-csi -s <script-file>
```

- Run a script file and call it's main function

```sh
chicken-csi -ss <script-file>
```

- Compile a binary

```sh
chicken-csc <file>

# static linking libchicken
chicken-csc -static <file>
```

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
