# SBCL Lang Learning

https://www.sbcl.org/manual/

## Resources

- [cl-cookbook](https://lispcookbook.github.io/cl-cookbook/)
- [Learn Lisp The Hard Way](https://llthw.common-lisp.dev)
- [Common Lisp Language Reference](https://lisp-docs.github.io/cl-language-reference)
- [Awesome CL](https://github.com/CodyReichert/awesome-cl)

## Tools

### [quick-lisp](https://www.quicklisp.org/beta/): library manager

- Install
```sh
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp <<EOF
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
EOF
rm quicklisp.lisp
```

### [asdf](https://asdf.common-lisp.dev): build tool

## Build an executable binary

- [sb-ext:save-lisp-and-die](https://www.sbcl.org/manual/#Function-sb_002dext-save_002dlisp_002dand_002ddie)

```sh
sbcl --load app.lisp <<EOF
(sb-ext:save-lisp-and-die "app" 
                          :toplevel #'main
                          :executable t
                          :compression 22)
EOF
```


