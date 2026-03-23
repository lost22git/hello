# SBCL Lang Learning

https://www.sbcl.org/manual/

## Resources

- [cl-cookbook](https://lispcookbook.github.io/cl-cookbook/)
- [Learn Lisp The Hard Way](https://llthw.common-lisp.dev)
- [Common Lisp Language Reference](https://lisp-docs.github.io/cl-language-reference)
- [Awesome CL](https://github.com/CodyReichert/awesome-cl)

## Tools

### [asdf](https://asdf.common-lisp.dev) - Build Tool

In generate, it is built-in CL distributions.

### [quick-lisp](https://www.quicklisp.org/beta/) - Dependency Management

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

### cl-project - Project Template

- Install

on REPL
```lisp
(ql:quickload "cl-project")
```

OR on SHELL
```sh
sbcl --non-interactive --eval '(ql:quickload :cl-project)'
```
