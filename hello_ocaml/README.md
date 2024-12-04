# [OCaml](https://ocaml.org/docs/installing-ocaml) Lang Learning

## Installation

https://ocaml.org/docs/installing-ocaml

```shell
nix-env -iA nixpkgs.opam

opam init -y

eval $(opam env --switch=default)

cat <<'EOF' >> ~/.bashrc

# ocaml opam
eval $(opam env --switch=default)

EOF

opam install ocaml-lsp-server odoc ocamlformat
```

## Resources

- https://ocaml.org/docs/modules
- https://ocaml.org/docs/metaprogramming
- https://ocaml.org/docs/memory-representation
- https://ocaml.org/docs/garbage-collector

## Summary
