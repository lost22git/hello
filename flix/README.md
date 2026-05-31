# Flix Lang Learning

https://flix.dev

## Installation

```sh
mkdir ~/.local/lib/flix
wget -O ~/.local/lib/flix/flix.jar https://github.com/flix/flix/releases/latest/download/flix.jar
cat <<'EOF' > ~/.local/bin/flix
#!/usr/bin/env bash
java -jar ~/.local/lib/flix/flix.jar "$@"
EOF
chmod +x ~/.local/bin/flix
```

## Try it

- REPL on Dev

```sh
flix repl
```

```sh
flix> :help
flix> :info println
```
