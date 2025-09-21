# Flix Lang Learning

https://flix.dev

## Installation

```sh
mkdir ~/.local/lib/flix
wget -O ~/.local/lib/flix/flix.jar https://github.com/flix/flix/releases/latest/download/flix.jar
cat <<EOF > ~/.local/bin/flix
#!/usr/bin/env bash
java -jar ~/.local/lib/flix/flix.jar "$@"
EOF
chmod +x ~/.local/bin/flix
```

## Try it

- create a project
```sh
mkdir demo && cd demo
flix init
```

- check the project
```sh
flix check
```

- run the project
```sh
flix run
```

- run the project's tests
```sh
flix test
```

- build the project
```sh
flix build-jar
# or
flix build-fatjar

```
