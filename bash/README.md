# Bash Lang Learning

## Tips

### stdin

- Bad (pipe)
The shell process **will not** exit until the exec process exits.
```sh
echo 1 | exec ...
```

- Good (heredoc)
The shell process **will** exit due to being replaced by the exec process.
```sh
exec ... <<EOF
1
EOF
```

