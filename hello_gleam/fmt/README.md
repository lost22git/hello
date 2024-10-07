# fmt

gleam string format

```gleam
import gleeunit
import gleeunit/should
import gleam/dynamic
import fmt
pub fn fmt_test() {
  fmt.fmt("{2:->10} 🐖 {{{:^10}}} 💊 {0:-<10}", [
    dynamic.from("foo"),
    dynamic.from("bar"),
    dynamic.from([1, 2]),
  ])
  |> should.equal("----[1, 2] 🐖 {  \"bar\"   } 💊 \"foo\"-----")
}
```

Further documentation can be found at <https://hexdocs.pm/fmt>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
