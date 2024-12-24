import gleeunit
import gleeunit/should
import fmt
import gleam/dynamic

pub fn main() {
  gleeunit.main()
}

pub fn fmt_test() {
  fmt.fmt("{2:->10} 🐖 {{{:^10}}} 💊 {0:-<10}", [
    dynamic.from("foo"),
    dynamic.from("bar"),
    dynamic.from([1, 2]),
  ])
  |> should.equal("----[1, 2] 🐖 {  \"bar\"   } 💊 \"foo\"-----")
}
