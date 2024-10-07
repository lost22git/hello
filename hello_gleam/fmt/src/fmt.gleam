import gleam/string
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/iterator
import gleam/int

pub fn fmt(template: String, env_vars: List(Dynamic)) -> String {
  let #(fragments_from_to, exprs_from_to) = parse(template)
  case list.length(exprs_from_to) == list.length(env_vars) {
    False -> panic as "env vars and `{..}` (in template) must have same length"
    True -> {
      let fragments =
        fragments_from_to
        |> iterator.from_list()
        |> iterator.map(get_template_slice(template, _))
        |> iterator.map(string.replace(_, "{{", "{"))
        |> iterator.map(string.replace(_, "}}", "}"))
        |> iterator.to_list()

      let exprs =
        exprs_from_to
        |> list.map(get_template_slice(template, _))

      exprs
      |> eval(with: env_vars)
      |> process(with: fragments)
    }
  }
}

fn get_template_slice(template: String, from_to: #(Int, Int)) -> String {
  let #(from, to) = from_to
  case from > to {
    True -> ""
    False ->
      template
      |> string.slice(from, to - from + 1)
  }
}

type ParseRecord {
  ParseRecord(
    fragments: List(#(Int, Int)),
    exprs: List(#(Int, Int)),
    in_expr: Bool,
    from: Int,
    to: Int,
  )
}

fn parse(template: String) -> #(List(#(Int, Int)), List(#(Int, Int))) {
  let record =
    template
    |> string.to_graphemes()
    |> list.fold(ParseRecord([], [], False, 0, 0), fn(acc, it) {
      case it {
        "{" -> {
          case acc.in_expr, acc.to == acc.from + 1 {
            True, True -> {
              // {{
              let assert Ok(last_fragment) =
                acc.fragments
                |> list.last()
              ParseRecord(
                ..acc,
                fragments: acc.fragments
                |> list.take(list.length(acc.fragments) - 1),
                in_expr: False,
                from: last_fragment.0,
                to: acc.to
                + 1,
              )
            }
            True, False -> {
              // { .. { 
              panic as "`{` can not in `{ .. }`"
            }
            False, _ -> {
              // ..{
              ParseRecord(
                ..acc,
                fragments: acc.fragments
                |> list.append([#(acc.from, acc.to - 1)]),
                in_expr: True,
                from: acc.to,
                to: acc.to
                + 1,
              )
            }
          }
        }
        "}" -> {
          case acc.in_expr {
            True -> {
              // { .. }
              ParseRecord(
                ..acc,
                exprs: acc.exprs
                |> list.append([#(acc.from + 1, acc.to - 1)]),
                in_expr: False,
                from: acc.to
                + 1,
                to: acc.to
                + 1,
              )
            }
            False -> {
              // ..}
              ParseRecord(..acc, to: acc.to + 1)
            }
          }
        }
        _ -> {
          ParseRecord(..acc, to: acc.to + 1)
        }
      }
    })

  case record.in_expr {
    True -> {
      panic as "`{` not be pairred"
    }
    False -> {
      #(
        record.fragments
          |> list.append([#(record.from, record.to - 1)]),
        record.exprs,
      )
    }
  }
}

fn eval(exprs: List(String), with env_vars: List(Dynamic)) -> List(String) {
  exprs
  |> iterator.from_list()
  |> iterator.index()
  |> iterator.map(fn(it) {
    let #(expr, i) = it
    let index_and_format = string.split(expr, ":")

    // index
    let assert Ok(index_str) =
      index_and_format
      |> list.at(0)

    // get value from env_vars by index
    let assert Ok(index) = case index_str {
      "" -> Ok(i)
      _ -> int.parse(index_str)
    }
    let assert Ok(value) =
      env_vars
      |> list.at(index)
    let value_str =
      value
      |> string.inspect

    // format
    case
      index_and_format
      |> list.at(1)
    {
      Ok(format) -> {
        // parse format
        let #(pad_str, align, width) = parse_format(format)

        // format value
        case align {
          "^" -> {
            let assert Ok(pad_left_width) =
              value_str
              |> string.length()
              |> int.add(width)
              |> int.divide(2)
            value_str
            |> string.pad_left(pad_left_width, pad_str)
            |> string.pad_right(width, pad_str)
          }
          ">" ->
            value_str
            |> string.pad_left(width, pad_str)
          "<" ->
            value_str
            |> string.pad_right(width, pad_str)
          _ -> panic as "invalid format"
        }
      }
      _ -> {
        value_str
      }
    }
  })
  |> iterator.to_list()
}

fn parse_format(format: String) -> #(String, String, Int) {
  case string.slice(format, 1, 1) {
    // {:^10}
    "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> {
      let pad_str = " "
      let align =
        format
        |> string.slice(0, 1)
      let assert Ok(width) =
        format
        |> string.slice(1, string.length(format) - 1)
        |> int.parse()

      #(pad_str, align, width)
    }
    // {:-^10}
    "^" | "<" | ">" -> {
      let pad_str =
        format
        |> string.slice(0, 1)
      let align =
        format
        |> string.slice(1, 1)
      let assert Ok(width) =
        format
        |> string.slice(2, string.length(format) - 2)
        |> int.parse()

      #(pad_str, align, width)
    }
    _ -> panic as "invalid format"
  }
}

fn process(values: List(String), with fragments: List(String)) -> String {
  [fragments, values]
  |> list.interleave()
  |> string.join("")
}
