app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout

# greet : Str -> Str
greet = \a ->
    "Hello $(a)"

# add : I32,I32 -> I32
add = \a, b ->
    a + b

# add2 : {a: I32, b: I32} -> I32
add2 = \{ a, b } ->
    a + b

main =
    greet ("Roc") |> Stdout.line!
    # string interpolation
    greet ("$(Num.toStr (add 7 7))") |> Stdout.line!
    greet ("$(Num.toStr (add2 { a: 7, b: 7 }))") |> Stdout.line!

    # dbg expression
    dbg add 7 7

    seasons = ["春", "夏", "秋", "冬"]
    dbg seasons

    dbg seasons |> List.keepIf \s -> s == "冬"

    # pattern matching
    seasonTagFromStr = \s ->
        when s is
            "春" -> Spring
            "夏" -> Summer
            "秋" -> Autumn
            "冬" -> Winter
            _ -> Others "无效季节" # tag with payload

    dbg seasons |> List.map seasonTagFromStr |> List.dropIf \s -> s == Others "无效季节"

    dbg seasons |> List.get 1 |> Result.try \s -> Ok (seasonTagFromStr s)

    dbg seasons |> List.get 5 |> Result.try \s -> Ok (seasonTagFromStr s)

    dbg seasons |> List.get 1 |> Result.map seasonTagFromStr

    dbg seasons |> List.get 5 |> Result.map seasonTagFromStr

    # tuple and record
    a = { id: 1, name: "roc" }
    b = { id: 1, name: "roc" }
    c = { b & id: 2 }
    dbg c

    # if-else if-else
    Stdout.line! (if (a == b) then "true" else "false")
    Stdout.line! (if (b == c) then "true" else "false")

