module [
    Style,
    ansiStr,
]

csi = "\u(001b)["
csiReset = "$(csi)m"

Style : [
    Bold,
    Dim,
    Italic,
    Underline,
    FgRed,
    FgGreen,
    FgYellow,
    FgBlue,
    BgRed,
    BgGreen,
    BgYellow,
    BgBlue,
]

## convert ansi [Style] to [U8]
styleToInt : Style -> U8
styleToInt = \style ->
    when style is
        Bold -> 1
        Dim -> 2
        Italic -> 3
        Underline -> 4
        FgRed -> 31
        FgGreen -> 32
        FgYellow -> 33
        FgBlue -> 34
        BgRed -> 41
        BgGreen -> 42
        BgYellow -> 43
        BgBlue -> 44

## format input [Str] with ansi [Style]s
ansiStr : Str, List Style -> Str
ansiStr = \str, styles ->
    joinStyles = styles |> List.map styleToInt |> List.map Num.toStr |> Str.joinWith ";"
    "$(csi)$(joinStyles)m$(str)$(csiReset)"

