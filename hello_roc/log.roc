#!/usr/bin/env -S roc dev
app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout
import pf.Utc
import pf.Env

###############
## Date Time ##
###############

sPerMin = 60
sPerHour = 60 * sPerMin
sPerDay = 24 * sPerHour
msPerS = 1000
nsPerMs = 1_000_000
nsPerS = nsPerMs * msPerS

daysPer400Years = 365 * 400 + 97
daysPer100Years = 365 * 100 + 24
daysPer4Years = 365 * 4 + 1

daysPerMonth = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
daysPerMonthLeap = [0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

## check year if leap year or not
isLeapYear : U16 -> Bool
isLeapYear = \year -> (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)

## convert days to record {days, year, month, day, dayOfYear}
daysToYMD : Int _ -> { days : Int _, year : U16, month : U8, day : U8, dayOfYear : U16 }
daysToYMD = \daysTotal ->
    num400 = daysTotal // daysPer400Years
    daysRest = daysTotal - num400 * daysPer400Years

    num100 = daysRest // daysPer100Years |> \x -> if Num.compare x 4 == EQ then 3 else x
    daysRest2 = daysRest - num100 * daysPer100Years

    num4 = daysRest2 // daysPer4Years
    daysRest3 = daysRest2 - num4 * daysPer4Years

    numyears = daysRest3 // 365 |> \x -> if Num.compare x 4 == EQ then 3 else x
    daysRest4 = daysRest3 - numyears * 365

    year = num400 * 400 + num100 * 100 + num4 * 4 + numyears + 1970
    dayOfYear = daysRest4 + 1

    { days, month } =
        List.range { start: At 1, end: At 12 }
        |> List.walkUntil
            {
                days: daysRest4,
                month: 1,
                dpm: if (Num.compare numyears 3 == EQ) && ((Num.compare num100 3 == EQ) || !(Num.compare num4 24 == EQ)) then daysPerMonthLeap else daysPerMonth,
            }
            \state, monthIndex ->
                when List.get state.dpm monthIndex is
                    Err _ -> crash "Unreachable branch: failed to get month days by month index: $(Num.toStr monthIndex)"
                    Ok d -> if d > state.days then Break state else Continue { state & days: (state.days - d), month: state.month + 1 }

    { days: daysTotal, year: Num.toU16 year, month: Num.toU8 month, day: Num.toU8 days, dayOfYear: Num.toU16 dayOfYear }

## convert seconds to record {seconds, hour, minute, second}
secondsToHMS : Int _ -> { seconds : Int _, hour : U8, minute : U8, second : U8 }
secondsToHMS = \seconds -> {
    seconds: seconds,
    hour: (seconds // sPerHour) |> Num.toU8,
    minute: (seconds % sPerHour // sPerMin) |> Num.toU8,
    second: (seconds % sPerMin) |> Num.toU8,
}

## convert unix timestamp (seconds) to days
unixToDays = \unixS -> unixS // sPerDay

## convert unix timestamp (seconds) to seconds of day
unixToDaySeconds = \unixS -> unixS % sPerDay

## convert utc to RFC3339 format string
utcToRFC3339 : Utc.Utc -> Str
utcToRFC3339 = \utc ->
    unixS = utc |> Utc.toMillisSinceEpoch |> \x -> x // msPerS
    { year, month, day } = unixS |> unixToDays |> daysToYMD
    { hour, minute, second } = unixS |> unixToDaySeconds |> secondsToHMS
    date = "$(Num.toStr year)-$(Num.toStr month)-$(Num.toStr day)"
    time = "$(Num.toStr hour):$(Num.toStr minute):$(Num.toStr second)"
    "$(date)T$(time)Z"

###############
## Ansi Code ##
###############

csi = "\u(001b)["
csiReset = "$(csi)m"

AnsiStyle : [
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

## convert ansi style to int
ansiStyleToInt : AnsiStyle -> U8
ansiStyleToInt = \style ->
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

## format input string with ansi styles
ansiStr : Str, List AnsiStyle -> Str
ansiStr = \str, styles ->
    joinStyles = styles |> List.map ansiStyleToInt |> List.map Num.toStr |> Str.joinWith ";"
    "$(csi)$(joinStyles)m$(str)$(csiReset)"

#########
## Log ##
#########

Level : [
    Debug,
    Info,
    Warn,
    Error,
]

## convert `Level` to `Int`
levelToInt : Level -> I8
levelToInt = \lv ->
    when lv is
        Debug -> 0
        Info -> 10
        Warn -> 20
        Error -> 30

## convert plain `Str` to `Level`
levelFromStr : Str -> Result Level [LevelFromStrErr Str]
levelFromStr = \s ->
    when s is
        "DBG" -> Ok Debug
        "INF" -> Ok Info
        "WAR" -> Ok Warn
        "ERR" -> Ok Error
        _ -> Err (LevelFromStrErr s)

## convert `Level` to plain `Str`
levelToStr : Level -> Str
levelToStr = \lv ->
    when lv is
        Debug -> "DBG"
        Info -> "INF"
        Warn -> "WAR"
        Error -> "ERR"

## get `AnsiStyle`s of given `Level`
levelToAnsiStyles : Level -> List AnsiStyle
levelToAnsiStyles = \lv ->
    when lv is
        Debug -> [Bold, FgBlue]
        Info -> [Bold, FgGreen]
        Warn -> [Bold, FgYellow]
        Error -> [Bold, FgRed]

## convert `Level` to ansi `Str`
levelToAnsiStr : Level -> Str
levelToAnsiStr = \lv -> ansiStr (levelToStr lv) (levelToAnsiStyles lv)

## read `Level` from environment variable `ROC_LOG_LEVEL`
levelFromEnv : {} -> Task Level _
levelFromEnv = \_ ->
    Env.var "ROC_LOG_LEVEL"
        |> Task.await \s -> (levelFromStr s |> Task.fromResult)
        |> Task.onErr! \_ -> Task.ok Info

LogRecord : {
    time : Utc.Utc,
    level : Level,
    msg : Str,
}

## convert `LogRecord` to `Str`
logRecordToStr : LogRecord -> Str
logRecordToStr = \r -> "$(r.time |> utcToRFC3339) $(levelToAnsiStr r.level) - $(r.msg)"

## log filter for the given `LogRecord`
logFilter : LogRecord -> Task Bool _
logFilter = \r ->
    minLv = levelFromEnv! {}
    Task.ok ((levelToInt minLv) <= (levelToInt r.level))

## do log with the given `LogRecord`
logRecord : LogRecord -> Task {} _
logRecord = \r ->
    if logFilter! r then
        Stdout.line! (logRecordToStr r)
    else
        Task.ok {}

## do log with the given level and message
log : Level, Str -> Task {} _
log = \lv, msg ->
    now = Utc.now! {}
    logRecord { time: now, level: lv, msg: msg }

## log debug message
debug : Str -> Task {} _
debug = \msg -> log Debug msg

## log info message
info : Str -> Task {} _
info = \msg -> log Info msg

## log warn message
warn : Str -> Task {} _
warn = \msg -> log Warn msg

## log error message
error : Str -> Task {} _
error = \msg -> log Error msg

##########
## Main ##
##########
main =
    debug! "this is a msg"
    info! "this is a msg"
    warn! "this is a msg"
    error! "this is a msg"

##########
## Test ##
##########
expect isLeapYear 2000
expect isLeapYear 2008
expect isLeapYear 2020
expect isLeapYear 2024
expect !(isLeapYear 2100)
