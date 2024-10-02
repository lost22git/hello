#!/usr/bin/env -S roc dev
app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout
import pf.Utc
import pf.Env
import Lazy
import Time
import Ansi

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
levelToAnsiStyles : Level -> List Ansi.Style
levelToAnsiStyles = \lv ->
    when lv is
        Debug -> [Bold, FgBlue]
        Info -> [Bold, FgGreen]
        Warn -> [Bold, FgYellow]
        Error -> [Bold, FgRed]

## convert `Level` to ansi `Str`
levelToAnsiStr : Level -> Str
levelToAnsiStr = \lv -> Ansi.ansiStr (levelToStr lv) (levelToAnsiStyles lv)

## read `Level` from environment variable `ROC_LOG_LEVEL`
levelFromEnv : {} -> Task Level _
levelFromEnv = \_ ->
    Env.var "ROC_LOG_LEVEL"
        |> Task.await \s -> (levelFromStr s |> Task.fromResult)
        |> Task.onErr! \_ -> Task.ok Info

LogRecord : {
    time : Utc.Utc,
    level : Level,
    msg : Lazy.Lazy Str,
}

## convert `LogRecord` to `Str`
logRecordToStr : LogRecord -> Str
logRecordToStr = \r ->
    msg = r.msg |> Lazy.tryInit |> Lazy.get |> Result.withDefault ""
    "$(r.time |> Time.utcToRFC3339) $(levelToAnsiStr r.level) - $(msg)"

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
log : Level, Lazy.Lazy Str -> Task {} _
log = \lv, msg ->
    now = Utc.now! {}
    logRecord { time: now, level: lv, msg: msg }

## log debug message
debug : Str -> Task {} _
debug = \msg -> log Debug (Inited msg)

## log info message
info : Str -> Task {} _
info = \msg -> log Info (Inited msg)

## log warn message
warn : Str -> Task {} _
warn = \msg -> log Warn (Inited msg)

## log error message
error : Str -> Task {} _
error = \msg -> log Error (Inited msg)

## log debug message (lazy version)
debugz : ({} -> Str) -> Task {} _
debugz = \msg -> log Debug (UnInited msg)

## log info message (lazy version)
infoz : ({} -> Str) -> Task {} _
infoz = \msg -> log Info (UnInited msg)

## log warn message (lazy version)
warnz : ({} -> Str) -> Task {} _
warnz = \msg -> log Warn (UnInited msg)

## log error message (lazy version)
errorz : ({} -> Str) -> Task {} _
errorz = \msg -> log Error (UnInited msg)

##########
## Main ##
##########
main =
    debug! "this is a msg"
    info! "this is a msg"
    warn! "this is a msg"
    error! "this is a msg"

    debugz! \_ -> "this is a lazy msg"
    infoz! \_ -> "this is a lazy msg"
    warnz! \_ -> "this is a lazy msg"
    errorz! \_ -> "this is a lazy msg"

