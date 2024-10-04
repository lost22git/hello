#!/usr/bin/env -S roc dev
app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout
import pf.Utc
import pf.Env
import Lazy
import Time
import Ansi

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

Record : {
    time : Utc.Utc,
    level : Level,
    logger : Str,
    msg : Lazy.Lazy Str,
}

Err : [
    FilterErr Str,
    FormatterErr Str,
    WriterErr Str,
]

Logger : {
    name : Str,
    filter : Filter,
    writer : Writer,
    formatter : Formatter,
}

defaultLogger : Logger
defaultLogger = { name: "default", filter: defaultFilter, writer: defaultWriter, formatter: defaultFormatter }

Filter : Record -> Task Bool [FilterErr Str]
Writer : Str -> Task {} [WriterErr Str]
Formatter : Writer, Record -> Task {} [WriterErr Str, FormatterErr Str]

defaultFilter : Filter
defaultFilter = \record ->
    minLevel = levelFromEnv {} |> Task.mapErr! \e -> FilterErr "Failed to filter log record"
    Task.ok ((levelToInt minLevel) <= (levelToInt record.level))

defaultWriter : Writer
defaultWriter = \str -> Stdout.line str |> Task.mapErr \e -> WriterErr "Failed to write log record"

defaultFormatter : Formatter
defaultFormatter = \writer, record ->
    msg = record.msg |> Lazy.tryInit |> Lazy.get |> Result.withDefault ""
    writer "$(record.time |> Time.utcToRFC3339) $(levelToAnsiStr record.level) [$(record.logger)] - $(msg)"

## do log with the given `Record`
logRecord : Logger, Record -> Task {} [LogErr Err]
logRecord = \logger, record ->
    canLog = logger.filter record |> Task.mapErr! \e -> LogErr e
    if canLog then
        logger.writer |> logger.formatter record |> Task.mapErr \e -> LogErr e
    else
        Task.ok {}

## do log with the given level and message
log : Logger, Level, Lazy.Lazy Str -> Task {} _
log = \logger, level, msg ->
    now = Utc.now! {}
    logger |> logRecord { time: now, level: level, msg: msg, logger: logger.name }

## log debug message
debug : Logger, Str -> Task {} _
debug = \logger, msg -> logger |> log Debug (Inited msg)

## log info message
info : Logger, Str -> Task {} _
info = \logger, msg -> logger |> log Info (Inited msg)

## log warn message
warn : Logger, Str -> Task {} _
warn = \logger, msg -> logger |> log Warn (Inited msg)

## log error message
error : Logger, Str -> Task {} _
error = \logger, msg -> logger |> log Error (Inited msg)

## log debug message (lazy version)
debugz : Logger, ({} -> Str) -> Task {} _
debugz = \logger, msg -> logger |> log Debug (UnInited msg)

## log info message (lazy version)
infoz : Logger, ({} -> Str) -> Task {} _
infoz = \logger, msg -> logger |> log Info (UnInited msg)

## log warn message (lazy version)
warnz : Logger, ({} -> Str) -> Task {} _
warnz = \logger, msg -> logger |> log Warn (UnInited msg)

## log error message (lazy version)
errorz : Logger, ({} -> Str) -> Task {} _
errorz = \logger, msg -> logger |> log Error (UnInited msg)

##########
## Main ##
##########
main =
    run {} |> Task.onErr \e -> Stdout.line "error: $(Inspect.toStr e)"

run = \{} ->
    defaultLogger |> debug! "this is a msg"
    defaultLogger |> info! "this is a msg"
    defaultLogger |> warn! "this is a msg"
    defaultLogger |> error! "this is a msg"

    defaultLogger |> debugz! \_ -> "this is a lazy msg"
    defaultLogger |> infoz! \_ -> "this is a lazy msg"
    defaultLogger |> warnz! \_ -> "this is a lazy msg"
    defaultLogger |> errorz! \_ -> "this is a lazy msg"

# TODO:
# 1. how to use default logger when not given logger? (roc funcation not support default value of parameters)
# 2. how to solute the compilation error of error union?
# 3. add attributes (contextual or given by arguments) to log
