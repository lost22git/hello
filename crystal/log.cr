#!/usr/bin/env crystal

require "log"

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

# format: "time severity <source> (context) {data} - message \nraise: exception"
#
Log.define_formatter MyLogFormatter, "#{timestamp} #{severity}#{source(before: " <", after: ">")}#{context(before: " (", after: ")")}#{data(before: " {", after: "}")} - #{message} #{exception(before: "\nraise: ")}"

Log.setup do |c|
  console = Log::IOBackend.new io: STDOUT, formatter: MyLogFormatter, dispatcher: Log::DispatchMode::Sync
  c.bind "*", :trace, console
end

__ "global logger instance"

Log.trace { "a message for trace" }
Log.debug { "a message for debug" }
Log.info { "a message for info" }
Log.notice { "a message for notic" }
Log.warn { "a message for warn" }
Log.error { "a message for error" }
Log.fatal { "a message for fatal" }

__ "custom logger instance: main"

main_logger = Log.for "main"

main_logger.trace { "a message for trace" }
main_logger.debug { "a message for debug" }
main_logger.info { "a message for info" }
main_logger.notice { "a message for notic" }
main_logger.warn { "a message for warn" }
main_logger.error { "a message for error" }
main_logger.fatal { "a message for fatal" }

__ "log with context"

Log.with_context(txid: 100, rxid: 101) do
  main_logger.trace { "a message for trace" }
  main_logger.debug { "a message for debug" }
  main_logger.info { "a message for info" }
  main_logger.notice { "a message for notic" }
  main_logger.warn { "a message for warn" }
  main_logger.error { "a message for error" }
  main_logger.fatal { "a message for fatal" }
end

__ "log exception"

log_exception

def log_exception
  main_logger = Log.for "main"

  ex = Exception.new "an exception"

  main_logger.trace(exception: ex) { "a message for trace" }
  main_logger.debug(exception: ex) { "a message for debug" }
  main_logger.info(exception: ex) { "a message for info" }
  main_logger.notice(exception: ex) { "a message for notic" }
  main_logger.warn(exception: ex) { "a message for warn" }
  main_logger.error(exception: ex) { "a message for error" }
  main_logger.fatal(exception: ex) { "a message for fatal" }
end

__ "log with emit data"

main_logger.trace &.emit("a message", tag: ["dev"])
main_logger.debug &.emit("a message", tag: ["dev"])
main_logger.info &.emit("a message", tag: ["dev"])
main_logger.notice &.emit("a message", tag: ["dev"])
main_logger.warn &.emit("a message", tag: ["dev"])
main_logger.error &.emit("a message", tag: ["dev"])
main_logger.fatal &.emit("a message", tag: ["dev"])
