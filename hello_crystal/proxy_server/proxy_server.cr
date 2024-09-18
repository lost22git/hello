require "http/client"
require "http/server"
require "uri"
require "log"
require "colorize"

Log.setup do |c|
  console = Log::IOBackend.new io: STDOUT, dispatcher: Log::DispatchMode::Sync, formatter: MyLogFmt.new
  c.bind "*", :debug, console
end

class MyLogFmt
  include Log::Formatter

  def initialize
  end

  def format(entry : Log::Entry, io : IO)
    severity_color = case entry.severity
                     when .error?, .fatal? then :red
                     when .warn?           then :yellow
                     when .info?           then :green
                     when .debug?          then :blue
                     else                       :default
                     end

    io << " ["
    io << entry.severity.to_s[0].upcase.colorize.mode(:bold).fore(severity_color)
    io << "] "
    entry.timestamp.to_s(io, "%Y-%m-%d %H:%M:%S")
    io << " - "
    io << entry.message
    if !entry.data.empty?
      io << " { "
      entry.data.to_s(io)
      io << " } "
    end
  end
end

server = HTTP::Server.new([
  HTTP::ErrorHandler.new,
  HTTP::LogHandler.new,
  HTTP::CompressHandler.new,
]) do |context|
  target = URI.parse(context.request.query_params["target"])
  context.request.headers["host"] = target.host.not_nil!
  Log.info &.emit("Fetching", method: context.request.method.upcase, target: target.to_s)
  context.request.path = target.path
  context.request.query = target.query
  HTTP::Client.new(target.host.not_nil!, target.port) do |client|
    client.exec(context.request) do |response|
      context.response.status = response.status
      context.response.headers.merge!(response.headers)
      IO.copy(response.body_io, context.response.output)
      context.response.flush
    end
  end
end

address = server.bind_tcp 8000
Log.info { "Proxy server is listening on http://#{address}" }
server.listen
