require "http/client"
require "http/server"
require "uri"

server = HTTP::Server.new([
  HTTP::ErrorHandler.new,
  HTTP::LogHandler.new,
  HTTP::CompressHandler.new,
]) do |context|
  target = URI.parse(context.request.query_params["target"])
  context.request.headers["host"] = target.host.not_nil!
  puts "Do request: [#{context.request.method.upcase}] #{target}"
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
puts "Proxy server is listening on http://#{address}"
server.listen
