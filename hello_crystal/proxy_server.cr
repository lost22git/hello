require "http/client"
require "http/server"
require "uri"

server = HTTP::Server.new([
  HTTP::ErrorHandler.new,
  HTTP::LogHandler.new,
  HTTP::CompressHandler.new,
]) do |context|
  context.request.headers["host"] = "httpbin.org"
  site = URI.parse("https://httpbin.org")
  puts "Do request: [#{context.request.method.upcase}] #{site}#{context.request.resource}"
  HTTP::Client.new(site) do |client|
    client.exec(context.request) do |response|
      response.to_io(context.response)
      context.response.flush
    end
  end
end

address = server.bind_tcp 8000
puts "Proxy server is listening on http://#{address}"
server.listen
