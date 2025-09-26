#!/usr/bin/env crystal

require "socket"
require "uuid"
require "colorize"

TCPServer.open("localhost", 9933) do |server|
  puts "Listening on #{server.local_address}"
  while client = server.accept?
    spawn handle_client(client)
  end
end

def handle_client(client)
  id = UUID.v4
  puts "#{id} [connected]".colorize.fore(:yellow).mode(:bold)
  while msg = client.gets
    if msg == "error"
      raise "got an error"
    end
    puts "#{id} #{msg}".colorize.fore(:green)
    client.puts msg
  end
  puts "#{id} [disconnected]".colorize.fore(:yellow).mode(:bold)
rescue ex
  puts "#{id} [error] #{ex}".colorize.fore(:red).mode(:bold)
ensure
  client.close
  puts "#{id} [closed]".colorize.fore(:yellow).mode(:bold)
end
