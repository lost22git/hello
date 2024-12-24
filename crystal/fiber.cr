#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

__ "select event and call action"

ch = Channel(Int32).new

spawn do
  sleep 2.seconds
  ch.send 10
end

r = select
when x = ch.receive
  p "got value"
  x
when timeout 1.seconds
  p "timeout"
  0
end

p! r

__ "Fiber enqueue"

ch2 = Channel(Int32).new

fiber = Fiber.new "my-fiber" do
  sleep 1.seconds
  puts "[#{Fiber.current.name}] send 11"
  ch2.send 11
end

fiber.enqueue

puts "[#{Fiber.current.name}] receive: #{ch2.receive}"

__ "Fiber resume"

main_fiber = Fiber.current

fiber = Fiber.new "my-fiber2" do
  puts "[#{Fiber.current.name}] is running"
  sleep 1.seconds
  puts "[#{Fiber.current.name}] is end"

  main_fiber.resume if main_fiber.resumable?
end

fiber.resume
puts "[#{Fiber.current.name}] is end"
