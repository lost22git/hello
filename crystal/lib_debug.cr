#!/usr/bin/env -S DEBUG=1 crun

# ---
# debug:
#   github: Sija/debug.cr
# ...

require "debug"

# 开启 debug 输出
Debug.enabled = true

Debug.configure do |settings|
  settings.max_path_length = 100

  settings.colors[:expression] = :magenta
  settings.colors[:value] = :yellow
end

Debug::Logger.configure do |settings|
  settings.progname = ""

  settings.show_severity = true
  settings.show_datetime = false
  settings.show_progname = true

  settings.colors[:datetime] = :dark_gray
  settings.colors[:progname] = :light_blue

  settings.severity_colors[:debug] = :cyan
  settings.severity_colors[:info] = :white
end

# ------ debug! ------------------------

debug! 10**3

static_string = StaticArray["你好", "世界"]
debug! static_string
debug! static_string.size

range = 1..10
debug! range

tuple = {1, 2, 3}
debug! tuple
debug! tuple.size

named_tuple = {first: "foo", last: "bar"}
debug! named_tuple
debug! named_tuple["first"]
debug! named_tuple["last"]

string = "你好，世界"
debug! string
debug! string.size
debug! string.bytesize

array = ["你好", "世界"]
debug! array
debug! array.size

hash = {"你好" => "世界"}
debug! hash
debug! hash.size

set = {"你好", "世界"}
debug! set
debug! set.size

@[Flags]
enum Color
  Red
  Green
  Blue
end

c = Color::Red | Color::Blue
debug! c
debug! c.red?
debug! c.blue?

record Point, x : Int32, y : Int32
p = Point.new x: 1, y: 2
debug! p
debug! p.x
debug! p.y
