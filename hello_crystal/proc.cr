#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

__ "copy or ptr"

def copy(v : Int32)
  p! pointerof(v).address
  v = 10
end

def ptr(v : Pointer(Int32))
  p! v.address
  v.value = 20
end

value = 0
p! pointerof(value).address
p! value

copy value
p! value

ptr pointerof(value)
p! value

__ "splat params"

def splat_params(*v : String)
  p! v
end

def splat_params2(**v : String)
  p! v
end

splat_params "你好", "hello"
splat_params *{"你好", "hello"}

splat_params2 zh: "你好", en: "hello"
splat_params2 **{zh: "你好", en: "hello"}

__ "param default value"

def paramDefaultValue(a : String = "你好", *, b : String)
  p! "#{a}, #{b}"
end

paramDefaultValue b: "世界"
paramDefaultValue **{a: "hello", b: "world"}

__ "& block param"

def block_param(v : Int, &)
  v.times.each do |i|
    yield i
  end
end

def block_param2(v : Int, &action : Int32 ->)
  v.times.each &action
end

def block_param3(v : Int, &action : Int32 ->)
  v.times.each do |i|
    action.call i
  end
end

block_param 3 do |i|
  p i
end

block_param2 3 do |i|
  p i
end

block_param3 3 do |i|
  p i
end

__ "union type param convert to single type"

require "uri"

def fetch(uri : String | URI)
  p! typeof(uri) # => URI

  if uri.is_a? String
    uri = URI.parse(uri)
  end

  p! typeof(uri) # => URI
end

fetch "https://github.com"
