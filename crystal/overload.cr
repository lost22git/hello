#!/usr/bin/env crystal

# def foo(x)
#   puts "foo(x): #{x}."
# end
#
# # same as foo(x) and would override it
# def foo(y)
#   puts "foo(y): #{y}."
# end
#
# foo("Crystal")
# foo(11)
# foo(:a)

def foo(x : String)
  puts "foo(String): #{x}."
end

def foo(x : Int32)
  puts "foo(Int32): #{x}."
end

# priority: Int32|String = String|Int32 > String|Int > String|Int32||Float32 > Object > T > auto

def test(x : Object)
  puts "test(Object)"
  p! typeof(x) # String | Int32
  foo(x)       # compiled, dispatch in run-time
end

def test(x : Int32 | String)
  puts "test(Int32|String)"
  p! typeof(x)
  foo(x)
end

# same as test(Int32|String) and would override it
def test(x : String | Int32)
  puts "test(String|Int32)"
  p! typeof(x)
  foo(x)
end

def test(x : String | Int)
  puts "test(String|Int)"
  p! typeof(x)
  foo(x)
end

def test(x : String | Int32 | Float32)
  puts "test(String|Int32|Float32)"
  p! typeof(x)
  foo(x)
end

def test(x : T) forall T
  puts "test(T)"
  p! typeof(x)
  foo(x)
end

def test(x)
  puts "test(x)"
  p! typeof(x)
  foo(x)
end

x = ARGV[0]
x = x.to_i32? || x
test(x)
