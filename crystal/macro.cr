#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

class Book
  property name : String
  property price : Float64

  def initialize(@name : String, @price : Float64)
  end
end

__ "macro id"

hello = "你好"

macro use_macroid(v)
  p! {{ v.id }}
end

macro use_token(v)
  p! {{ v }}
end

use_macroid "hello"
use_macroid :hello
use_macroid hello

use_token "hello"
use_token :hello
use_token hello

__ "has_getter? / has_setter?"

macro has_method?(obj, method)
  if {{ obj }}.is_a? Pointer
    {{ obj }}.value.responds_to? {{ method.id.symbolize }} 
  else
    {{ obj }}.responds_to? {{ method.id.symbolize }} 
  end
end

macro has_setter?(obj, field_name)
  has_method? {{ obj }}, "{{ field_name.id }}="
end

macro has_getter?(obj, field_name)
  has_method? {{ obj }}, {{ field_name.id }}
end

book = Book.new("foo", 11)
p! has_getter? book, price
p! has_getter? book, pricesss
p! has_getter? pointerof(book), price
p! has_getter? pointerof(book), pricesss
p! has_setter? book, price
p! has_setter? book, pricesss
p! has_setter? pointerof(book), price
p! has_setter? pointerof(book), pricesss

__ "get/set fields"

macro get_field_val_real(obj, field_name)
  if {{ obj }}.is_a? Pointer
    {{ obj }}.value.{{ field_name.id }}
  else
    {{ obj }}.{{ field_name.id }}
  end
end

macro set_field_val_real(obj, field_name, field_val)
  if {{ obj }}.is_a? Pointer
    {{ obj }}.value.{{ field_name.id }} = {{ field_val }}
  else
    {{ obj }}.{{ field_name.id }} = {{ field_val }}
  end
end

def get_field_val(obj : Pointer(T) | T, field_name : String) forall T
  {% begin %}
  return case field_name
  {% for f in T.instance_vars %}
    when "{{ f.name }}"
      get_field_val_real obj, {{ f.name }}   
  {% end %}
    else raise "the field: `{{ T.name }}.#{ field_name }` not found"
  end
  {% end %}
end

def get_field_val?(obj : Pointer(T) | T, field_name : String) forall T
  get_field_val obj, field_name
rescue
  nil
end

def set_field_val(obj : Pointer(T) | T, field_name : String, field_val) forall T
  {% begin %}
  case field_name
  {% for f in T.instance_vars %}
  when "{{ f.name }}"
    if v = field_val.as? {{ f.type }}
      set_field_val_real obj, {{ f.name }}, v
    else 
      raise "the type of field: `{{ T.name }}.#{ field_name }` is `{{ f.type }}`, but got `#{ field_val }`" 
    end
  {% end %}
  else
    raise "the field: `{{ T.name }}.#{ field_name }` not found"
  end
  {% end %}
end

def set_field_val?(obj : Pointer(T) | T, field_name : String, field_val) : Bool forall T
  set_field_val obj, field_name, field_val
  true
rescue
  false
end

book = Book.new("foo", 11)
set_field_val book, "price", 22.0
p! get_field_val book, "price"

p! set_field_val? pointerof(book), "price", 33.0
p! get_field_val? pointerof(book), "price"
p! get_field_val?(pointerof(book), "pricesssss") || 0

__ "@type in macro"

macro gen_get_field_val
  def [](field_name)
  {% verbatim do %}
    {% begin %}
      case field_name
    {% for f in @type.instance_vars %}
      when "{{ f.name }}", {{ f.name.id.symbolize }}
        return @{{ f.name }}
    {% end %}
      else
        raise "the field: `{{ @type.name }}.#{ field_name }` not found"
      end
    {% end %}
  {% end %}
  end

  def []?(field_name)
    self[field_name]
  rescue
    nil
  end
end

macro gen_set_field_val
  def []=(field_name, field_val)
  {% verbatim do %}
    {% begin %}
      case field_name
    {% for f in @type.instance_vars %}
      when "{{ f.name }}", {{ f.name.id.symbolize }}
        if v = field_val.as? {{ f.type }}
          @{{ f.name }} = v
        else
          raise "the type of field: `{{ @type.name }}.#{ field_name }` is `{{ f.type }}`, but got `#{field_val}`" 
        end
    {% end %}
      else
        raise "the field: `{{ @type.name }}.#{ field_name }` not found"
      end
    {% end %}
  {% end %}
  end
end

class Book2
  @name : String
  property price : Float32

  def initialize(@name, @price)
  end

  gen_get_field_val
  gen_set_field_val
end

book = Book2.new "foo", 33
fname = "price"
p! book[fname]
p! book["price"]
p! book[:price]
p! book["fffffffffffff"]?
book[fname] = 44_f32
p! book[fname]
