#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

__ "struct (Value Type)"

struct BookStruct
  getter name : String
  getter tags : Array(String)
  getter price : Float64

  def initialize(@name : String, @tags : Array(String), @price : Float64)
  end
end

p BookStruct.new "《史记》", ["历史", "中国"], 66.6

__ "record (Value Type)"

record BookRecord,
  name : String,
  tags : Array(String),
  price : Float64

p BookRecord.new "《史记》", ["历史", "中国"], 66.6

__ "class (Referenced Type)"

class BookClass
  getter name : String
  getter tags : Array(String)
  getter price : Float64

  def initialize(@name : String, @tags : Array(String), @price : Float64)
  end
end

book = BookClass.new "《史记》", ["历史", "中国"], 66.6
p! book
p! book.is_a? BookClass
p! book.object_id

__ "类方法和类变量"

class A
  @@class_var = "world"

  def self.class_method : String
    "hello" + " " + @@class_var
  end
end

p! A.class_method

__ "继承"

class DiscountBookClass < BookClass
  getter discount : Float64

  def initialize(@name : String, @tags : Array(String), @price : Float64, @discount : Float64)
  end

  def final_price : Float64
    @price * @discount
  end
end

book = DiscountBookClass.new "《史记》", ["历史", "中国"], 66, 0.8
p! book
p! book.final_price
p! book.is_a? BookClass
p! book.is_a? DiscountBookClass

__ "include modules 继承modules中所有实例方法"

module Discountable
  getter discount : Float64

  def final_price : Float64
    @price * discount
  end
end

class DiscountableBooClass
  include Discountable
  getter name : String
  getter tags : Array(String)
  getter price : Float64

  def initialize(@name : String, @tags : Array(String), @price : Float64, @discount : Float64)
  end
end

book = DiscountableBooClass.new "《史记》", ["历史", "中国"], 66, 0.8
p! book
p! book.final_price
p! book.is_a? Discountable
p! book.is_a? DiscountableBooClass

__ "extend modules 继承moudles中所有类方法"

module StaticTitle
  def static_title : String
    "Hello world"
  end
end

class ABookClass < BookClass
  extend StaticTitle
end

book = ABookClass.new "《史记》", ["历史", "中国"], 66
p! book
p! ABookClass.static_title
p! book.is_a? BookClass
p! book.is_a? ABookClass

__ "反射"

p! "你好".is_a? String
p! "你好".responds_to? :split # 检查是否有 split 方法
p! "你好".nil?
p! typeof("你好")
