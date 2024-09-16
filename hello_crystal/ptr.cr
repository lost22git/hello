#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (60 - 8 - t.size))
  puts ""
end

__ "pointer"

class Book
  property name : String
  property price : Float32

  def initialize(@name, @price)
  end
end

book = Book.new "fo", 23

p! instance_sizeof(Book)  # (TYPEID 4 + padding 4) + name 8 + price 4 + padding 4
p! offsetof(Book, @name)  # 8
p! offsetof(Book, @price) # 16

p! pointerof(book).address # 变量 book 在 stack 中的地址
p! book.object_id          # heap 中的地址 (变量 book 的值)
p! book.object_id == book.unsafe_as(UInt64)
p! book.object_id == book.as(Pointer(Book)).address

p! Pointer(Float32).new(book.unsafe_as(UInt64) + offsetof(Book, @price)).value
Pointer(Float32).new(book.unsafe_as(UInt64) + offsetof(Book, @price)).value = 33
p! Pointer(Float32).new(book.unsafe_as(UInt64) + offsetof(Book, @price)).value

#
# Slice
#
# - bound check 版本的 Pointer
# - unsafe: 会有悬空指针的问题
# - Value type: 包含以下三个字段
#   - size : UInt32
#   - read_only : Bool
#   - pointer : Pointer(T)
#

__ "Slice is unsafe when pointer to stack memory"

def get_slice_pointer_to_stack
  a = StaticArray[1]
  a.to_slice
end

b = get_slice_pointer_to_stack
p! b    # dangling pointer !!!
p! b[0] # 0

def get_slice_pointer_to_heap
  a = IO::Memory.new
  a << "hello"
  a.to_slice
end

b = get_slice_pointer_to_heap
p! String.new b # ok, "hello"
