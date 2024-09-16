#!/usr/bin/env crystal

require "spec"

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

#
# - sizeof:  查看 type 在 stack 内存大小
# - alignof: 查看 type 在 stack 内存对齐
# - instance_sizeof: 查看 reference type 在 heap 内存大小
# - instance_alignof: 查看 reference type 在 heap 内存对齐
# - offsetof: 查看 field 在 type 中内存偏移量
#

describe "Test" do
  it "primitive types (Value Type)" do
    sizeof(Bool).should eq 1
    alignof(Bool).should eq 1

    sizeof(Char).should eq 4 # UTF-8 4
    alignof(Char).should eq 4

    sizeof(Int).should eq 24
    alignof(Int).should eq 8

    sizeof(Int32).should eq 4
    alignof(Int32).should eq 4
    sizeof(UInt32).should eq 4
    alignof(UInt32).should eq 4

    sizeof(Int8).should eq 1
    alignof(Int8).should eq 1
    sizeof(UInt8).should eq 1
    alignof(UInt8).should eq 1

    sizeof(Int16).should eq 2
    alignof(Int16).should eq 2
    sizeof(UInt16).should eq 2
    alignof(UInt16).should eq 2

    sizeof(Int64).should eq 8
    alignof(Int64).should eq 8
    sizeof(UInt64).should eq 8
    alignof(UInt64).should eq 8

    sizeof(Int128).should eq 16
    alignof(Int128).should eq 8
    sizeof(UInt128).should eq 16
    alignof(UInt128).should eq 8

    sizeof(Float).should eq 16
    alignof(Float).should eq 8
    sizeof(Float32).should eq 4
    alignof(Float32).should eq 4
    sizeof(Float64).should eq 8
    alignof(Float64).should eq 8
  end

  it "String (Reference Type)" do
    sizeof(String).should eq 8 # ptr
    alignof(String).should eq 8
    instance_sizeof(String).should eq 16 # TYPEID 4 + bytesize 4 + length 4 + c bytesize
    instance_alignof(String).should eq 4
    offsetof(String, @bytesize).should eq 4
    offsetof(String, @length).should eq 8
    offsetof(String, @c).should eq 12
  end

  it "Array (Reference Type)" do
    sizeof(Array(Int32)).should eq 8 # ptr
    alignof(Array(Int32)).should eq 8
    instance_sizeof(Array(Int32)).should eq 24 # TYPEID 4 + size 4 + capacity 4 + offset_to_buffer 4 + buffer 8
    instance_alignof(Array(Int32)).should eq 8
    offsetof(Array(Int32), @size).should eq 4
    offsetof(Array(Int32), @capacity).should eq 8
    offsetof(Array(Int32), @offset_to_buffer).should eq 12
    offsetof(Array(Int32), @buffer).should eq 16
  end

  it "StaticArray (Value Type)" do
    sizeof(StaticArray(Int32, 2)).should eq 8 # 4 * 2
    alignof(StaticArray(Int32, 2)).should eq 4
  end

  it "Hash (Reference Type)" do
    sizeof(Hash(String, Int32)).should eq 8 # ptr
    alignof(Hash(String, Int32)).should eq 8
    instance_sizeof(Hash(String, Int32)).should eq 56 # TYPEID 4 + first 4 + entries 8 + indices 8 + size 4 + deleted_count 4 + indices_bytesize 1 + indices_size_pow2 1 + compare_by_identity 1 + padding 5 + block 16
    instance_alignof(Hash(String, Int32)).should eq 8
    offsetof(Hash(String, Int32), @first).should eq 4
    offsetof(Hash(String, Int32), @entries).should eq 8
    offsetof(Hash(String, Int32), @indices).should eq 16
    offsetof(Hash(String, Int32), @size).should eq 24
    offsetof(Hash(String, Int32), @deleted_count).should eq 28
    offsetof(Hash(String, Int32), @indices_bytesize).should eq 32
    offsetof(Hash(String, Int32), @indices_size_pow2).should eq 33
    offsetof(Hash(String, Int32), @compare_by_identity).should eq 34
    offsetof(Hash(String, Int32), @block).should eq 40
  end

  it "Set (Value Type)" do
    sizeof(Set(String)).should eq 8 # ptr to Hash(String,Nil)
    alignof(Set(String)).should eq 8
    offsetof(Set(String), @hash).should eq 0
  end

  it "Tuple (Value Type)" do
    sizeof(Tuple(Int32, Int32, Int32)).should eq 12 # 4 * 3
    alignof(Tuple(Int32, Int32, Int32)).should eq 4
  end

  it "NamedTuple (Value Type)" do
    sizeof(NamedTuple(name: String, age: UInt8)).should eq 16 # name 8 + age 1 + padding 7
    alignof(NamedTuple(name: String, age: UInt8)).should eq 8
  end

  it "Range (Value Type)" do
    sizeof(Range(Int32, Int32)).should eq 12 # begin 4 + end 4 + exclusive 1 + padding 3
    alignof(Range(Int32, Int32)).should eq 4
    offsetof(Range(Int32, Int32), @begin).should eq 0
    offsetof(Range(Int32, Int32), @end).should eq 4
    offsetof(Range(Int32, Int32), @exclusive).should eq 8
  end

  it "Proc (Value Type)" do
    sizeof(Proc(String, String)).should eq 16
    alignof(Proc(String, String)).should eq 8
  end
end

enum Lang
  Crystal
  Nim
  Zig
end

enum Color : UInt8
  Red
  Blue
  Green
end

describe "Test Enum" do
  it "Enum (Value Type)" do
    sizeof(Lang).should eq 4 # 4 UInt32
    alignof(Lang).should eq 4

    sizeof(Color).should eq 1 # 1 UInt8
    alignof(Color).should eq 1
  end
end

struct UserStruct
  @id : UInt32
  @name : String
  @balance : UInt32

  def initialize(@id : UInt32, @name : String, @balance : UInt32)
  end
end

describe "Test struct" do
  it "struct (Value Type)" do
    sizeof(UserStruct).should eq 24 # (id 4 + padding 4) + name 8 + (balance 4 + padding 4)
    alignof(UserStruct).should eq 8
    offsetof(UserStruct, @id).should eq 0
    offsetof(UserStruct, @name).should eq 8
    offsetof(UserStruct, @balance).should eq 16
  end
end

record UserRecord,
  id : UInt32,
  name : String,
  balance : UInt32

@[Packed]
record UserPackedRecord,
  id : UInt32,
  name : String,
  balance : UInt32

describe "Test record" do
  it "record (Value Type)" do
    sizeof(UserRecord).should eq 24 # (id 4 + padding 4) + name 8 + (balance 4 + padding 4)
    alignof(UserRecord).should eq 8
    offsetof(UserRecord, @id).should eq 0
    offsetof(UserRecord, @name).should eq 8
    offsetof(UserRecord, @balance).should eq 16
  end

  it "Packed struct" do
    sizeof(UserPackedRecord).should eq 16 # id 4 + name 8 + balance 4
    alignof(UserPackedRecord).should eq 1
    offsetof(UserPackedRecord, @id).should eq 0
    offsetof(UserPackedRecord, @name).should eq 4
    offsetof(UserPackedRecord, @balance).should eq 12
  end
end

class UserClass
  @id : UInt32
  @name : String
  @balance : UInt32

  def initialize(@id : UInt32, @name : String, @balance : UInt32)
  end
end

describe "Test class" do
  it "class (Reference Type)" do
    sizeof(UserClass).should eq 8 # ptr
    alignof(UserClass).should eq 8
    instance_sizeof(UserClass).should eq 24 # TYPEID 4 + id 4 + name 8 + (balance 4 + padding 4)
    instance_alignof(UserClass).should eq 8
    offsetof(UserClass, @id).should eq 4
    offsetof(UserClass, @name).should eq 8
    offsetof(UserClass, @balance).should eq 16
  end
end

describe "Test Pointer" do
  it "Pointer (Value Type)" do
    sizeof(Pointer(String)).should eq 8
    alignof(Pointer(String)).should eq 8
  end

  it "Slice (Value Type)" do
    sizeof(Slice(String)).should eq 16 # size 4 + ( read_only 1 + padding 3 ) + ptr 8
    alignof(Slice(String)).should eq 8
    offsetof(Slice(String), @size).should eq 0
    offsetof(Slice(String), @read_only).should eq 4
    offsetof(Slice(String), @pointer).should eq 8
  end
end
