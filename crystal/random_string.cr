def random_string(length : Int32) : String
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  String.new(length) do |bytes|
    bytes.to_slice(length).fill { chars.to_slice.sample }
    {length, length}
  end
end

def random_string2(length : Int32) : String
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  String.new(Bytes.new(length) { chars.to_slice.sample })
end

def random_string3(length : Int32) : String
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  String.new (1..length).map { chars.to_slice.sample }.to_unsafe, length
end

def random_string4(length : Int32) : String
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  String.new Pointer.malloc(3*length) { chars.to_unsafe[rand(64)] }, length
end

def random_string5(length : Int32) : String
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  String.new Bytes.new(length) { chars.to_unsafe[rand(64)] }
end

def random_string6(length : Int32) : String
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  String.new(length) do |bytes|
    bytes.to_slice(length).fill { chars.to_unsafe[rand(64)] }
    {length, length}
  end
end

def random_string7(length : Int32) : String
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  indices = Random::DEFAULT.random_bytes(length).map! { |v| v // 4 }
  String.new(capacity: length) do |buffer|
    indices.each_with_index do |chars_index, buffer_index|
      buffer[buffer_index] = chars.byte_at(chars_index)
    end
    {length, length}
  end
end

def random_string8(length : Int32, random = Random::DEFAULT) : String
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  if length <= 1024
    buffer = uninitialized UInt8[1024]
    bytes = buffer.to_slice[0...length]
  else
    bytes = Bytes.new(length)
  end

  random.random_bytes(bytes)
  bytes.map! { |v| v % chars.bytesize }

  String.new(capacity: length) do |buffer|
    bytes.each_with_index do |chars_index, buffer_index|
      buffer[buffer_index] = chars.byte_at(chars_index)
    end
    {length, length}
  end
end

puts random_string 20
puts random_string2 20
puts random_string3 20
puts random_string4 20
puts random_string5 20
puts random_string6 20
puts random_string7 20
puts random_string8 20

require "benchmark"
Benchmark.ips(warmup: 4, calculation: 10) do |x|
  x.report "1" do
    random_string 20
  end

  x.report "2" do
    random_string2 20
  end

  x.report "3" do
    random_string3 20
  end

  x.report "4" do
    random_string4 20
  end

  x.report "5" do
    random_string5 20
  end

  x.report "6" do
    random_string6 20
  end

  x.report "7" do
    random_string7 20
  end

  x.report "8" do
    random_string8 20
  end
end
