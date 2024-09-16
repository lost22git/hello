#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

__ "调试输出"

v = "你好"
p! v

__ "字符串 %()"

puts %(你好 "世界")

__ "raw 字符串 %q()"

puts %q(你好 \n "世界")

__ "字符串拼接"

p! "你好" + " " + "世界"

__ "字符串插值 `\#{}`"

v = "你好"
puts "v is #{v}"
puts "#{v} len is #{v.size}"
puts "转义 \#{v.size}"
p! String.interpolation(v, " len is ", v.size)

__ "字符串格式化 %"

p! "你好，%s" % {"世界"}

__ "多行字符串"

puts "你好

世界
"

puts "-" * 11

# 不换行
puts "你好 \

世界
"

__ "动态字符串String::Builder"

p! (String::Builder.new("你好") << " " << "世界").to_s
p!(String.build do |sb|
  sb << "你好"
  sb << " "
  sb << "世界"
end)

__ "字符串长度"

p! "你好".size
p! "你好".bytesize

__ "字符串eq / compare"

p! "你好a" == "你好A"
p! "你好a" <=> "你好A"
p! "你好a".compare("你好A", case_insensitive: true)

__ "字符串sub string"

p! "你好，世界"[0..1]
p! "你好，世界"[-2..-1]

__ "字符串是否为空串"

p! "".empty?
p! "\r\n \v\t \r".empty?
p! "\r\n \v\t \r".blank?

__ "字符串strip"

p! " 你好 世界 ".lstrip
p! " 你好 世界 ".rstrip
p! " 你好 世界 ".strip

__ "字符串split"

p! "你好，世界".split("，")
p! "你好，世界".split("，", limit: 1)
p! "你好，世界，，crystal".split("，", remove_empty: true)

__ "字符串sub/gsub"

p! "你好，世界".sub("你好", "halo")
p! "你好，世界; 你好，crystal".gsub("你好", "halo")

__ "字符串解析"

text = "
crystal,gc,nativecode
java,gc,bytecode
rust,non-gc,nativecode
"

find /(\w+\-?\w+),(\w+\-?\w+),(\w+\-?\w+)/, in: text do |m|
  puts "[#{m.begin(1)},#{m.end(1)}) => #{m[1]}"
  puts "[#{m.begin(2)},#{m.end(2)}) => #{m[2]}"
  puts "[#{m.begin(3)},#{m.end(3)}) => #{m[3]}"
  puts "-" * 40
end

def find(pattern : Regex, *, in text : String, &)
  pos = 0
  while true
    if m = pattern.match text, pos
      yield m
      pos = m.end 0
    else
      break
    end
  end
end
