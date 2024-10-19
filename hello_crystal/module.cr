# std 顶级 module `::`

# 比如当我们自定义的class名称和std的class名称重复时

class MyModule::Channel
  def initialize
    # ch = Channel(Nil) # 编译错误，因为会被误认为 MyModule::Channel
    ch = ::Channel(Nil) # 编译通过
  end
end

# extends methods for built-in String class via include modules

module StringExt
  def name_and_ext
    self.split "."
  end
end

class String
  include StringExt
end

name, ext = "hello.cr".name_and_ext
p! name
p! ext
