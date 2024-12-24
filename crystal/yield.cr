#!/usr/bin/env crystal

def __(t : String)
  puts ""
  puts "------\e[1;33m " + t + " \e[m" + ("-" * (50 - 8 - t.size))
  puts ""
end

__ "with ... yield"

class Table
  @header : String = ""
  @rows : Array(String) = [] of String

  def initialize
  end

  def self.create(&)
    table = Table.new
    with table yield # here
    table
  end

  def header(s)
    @header = s
  end

  def row(s)
    @rows << s
  end
end

table = Table.create do
  header "Todo items"
  row "item1"
  row "item2"
  row "item3"
end

p! table
