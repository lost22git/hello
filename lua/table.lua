#!/usr/bin/env lua

-- table donot store entry which value is nil
-- `#` just simply return current max index number in table

assert(#{} == 0)
assert(#{ 1 } == 1)
assert(#{ 1, 1 } == 2)
assert(#{ 1, 1, nil } == 2)
assert(#{ 1, nil, 1 } == 3)
assert(#{ 1, 1, foo = "bar" } == 2)

function table:size()
  local result = 0
  for k, v in pairs(self) do
    result = result + 1
    -- print(k, v)
  end
  return result
end

assert(table.size({ foo = "bar", zoo = "baz" }) == 2)
assert(table.size({ 1, foo = "bar", zoo = "baz" }) == 3)
assert(table.size({ nil, 1, foo = "bar", zoo = "baz" }) == 3)

assert(table.size({}) == 0)
assert(table.size({ 1 }) == 1)
assert(table.size({ 1, 1, nil }) == 2)
assert(table.size({ 1, nil, 1 }) == 2)

-- table concat
--

assert(table.concat({'src', 'main', 'hello.lua'}, '/') == 'src/main/hello.lua')

