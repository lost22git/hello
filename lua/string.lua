#!/usr/bin/env lua

-- single quote vs double quote (NO DIFFERENCE!!!)
--
assert("hello\r\nlua" == "hello\r\nlua")

-- mutiline string `[[`, `]]`
--
local json = [[
{
  "foo": "bar"
}
]]
assert(json == '{\n  "foo": "bar"\n}\n')

-- raw string `[[`, `]]`
--
assert([[\r\n]] == "\\r\\n")

-- byte length and unicode length
--
local a = "lua 👻"
assert(#a == 8, "the byte length of `" .. a .. "` is not 8")
assert(utf8.len(a) == 5, "the unicode length of `" .. a .. "` is not 5")

-- string concat `..`
--
assert("hello" .. ", " .. "lua" == "hello, lua")

-- string format
--
assert(string.format("hello, %s", "lua") == "hello, lua")

-- string format (Ruby-like `%`)
--
getmetatable("").__mod = function(a, b)
	if not b then
		return a
	elseif type(b) == "table" then
		return string.format(a, table.unpack(b))
	else
		return string.format(a, b)
	end
end

assert("hello, %s" % "lua" == "hello, lua")

-- string interpolation
-- http://lua-users.org/wiki/StringInterpolation

-- string replace
--
assert(string.gsub("hello, lua", "l", "L") == "heLLo, Lua")
assert(string.gsub("hello, lua", "l", function(_)
	return "L"
end) == "heLLo, Lua")

-- string iterate
--

local name = "halo 👻"

print("=>`%s` iterate:" % name)
for i = 1, #name do
	print(string.sub(name, i, i))
end

print("=>`%s` iterate bytes:" % name)
for i = 1, #name do
	print(string.byte(name, i))
end

print("=>`%s` iterate chars:" % name)
for c in string.gmatch(name, "([%z\1-\127\194-\244][\128-\191]*)") do
	print(c)
end

-- contains
--
function string:contains(str)
	return str == "" or self:find(str, 1, true) ~= nil
end

-- startswith
--
function string:startswith(str)
	return str == "" or self:sub(1, #str) == str
end

-- endswith
--
function string:endswith(str)
	return str == "" or self:sub(-#str) == str
end

assert(string.contains("halo 👻", "👻"))
assert(string.startswith("halo 👻", "ha"))
assert(string.endswith("halo 👻", "👻"))

-- find
--
local begin_pos, end_pos = string.find("halo 👻", "👻", 1, true)
assert(begin_pos == 6)
assert(end_pos == 6 + 4 - 1)
