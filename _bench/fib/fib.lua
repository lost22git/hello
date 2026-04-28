#!/usr/bin/env luajit

local function fib(n)
	if n < 2 then
		return 1
	else
		return fib(n - 1) + fib(n - 2)
	end
end

local n = 40
print(fib(40))
