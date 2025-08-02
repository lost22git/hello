local function countup(from, to, step)
	local next = from
	return function()
		local result = nil
		if next <= to then
			result, next = next, next + step
		end
		return result
	end
end

for i in countup(1, 10, 3) do
	print(i)
end
