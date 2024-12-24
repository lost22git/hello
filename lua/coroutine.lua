local co = coroutine.create(function(x)
  print('Coroutine x:', x)
  local y = coroutine.yield(x + 10)
  print('Coroutine x:', x)
  print('Coroutine y:', y)
  coroutine.yield(x + y)
end)

local ok, first_result = coroutine.resume(co, 10)
assert(ok == true)
assert(first_result == 10 + 10)

local ok, second_result = coroutine.resume(co, first_result)
assert(ok == true)
assert(second_result == 10 + 20)



local iter_co = coroutine.create(function()
  for i = 1, 10, 1 do
    coroutine.yield(i)
  end
end)

for i = 1, 10 do
  local ok, val = coroutine.resume(iter_co)
  assert(ok)
  assert(val == i)
end
