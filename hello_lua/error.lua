local function parse_rgb(rgb)
  local r, g, b = string.match(rgb, '#([A-F0-9][A-F0-9])([A-F0-9][A-F0-9])([A-F0-9][A-F0-9])')
  if r and g and b then
    return {
      r = tonumber(r, 16),
      g = tonumber(g, 16),
      b = tonumber(b, 16)
    }
  else
    error('invalid rgb format')
  end
end

local function parse_rgb_2(rgb)
  local r, g, b = string.match(rgb, '#([A-F0-9][A-F0-9])([A-F0-9][A-F0-9])([A-F0-9][A-F0-9])')
  if r and g and b then
    return {
      r = tonumber(r, 16),
      g = tonumber(g, 16),
      b = tonumber(b, 16)
    }, nil
  else
    return nil, 'invalid rgb format'
  end
end

local function print_table(tbl)
  for k, v in pairs(tbl) do
    print(k, v)
  end
end

-- parse_rgb('77AAFF') -- raise error and print stacktrace


print([[

-----------
-- pcall --
-----------
]])
-- ok->true, result->function return value
-- ok->false, result->error object
local ok, result = pcall(parse_rgb, ('77AAFF'))
if ok then
  print('\27[1;32m[OK]\27[m:')
  print_table(result)
else
  print('\27[1;31m[ERR]\27[m: ' .. result)
end

print([[

------------
-- assert --
------------
]])
-- first_value->not-nil, return first_value
-- first_value->nil, raise error(second_value)
local rgb = assert(parse_rgb_2('#77AAFF'))
print_table(rgb)


print([[

------------
-- xpcall --
------------
]])
local function err_handle_wrap_err(err)
  return 'errmsg generated by err handle, inner: ' .. err
end
local function err_handle_default_value(err)
  return { r = 0, g = 0, b = 0 }
end
local ok, result = xpcall(parse_rgb, err_handle_default_value, '77AAFF')
if ok then
  print('\27[1;32m[OK]\27[m:')
  print_table(result)
else
  print('\27[1;31m[ERR]\27[m:')
  print_table(result)
end
