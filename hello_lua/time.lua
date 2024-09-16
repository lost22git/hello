local tz = {}
---local timezone int
---@return  integer timezone_int
function tz.int()
  return os.time() - os.time(os.date('!*t'))
end

---get timezone str from timezone int
---@param value integer timezone_int
---@param sep? string default: ''
---@return  string timezone_str
function tz.str_from_int(value, sep)
  if value == 0 then return 'Z' end
  sep = sep or ''
  local flag = value >= 0 and '+' or '-'
  local hour = math.abs(value) // 3600
  local minute = math.abs(value) % 3600 // 60
  return string.format('%s%02d%s%02d', flag, hour, sep, minute)
end

---local timezone str
---@param sep? string default: ''
---@return  string timezone_str
function tz.str(sep)
  return tz.str_from_int(tz.int(), sep)
end

---get timezone int from timezone str
---@param s string timezone_str
---@return  integer timezone_int
function tz.int_from_str(s)
  s = string.gsub(s, '^%s+', '')
  s = string.gsub(s, '%s+$', '')

  if s == 'Z' then
    return 0
  else
    local flag, hour, minute = string.match(s, [[([%+%-]?)(%d%d)[:]?(%d%d)$]])
    local total_seconds = tonumber(hour) * 3600 + tonumber(minute) * 60
    return flag == '-' and -total_seconds or total_seconds
  end
end

---parse timezone str or timezone int and return timezone_str, timezone_int
---@param value string|integer timezone_str or timezone_int
---@return string, integer
function tz.parse(value)
  if type(value) == 'number' then
    return tz.str_from_int(value), value
  elseif type(value) == 'string' then
    return value, tz.int_from_str(value)
  else
    error('unsupported tz type: ' .. type(value))
  end
end

assert(tz.str_from_int(tz.int_from_str('+0220')) == '+0220')
assert(tz.str_from_int(tz.int_from_str('-0220')) == '-0220')
assert(table.pack(tz.parse('+0220'))[1] == '+0220')
assert(table.pack(tz.parse('+0220'))[2] == 2 * 60 * 60 + 20 * 60)
assert(table.pack(tz.parse('-0220'))[1] == '-0220')
assert(table.pack(tz.parse('-0220'))[2] == -(2 * 60 * 60 + 20 * 60))


---get unix timestamp
---@param datetime osdateparam datetime
---@param timezone? string|integer default: tz.str() (local timezone)
---@return integer unix timestamp
local function unix_time(datetime, timezone)
  local diff = 0
  if timezone then
    local _, tz_int = tz.parse(timezone)
    diff = tz.int() - tz_int
  end
  if type(datetime) == 'table' then
    return os.time(datetime) + diff
  else
    error('unsupport datetime type: ' .. type(datetime))
  end
end

assert(unix_time(os.date('*t')), unix_time(os.date('!*t'), 0))


---format RFC3339
---@param time osdateparam|integer datetime or unix timestamp
---@param timezone? string|integer default: tz.str() (local timezone)
---@return string
local function format_rfc3339(time, timezone)
  timezone = timezone or tz.str()
  local tz_str, tz_int = tz.parse(timezone)

  local _time = 0
  if type(time) == 'number' then
    _time = time
  elseif type(time) == 'table' then
    _time = unix_time(time, timezone)
  else
    error('unsupport time type: ' .. type(time))
  end

  return os.date('!%Y-%m-%dT%H:%M:%S', _time + tz_int) .. tz_str
end

print('LOCAL_NOW rfc3339 format', format_rfc3339(os.time()))
print('LOCAL_NOW rfc3339 format', format_rfc3339(os.date('*t')))
print('UTC_NOW rfc3339 format', format_rfc3339(os.time(), 0))
print('UTC_NOW rfc3339 format', format_rfc3339(os.date('!*t'), 'Z'))


---parse RFC3339 as datetime,timezone_int
---@param s string
---@return osdateparam, integer
local function parse_rfc3339(s)
  local year, month, day, hour, minute, second, timezone = string.match(
    s,
    [[(%d%d%d%d)%-(%d%d)%-(%d%d)T(%d%d):(%d%d):(%d%d)(.+)$]]
  )
  local _, tz_int = tz.parse(timezone)
  return { year = year, month = month, day = day, hour = hour, min = minute, sec = second, isdst = false }, tz_int
end

assert(format_rfc3339(parse_rfc3339('2022-02-02T10:10:10+0220')) == '2022-02-02T10:10:10+0220')
