table.unpack = table.unpack or unpack -- 5.1 compatibility

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

-- string startswith
--
function string.startswith(str, prefix)
  return prefix == "" or str:sub(1, #prefix) == prefix
end

-- string find_each
-- consumer consume each pattern matching of str
--
function string.find_each(str, pattern, consumer)
  local pos = 1
  while pos ~= nil do
    local t = table.pack(string.find(str, pattern, pos))
    pos = t[2]
    if pos ~= nil then
      consumer(table.move(t, 3, #t, 1, {}))
    end
  end
end

-- Run external command and capture output
--
function os.run(cmd, strip)
  local f = assert(io.popen(cmd, 'r'))
  local s = assert(f:read('*a'))
  f:close()
  if not strip then return s end
  s = string.gsub(s, '^%s+', '')
  s = string.gsub(s, '%s+$', '')
  return s
end

-- Get os name
--
function os.name()
  if jit then
    return jit.os
  end

  local name = os.run([[uname -o 2>/dev/null]], true)
  return name or "Windows"
end

IS_WINDOWS = os.name() == 'Windows'

-- mkdirs creates any intermediate directories in the path, if needed.
--
function os.mkdirs(path)
  local cmd = (IS_WINDOWS
    and
    -- ([[powershell -nologo -noprofile -command "New-Item -ItemType Directory -Path '%s' -Force"]] % path)
    [[cmd /c "mkdir '%s'"]] % path
    or
    [[sh -c "mkdir -p '%s'"]] % path
  )
  os.run(cmd)
end

-- Check if the file exists
--
function os.exists_file(path)
  local f = io.open(path, "r")
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

-- Get file count in the dir
--
function os.count_file(dir)
  local cmd = (IS_WINDOWS
    and
    -- [[powershell -nologo -noprofile -command "(ls -File '%s').Count"]] % dir
    [[cmd /c "attrib '%s\*.*' | find /c /v ''"]] % dir
    or
    [[sh -c "ls '%s' | wc -l"]] % dir
  )
  local count = os.run(cmd, true)
  -- print('[file_count] count:', count)
  return tonumber(count)
end
