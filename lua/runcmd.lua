#!/usr/bin/env lua

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
