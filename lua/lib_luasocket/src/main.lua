local https = require 'ssl.https'
local ltn12 = require('ltn12')
local cjson = require 'cjson'


local function http_get()
  local result, code, headers, status_line = https.request { method = 'GET', url = 'http://httpbin.org/status/200' }

  assert(result == 1)
  assert(code == 200)
  -- print('status_line: ' .. status_line)
  -- for header_name, header_value in pairs(headers) do
  --   print(header_name .. ' => ' .. header_value)
  -- end
end

local function http_post()
  local response_body = {}
  local result, code, headers, status_line = https.request {
    method = 'POST',
    url = 'http://httpbin.org/post',
    source = ltn12.source.string(cjson.encode({ hello = 'lua' })),
    sink = ltn12.sink.table(response_body),
  }

  assert(result == 1)
  assert(code == 200)
  -- print('status_line: ' .. status_line)
  -- for header_name, header_value in pairs(headers) do
  --   print(header_name .. ' => ' .. header_value)
  -- end
  assert(cjson.decode(response_body[1]).data == [[{"hello":"lua"}]])
end

http_get()
http_post()
