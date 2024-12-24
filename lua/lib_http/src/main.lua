--[[
  http: https://daurnimator.github.io/lua-http/0.4/
  cjson: https://kyne.au/~mark/software/lua-cjson-manual.html
]]

local http_request = require 'http.request'
local cjson = require 'cjson'

local function http_get()
  local headers, stream = assert(http_request.new_from_uri("https://httpbin.org/status/200"):go())
  local body = assert(stream:get_body_as_string())

  for header_name, header_value in pairs(headers) do
    print(header_name .. ' => ' .. header_value)
  end

  assert(headers:get(':status') == '200')
  assert(body == '')
end


local function http_post()
  local request = http_request.new_from_uri("https://httpbin.org/post")
  request.headers:upsert(':method', 'POST')
  request:set_body(cjson.encode({ hello = 'lua' }))
  local headers, stream = assert(request:go())

  local body = assert(stream:get_body_as_string())
  assert(headers:get(':status') == '200')
  assert(cjson.decode(body).data == [[{"hello":"lua"}]])
end

http_get()
http_post()
