#!/usr/bin/env julia

using HTTP

# === GET ===

r = HTTP.get("https://httpbin.org/ip")
println(r.status)
for (k, v) in r.headers
    println(k, " => ", v)
end
println(String(r.body))

# === POST ===

r = HTTP.post(
    "https://httpbin.org/post",
    ["content-type" => "application/json"],
    """
    {
      "data": ["foo","bar"]
    }
    """
)
println(r.status)
for (k, v) in r.headers
    println(k, " => ", v)
end
println(String(r.body))
