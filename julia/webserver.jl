#!/usr/bin/env julia

using Oxygen

@get "/info" function (req)
    json(Dict("msg" => "Hello"))
end

serve(port = 8080)
