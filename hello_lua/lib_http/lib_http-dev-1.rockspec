package = "lib_http"
version = "dev-1"
source = {
  url = "git+ssh://git@github.com/lost22git/hello_lua.git"
}
description = {
  homepage = "*** please enter a project homepage ***",
  license = "*** please specify a license ***"
}

dependencies = {
  "lua >= 5.1, <5.5",
  "http",
  "lua-cjson",
}

build = {
  type = "builtin",
  modules = {
    main = "src/main.lua"
  }
}
