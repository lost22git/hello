package = "lib_luasocket"
version = "dev-1"
source = {
  url = "git+ssh://git@github.com/lost22git/hello_lua.git"
}
description = {
  homepage = "*** please enter a project homepage ***",
  license = "*** please specify a license ***"
}
build = {
  type = "builtin",

  modules = {
    main = "src/main.lua"
  }
}

dependencies = {
  "lua >=5.1,<5.5",
  "luasocket",
  "luasec",
  "lua-cjson",
}
