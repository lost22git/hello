# Package

version = "0.1.0"
author = "lost"
description = "A new awesome nimble package"
license = "MIT"
srcDir = "src"
bin = @["tls_fragment"]

# Dependencies

requires "nim >= 2.2.4"

task buildSync, "build sync version":
  exec "nimble build --verbose -d:release -d:lto"

task buildAsync, "build async version":
  exec "nimble build --verbose -d:release -d:lto -d:async"
