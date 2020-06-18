# Package

version       = "0.1.0"
author        = "haxscramper"
description   = "Collection of parsing algorithms"
license       = "BSD-3-Clause"
srcDir        = "src"
bin = @["wiptest"]

# Dependencies

requires "nim >= 1.2.0"
requires "regex"
requires "hmisc"
requires "macroutils"

after test:
  exec "./wiptest"
