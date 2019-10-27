# Package

version       = "0.1.0"
author        = "haxscramper"
description   = "A new awesome nimble package"
license       = "BSD-3-Clause"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["create_script"]



# Dependencies

requires "nim >= 0.20.2"
requires "hmisc >= 0.1.0"
requires "colecho >= 0.1.0"
requires "parsetoml >= 0.5.0"
requires "moustachu >= 0.13.0"


after install:
  exec "ln -sf ~/.nimble/bin/create_script ~/.nimble/bin/create-script"
