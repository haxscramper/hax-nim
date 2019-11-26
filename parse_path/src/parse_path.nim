# version 1.3
import hargparse
import colechopkg/lib
import hmisc/helpers

import macros
import os
import strutils

# TODO Add suport for reading from stdin
# TODO wrap all actions in proc so they can be used as a functions

parseArgs:
  opt:
    name: "last-suffix"
    opt: ["--last-suffix"]
    help: "Print last suffix for filename"
  opt:
    name: "all-suffixes"
    opt: ["--all-suffixes", "--all-suffices"]
    help: "Print all suffixes for file"
  opt:
    name: "dirname"
    opt: ["--dirname"]
    help: "Print directory name"
  opt:
    name: "name"
    opt: ["--name"]
    help: "Print file name without extension"
  opt:
    name: "basename"
    opt: ["--basename"]
    help: "Print file name with extension"
  opt:
    name: "no-local"
    opt: ["--no-local"]
    help: "Remove ./ prefix if present before doing anything else"
  opt:
    name: "all"
    opt: ["--all"]
    help: "Pring everything back"

if hasErrors:
  quit(1)
elif argParsed.len != 1:
  ceUserError0("need exactly one file name")
  quit(1)
else:
  var path = argParsed[0]
  let localPrefix =
    if "no-local".kp():
      if path.startsWith("./"): path = path[2..^1]
      ""
    else:
      ""

  var (dir, name, ext) = path.splitFile()
  var suffices = (name & ext).split('.')


  name = suffices[0]
  suffices = suffices[1..^1]
  let basename = ( @[name] & suffices ).join(".")

  if "last-suffix".kp:
    echo suffices[^1]
  if "all-suffixes".kp:
    echo suffices.join(".")
  if "dirname".kp:
    echo dir
  if "name".kp:
    echo name
  if "basename".kp:
    echo basename
  if "all".kp:
    echo localPrefix & joinpath(@[dir, basename])
