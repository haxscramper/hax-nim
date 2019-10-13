import ../lib/argparse
import ../lib/colecho_lib

import macros
import os
import strutils

parseArgs:
  opt:
    name: "last-suffix"
    opt: ["--last-suffix"]
    help: "Print last suffix for filename"
  opt:
    name: "all-suffixes"
    opt: ["--all-suffixes"]
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

if "get-help".kp:
  cmdPrintHelp(helpTable)
  quit(0)

if hasErrors:
  quit(1)
elif argParsed.len != 1:
  ceUserError0("need exactly one file name")
  quit(1)
else:
  let (dir, name, ext) = argParsed[0].splitFile()
  let suffices = ext.split('.')
  if "last-suffix".kp:
    echo suffices[^1]
  elif "all-suffixes".kp:
    echo suffices.join(".")
  elif "dirname".kp:
    echo dir
  elif "name".kp:
    echo name
  elif "basename".kp:
    echo name & ext
