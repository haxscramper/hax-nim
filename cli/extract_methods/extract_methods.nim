import pegs
import macros, argparse
import os
import re
import strutils
import strformat, sequtils



parseArgs:
  opt:
    name: "input-file"
    opt: ["--input-file", "+takes_value"]
    help: "Input file name"
  opt:
    name: "output-dir"
    opt: ["--output-dir", "+takes_value"]
    help: "Output directory"

func getbalance(str: string): int = str.count('{') - str.count('}')

proc splitClass(str: string): seq[tuple[
  name, body: string]] =
  var parenBalance = 0

  var currentName: string
  var currentBody: seq[string]
  var methStarted: bool

  let mre = re".*?(\w+)\s*\(.*\).*\{"

  for line in str.split('\n'):
    if line =~ re"import.*" or line =~ re"class.*":
      discard
    elif parenBalance == 0 and line =~ mre:
      currentName = matches[0]
      currentbody.add("{")
      inc parenBalance
      methstarted = true
    else:
      parenBalance += line.getbalance()
      if (parenBalance <= 0):
        currentBody.add(line)
        parenbalance = 0
        result.add((currentName, currentBody.join("\n")))
        methstarted = false




if "input-file".kp and "output-dir".kp:
  let infile = "input-file".k.tostr
  let outdir = "output-dir".k.tostr
  createDir(outdir)
  for meth in infile.readFile().string.splitClass():
    (outdir & meth.name & ".tmp.c").writefile(meth.body)
