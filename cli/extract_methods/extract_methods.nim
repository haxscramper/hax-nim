import pegs
import macros, ../../lib/argparse
import os
import re
import strutils
import strformat, sequtils



func getbalance(str: string): int = str.count('{') - str.count('}')

proc splitClass(str: string): seq[tuple[
  name, body: string]] =
  var parenBalance = 0

  var currentName: string
  var currentBody: seq[string]
  var methStarted: bool
  var className: string
  var inComment: bool

  for line in str.split('\n'):
    if line =~ re".*?class (\w+) .*?":
      className = matches[0]
    elif line =~ re"import.*" or line =~ re"class.*":
      discard

    elif not methstarted and line =~ re".*?(\w+)\s*\(.*\).*\{":
      currentName = matches[0]
      methstarted = true

    if methstarted:
      parenBalance += line.getbalance()

      let notrail =
        if line =~ re"(.*?)\/\/.*": matches[0]
        else: line

      let noComm =
        if inComment:
          if notrail =~ re".*?\*\/(.*)":
            inComment = false
            matches[0]
          else:
            ""
        else:
          if notrail =~ re"(.*?)\/\*.*?\*\/(.*)":
            matches[0] & " " & matches[1]
          elif notrail =~ re"(.*?)\/\*.*":
            inComment = true
            matches[0]
          else:
            notrail

      currentBody.add(noComm)

      if (parenBalance == 0):
        methstarted = false
        result.add((
          className & "." & currentName,
          currentBody[1..^2].join("\n")))
        currentBody = @[]


parseArgs:
  opt:
    name: "input-file"
    opt: ["--input-file", "+takes_value"]
    help: "Input file name"
  opt:
    name: "output-dir"
    opt: ["--output-dir", "+takes_value"]
    help: "Output directory"
  opt:
    name: "verbose"
    opt: ["--verbose", "-v"]
    help: "verbose"


if "input-file".kp and "output-dir".kp:
  let infile = "input-file".k.tostr
  let outdir = "output-dir".k.tostr
  createDir(outdir)
  for meth in infile.readFile().string.splitClass():
    let outPath = joinpath(outdir, meth.name & ".tmp.c")

    if "verbose".kp:
      echo outPath

    outpath.writefile("{\n" & meth.body & "\n}")
