#!/usr/bin/env nimcr
#nimcr-args c --verbosity:0 --hints:off

import shell
import hargparse
import colechopkg/lib
import sequtils
import strutils
import os
import re
import random

const testing = true

parseArgs:
  opt:
    name: "debug"
    opt: ["--debug"]
    help: "Use debug printing"
  opt:
    name: "circuit-dir"
    opt: ["--circuit-dir"]
    help: "Directory containing Circuit_macros distribution"



let doDebug = testing or "debug".kp
proc dlog(args: varargs[string, `$`]): void =
  ceUserLog0 args.join(" ")

let (sourceFile, targetFile) =
  when testing:
    ("input.tmp.m4", "output.tmp.png")
  else:
    if argParsed.len < 2:
      ceUserError0 "Missing input and output files (last two arguments for commang)"
      quit 1
    else:
      (argParsed[0], argParsed[1])

let circuitDir =
  if "circuit-dir".kp: "circuit-dir".k.toStr
  else: "~/.config/hax-software/m4circuit".expandTilde()

let targetExt =
  block:
    let (_, _, ext) = targetFile.splitFile()
    if ext.len > 0:
      ext[1..^1]
    else:
      ceUserError0("Target file has no extension")
      quit 1

dlog "Input file:", sourceFile
dlog "Output file:", targetFile
dlog "Circuit dir:", circuitDir
dlog "Target ext:", targetExt

let macroFile =
  block:
    let tmp = case targetExt:
      of "svg", "png": "svg.m4"
      else:
        ceUserError0("""Target extension is not recoginised.
        #TODO write allowed list""")
        quit 1

    circuitDir.joinPath(tmp)

proc getRandomBase64(length: int): string =
  newSeqWith(
    length,
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".
    sample()).join("")

proc mktemp(
  templ: string = "tmp.XXXXXXXXXX",
  dir: string = "/tmp"): string =

  if not existsDir(dir):
    createDir(dir)

  proc get(): string =
    dir.joinPath(
      templ.multiReplace(
        templ.
        findAll(re"X+").
        mapIt((re(it), getRandomBase64(it.len)))
      ))

  var file = get()
  while fileExists(file):
    file = get()

  file.open(fmWrite).close()

  return file


let intermediateExt =
  case targetExt:
    of "png": "svg"
    else: targetExt

let macroResult =
  mktemp(
    templ = "circuit.tmp.XXXXXXXXX." & intermediateExt,
    dir = "/tmp/circuit/"
  )

let (res, code) = shellVerbose:
  m4 -I ($circuitDir) ($macroFile) ($sourceFile) > ($macroResult".pic")
  dpic -v ($macroResult".pic") > ($macroResult)


case targetExt:
  of "png":
    let convertRes = shellVerbose:
      inkscape -z -e ($targetFile) -w ($400) -h ($400) ($macroResult)
  else:
    echo "dd"


when not testing:
  removeFile(macroResult)
