#!/usr/bin/env nimcr
#nimcr-args c --verbosity:0 --hints:off

import shell
import strscans
import hargparse
import colechopkg/lib
import sequtils
import strutils
import os
import re
import random
import hmisc/hfile
import strformat

setShellDebugConfig({})

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
  opt:
    name: "png-using"
    opt: ["--png-using"]
    takes: @["latex", "svg"]
    help: "Choose way of creating png images (intermediate steps)"



let doDebug = testing or "debug".kp
proc dlog(args: varargs[string, `$`]): void =
  ceUserLog0 args.join(" ")

let (sourceFile, targetFile, circuitDir, targetExt) =
  block:
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

    (sourceFile, targetFile, circuitDir, targetExt)


dlog "Input file:", sourceFile
dlog "Output file:", targetFile
dlog "Circuit dir:", circuitDir
dlog "Target ext:", targetExt

let macroResult =
  mktemp(
    templ = "circuit.tmp.XXXXXXXXX",
    dir = "/tmp/circuit/"
  )

proc createPngUsingSVG(inFile, outFile: string) = 
  var width = 400
  var height = 400
  shell:
    m4 -I ($circuitDir) "svg.m4" ($inFile) > ($macroResult".pic")
    dpic -v ($macroResult".pic") > ($macroResult".svg")

  let (dim, code) = shellVerbose:
    grep "\"<!-- width=\"" ($macroResult".svg")

  if not scanf(dim, "<!-- width=\"$i\" height=\"$i\" -->", width, height):
    echo dim, "does not match"

  width *= 3
  height *= 3

  shell:
    inkscape -z -e ($outFile) -w ($width) -h ($height) ($macroResult".svg")


proc createTexTikzpicture(inFile: string): string =
  let (res, code) = shellVerbose:
    pipe:
      m4 -I ($circuitDir) pgf.m4 ($inFile)
      dpic -g

  result = res


proc createTexStandalone(inFile, outFile: string) =
  outFile.writeFile(fmt"""
\documentclass[convert = false]{{standalone}}
\usepackage{{tikz}}

\begin{{document}}

{createTexTikzpicture(inFile)}

\end{{document}}
""")

proc createPngUsingTex(inFile, outFile: string) =
  let texfile = macroResult & ".tex"
  dlog "Creating png using latex, outputting into", texfile
  dlog "Output file is", outFile
  createTexStandalone(inFile, texfile)
  let (res, code) = shellVerbose {dokCommand, dokError, dokRuntime}:
    # latexmk -cd -C ($texfile)
    latexmk -cd -pdf "-latexoption='-shell-escape'" "--interaction='nonstopmode'" ($texfile)
    gm convert -density 150 ($macroResult".pdf") -quality 90 ($outFile)

  dlog "Output code is", code

case targetExt:
  of "png":
    when testing:
      createPngUsingTex(sourceFile, targetFile)
    else:
      if not "png-using".kp:
        createPngUsingSVG(sourceFile, targetFile)
      else:
        case "png-using".k.tostr:
          of "latex": createPngUsingTex(sourceFile, targetFile)
          of "svg": createPngUsingSVG(sourceFile, targetFile)

  of "tex":
    if "tex-tikzpicture".kp:
      targetFile.writeFile(createTexTikzpicture(sourceFile))
    else:
      createTexStandalone(sourceFile, targetFile)
  else:
    ceUserError0 "Unknown extension " & targetExt



when not testing:
  removeFile(macroResult)
