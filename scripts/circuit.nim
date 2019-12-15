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
import hmisc/helpers
import strformat

setShellDebugConfig({})

const testing = false


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
  if doDebug:
    ceUserLog0 args.join(" ")

dlog ">>>> ========"

if doDebug:
  echo "Arguments"
  for k, v in optParsed:
    echo k, " ", v

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

proc gettmp(): string =
  mktemp(
    templ = "circuit.tmp.XXXXXXXXX",
    dir = "/tmp/circuit/"
  )



proc createPngUsingSVG(inFile, outFile: string) =
  dlog "Using svg"
  let macroResult = gettmp()
  var width = 400
  var height = 400

  block:
    let
      svgfile = macroResult & ".svg"
      picfile = macroResult & ".pic"

    dlog &"{picfile} -> {svgfile}"
    let (res, code) = shellVerbose (if doDebug: {dokRuntime} else: {}):
      m4 -I ($circuitDir) "svg.m4" ($inFile) > ($picfile)
      dpic -v ($picfile) > ($svgfile)

    if code != 0:
      dlog "Error duing m4 or dpic execution"
      dlog res
    else:
      dlog "m4 and Pic generation ok"

  block:
    let (dim, code) = shellVerbose:
      grep "\"<!-- width=\"" ($macroResult".svg")

    if not scanf(dim, "<!-- width=\"$i\" height=\"$i\" -->", width, height):
      dlog dim, "does not match"
    else:
      dlog &"width: {height} height: {width}"

  width *= 3
  height *= 3

  block:
    let (res, code) = shellVerbose:
      inkscape -z -e ($outFile) -w ($width) -h ($height) ($macroResult".svg")

    if code != 0:
      dlog "Error durink inkscape execution"
      dlog res
    else:
      dlog "Inkscape ok"


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

proc createPdfUsingTexTikz(inFile, outFile: string) =
  let macroResult = gettmp()
  let texfile = macroResult & ".tex"
  let pdffile = macroResult & ".pdf"
  dlog "Creating png using latex, outputting into", texfile
  dlog "Output file is", outFile
  createTexStandalone(inFile, texfile)
  let (res, code) = shellVerbose {dokCommand, dokError, dokRuntime}:
    # latexmk -cd -C ($texfile)
    latexmk -cd -pdf "-latexoption='-shell-escape'" "--interaction='nonstopmode'" ($texfile)
    cp ($pdffile) ($outFile)


proc createEpsUsingTex(inFile, outFile: string) =
  let macroResult = gettmp()
  let pdfFile = macroResult & ".pdf"
  createPdfUsingTexTikz(inFile, pdfFile)

  let (res, code) = shellVerbose (doDebug.tern({dokCommand}, {})):
    gm convert ($inFile) ($outFile)

  dlog "Output code is", code

proc createPngUsingTex(inFile, outFile: string) =
  let macroResult = gettmp()
  let pdfFile = macroResult & ".pdf"
  createPdfUsingTexTikz(inFile, pdfFile)

  let (res, code) = shellVerbose (doDebug.tern({dokCommand}, {})):
    gm convert -density 150 ($pdfFile) -quality 90 ($outFile)

  dlog "Output code is", code

case targetExt:
  of "png":
    dlog "Creating png target"
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
    dlog "Creating tex target"
    if "tex-tikzpicture".kp:
      targetFile.writeFile(createTexTikzpicture(sourceFile))
    else:
      createTexStandalone(sourceFile, targetFile)

  of "eps":
    dlog "Creating eps target"
    createEpsUsingTex(sourceFile, targetFile)
  else:
    ceUserError0 "Unknown extension " & targetExt


dlog "<<<< ========"
