import moustachu
import fsmonitor
import argparse
import parsetoml
import sequtils
import strformat
import strutils
import math
import algorithm
import os
import asyncdispatch
import osproc
import posix
import selectors
import asyncfile
import options


import termformat
import colecho_lib
import colecho_types
import helpers
import uprompt
import argparse
import macros
import strparser
import logging
import create_script

var utilityPid: Pid



var log1 = newFileLogger("debug.tmp", fmtStr="[$time]: ")

proc dlog(args: varargs[string, `$`]): void =
  discard
  # log1.log(lvlAll, args.join(" "))
  # log1.file.flushFile()

#~#=== Command line parsing
type
  OperationMode = enum
    watchChanges
    doRunDev
    createFiles
    runSingleBuild
    defaultMode

  CmdParsed = object
    optParsed: Table[string, CmdArg]
    argParsed: seq[string]
    maxRepeat: Opt[int]
    waitUtil: bool
    case kind: OperationMode:
      of doRunDev:
        targetFile: string
      of watchChanges:
        targetFiles: seq[string]
      of createFiles:
        testShName: string
        fileToCreate: string
      of runSingleBuild:
        buildUname: string
        newFile: string
      of defaultMode:
        nil


proc parseCMDLine(): CmdParsed =
  parseArgs:
    opt:
      name: "create-test"
      opt: ["--create"]
      help: "Generate test.sh or update if already exits"
    opt:
      name: "listen"
      opt: ["--listen"]
      help: "Listen for changes and emit them"
    opt:
      name: "uname"
      opt: ["--uname", "+takes_value"]
      help: "Unique name of build config"
  #   opt:
  #     name: "run-dev"
  #     opt: ["--dev", "--run-dev", "+takes_value"]
  #     help: """Start build-and-run loop for a file (single file). If file
  # does not exist user will be asked whether he wants to create new file.
  # New file will be created using default script templates.
  # """

  dlog("---")

  result =
    if argParsed.len >= 2 and argParsed[0] == "dev":
      # "Running fsm-build dev on ", argParsed[1]
      CmdParsed(kind: doRunDev, targetFile: argParsed[1])
      #runDev(argParsed[1])

    elif argParsed.len >= 1 and argParsed[0] == "watch":
      dlog("Starting watcher")
      dlog("Input:", argParsed[1])
      #startWatch(argParsed[1].toStrSeq())
      CmdParsed(
        kind: watchChanges,
        targetFiles: argParsed[1].toStrSeq())
    elif argParsed.len >= 1 and argParsed[0] == "build":
      if "uname".kp:
        dlog("Running build", "uname".k.toStr())
      CmdParsed(
        kind: runSingleBuild,
        buildUname: "uname".k.toStr())
    elif "create-test".kp:
      dlog("Creating files")
      dlog("Input:", "create-test".k.toStr())
      let files: (string,string) = toTuple[string]("create-test".k.toStr())
      #files[1].writeFile(generateTestScript(files[0]))
      CmdParsed(
        kind: createFiles,
        testShName: files[1],
        fileToCreate: files[0])
    else:
      CmdParsed(kind: defaultMode)

  if "test".kp:
    result.maxRepeat = "test".k.toInt()

  result.waitUtil =
    if "wait-util".kp:
      "wait-util".k.toBool()
    else: true


#~#==== Build options config parsing
type
  BuildOption = object
    name: string
    uname: string
    files: seq[string]
    buildCommand: string
    runCommand: string


proc parseSingleBuild(
  build: TomlValueRef,
  langExt: string,
  c: var Context): Option[BuildOption] =
  if not build.hasKey("ext") or build["ext"].getStr() != langExt:
    return

  if build.hasKey("build_file"):
    c["build_file"] = build["build_file"].getStr().render(c)


  let buildCommand = build["build_command"].getStr().render(c)
  let runCommand = "$#" %
      [if build.hasKey("run_command"):
        build["run_command"].getStr().render(c)
      else:
        "./{{input_file}}".render(c)]
  let watchedFiles: seq[string] = concat(
    if build.hasKey("file_globs"):
      let globs = build["file_globs"]
      if globs.kind == Array:
        globs.getElems()
        .mapIt(
          it
          .getStr()
          .render(c))
        .mapIt(
          toSeq(it.walkPattern))
        .concat()
      elif globs.kind == String:
        toSeq(globs.getStr().walkPattern())
      else:
        ceUserError0("""Incorred format for file globs.
Expected string or array of strings""")
        quit(1)
    else:
      @[]
    ,
    @["{{input_file}}".render(c)])

  result = some(BuildOption(
    name: build["name"].getStr(),
    uname: build["uname"].getStr(),
    files: watchedFiles,
    buildCommand: buildCommand,
    runCommand: runCommand
  ))


proc parseBuildBuildOpts(
  buildConf: string,
  langExt: string,
  c: var Context,
  buildOpts: seq[BuildOption] = @[]):
    seq[BuildOption] =

  result = buildOpts

  let buildConfToml = parsetoml.parseFile(buildConf)

  for build in buildConfToml["build"].getElems():
    let res = parseSingleBuild(build, langExt, c)
    if res.isSome:
      result.add(res.get())

proc startUtility(command: string): void =
  let pid: Pid = fork()
  if pid < 0:
    ceUserError0 "Cannot fork"
  elif pid > 0:
    utilityPid = pid
  else:
    # TODO Add support for runing without use of bash, but by using
    # exec. Add different build command options for build command
    # templates (bash_build and exec_build).
    printSeparator("upper")
    let res = execShellCmd("/bin/bash -c \"$#\"" % command)
    # TODO catch return value and print all errors
    printSeparator("lower")
    quit(0)

proc startKeyListener() {.async.} =
  # TODO wip. I need to somehow read input from stdin even without RET
  # being pressed. This solution still blocks rebuilding and I don't
  # know how to solve this.
  ceinfo "Started key listener", 2
  var inSelector = newSelector[int]()
  inSelector.registerHandle(0, {Event.Read}, -1)

  var stdinAsync = newAsyncFile(AsyncFd(0))

  while true:
    ceLog "Waiting for events on stdin"
    let inChar = await stdinAsync.read(1)

proc select(
  buildOpts: seq[BuildOption],
  selected: Option[tuple[uname, ext: string]] =
    none((string,string))):
    BuildOption =


  let selected =
    if buildOpts.len > 1:
      buildOpts
      .mapIt(it.buildCommand)
      .toSeq()
      .enumerate()
      .mapIt(($(it[0]) , it[1]))
      .printTwoColumns()

      getInt(
        "Enter selection",
        valRange = (0, buildOpts.len))
    else:
      0

  result = buildOpts[selected]


proc getBuildBuildOpts(inputFile: string): seq[BuildOption] =
  var c: Context = newContext()

  c["input_file"] = inputFile
  let ext = inputFile.splitFile()[2][1..^1]

  result =
    "~/.config/hax-config/build_commands.toml"
    .expandTilde()
    .parseBuildBuildOpts(langExt = ext, c = c)


#~#==== File change loops
proc startBuilder(
  selected: BuildOption,
  waitUtil: bool = true,
  maxRepeat: Opt[int]) {.async.} =
    var monitor = newMonitor()
    for file in selected.files:
      monitor.add(file, {MonitorModify})

    proc doBuild(cmd: string): bool =
      execShellCmd("/bin/bash -c \"$#\"" % cmd) == 0

    while true:
      if utilityPid != 0 and waitUtil:
        var statVal: cint
        let res = waitpid(utilityPid, statVal, 0)
        ceUserInfo0("Run finished")
      elif utilityPid != 0 and not waitUtil:
        var null: cint = 0
        let testRes = waitpid(utilityPid, null, WNOHANG)
        if testRes == 0:
          ceUserInfo0("New change, killing utility ...")
          let res = kill(utilityPid, SIGTERM)
          ceUserLog0("Done")
        else:
          ceUserInfo0("Run finished")


      ceUserInfo2 "Waiting for changes"
      let events: seq[MonitorEvent] = await monitor.read()
      # TODO analyze list of changes and reduce list of events to
      # avoid repetitive rebuilds
      ceUserInfo2 "Change happened"

      for ev in events:
        if ev.kind == MonitorModify:
          ceUserInfo0 "Rebuilding ..."
          if doBuild(selected.buildCommand):
            ceUserInfo0 "Rebuild succeded, running ..."
            startUtility(selected.runCommand)

proc runDev(opts: CmdParsed) =
  let inputFile = opts.targetFile
  if not fileExists(inputFile) and promptYN("File is missing. Create files?"):
    let templ = templateFromExt(inputFile.getLastExt())
    inputFile.writeFile(templ.body)

    let pathnames: cstringArray = allocCStringArray(@[inputFile])
    let pathname: cstring = pathnames[0]

    var statRes: Stat
    discard stat(pathname, statRes)
    let newMode: Mode = statRes.stMode or S_IXUSR
    discard chmod(inputFile, newMode)

    deallocCstringArray(pathnames)

    ceUserInfo2(fmt"Created file {inputFile} and added execution permission")

  else:
    ceUserWarn("No file created, exiting development")
    return


  let buildOpts = getBuildBuildOpts(inputFile)

  asyncCheck startBuilder(
    buildOpts.select(),
    opts.waitUtil,
    opts.maxRepeat)

  runForever()

proc listenChanges(files: seq[string]) {.async.} =
  ## Listen for changes in files (and associated with them) and print
  ## to stdout
  # TODO monitor directory and add new files for watching
  dlog "Starting change listener on:", files[0]
  var monitor = newMonitor()
  for file in files:
    monitor.add(file, {MonitorModify})

  while true:
    dlog "Waiting for events"
    let events: seq[MonitorEvent] = await monitor.read()
    dlog $events
    for ev in events:
      dlog "Event" & $ev
      case ev.kind:
        of MonitorModify: echo ev.name
        else: discard

proc startWatch(files: seq[string]) =
  asyncCheck listenChanges(files)
  runForever()


#~#==== Test script generations
proc toFuncName(str: string): string =
  str.multiReplace([
      (".", "_"),
      ("-", "_")
  ])

proc getBuilderFunction(selected: BuildOption, inputFile: string): string =
  result = fmt"""
function build_{inputFile.toFuncName()} {{
  uname="{selected.uname}"
  fsm-build build --uname:"$uname"

  if [[ "$?" != "0" ]]; then
    colecho -e "Build failed"
    fail_state="1"
    return 1
  else
    fail_state="0"
  fi
}}
"""

proc getRunnerFunction(selected: BuildOption, inputFile: string): string =
  result = fmt"""
function run_{inputFile.toFuncName()} {{
  build_{inputFile.toFuncName()}
  if [[ "$fail_state" != "0" ]]; then
    return $fail_state
  fi

  uname="{selected.uname}"
  $msg -i:3 "Running $uname"

  {selected.runCommand}
  echo
}}
"""

proc generateChangeReader(buildFiles: seq[string]): string =
  let funcCalls = buildFiles.mapIt(
    fmt"""
    if [[ "$file" == "{it}" ]]; then
      run_{it.toFuncName()}
    fi""").join("\n")

  let preCall = buildFiles.mapIt("run_" & it.toFuncName()).join("\n")

  fmt"""
{preCall}

fsm-build watch "[$watched]" |
  while read -r file; do
{funcCalls}
  done
"""

proc generateTestScript(inputFile:string): string =
  let buildOpts = getBuildBuildOpts(inputFile)
  let selected = buildOpts.select()
  # TODO use script templates for file header
  let templ: string = fmt"""
#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
IFS=","
watched="'$*'"
IFS=" "
fail_state="0"
msg="colecho -b"

$msg "Starting test script"

{getBuilderFunction(selected, inputFile)}
{getRunnerFunction(selected, inputFile)}
{generateChangeReader(@[inputFile])}

"""
  return templ


when isMainModule:
  let parseResults = parseCMDLine()
  case parseResults.kind:
    of doRunDev:
      runDev(parseResults)
    of watchChanges:
      startWatch(parseResults.targetFiles)
    of runSingleBuild:
      discard
    of createFiles:
      parseResults.testShName.writeFile(
        generateTestScript(
          parseResults.fileToCreate))
    of defaultMode:
      discard
