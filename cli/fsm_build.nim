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
import times


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
import colecho_types

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



proc checkList(list: openArray[
  tuple[p: bool,m: string,l: MessageType]]):
    bool =

  result = true
  for it in list:
    if not it.p:
      if it.l == mError: ceUserError0(it.m)
      if it.l == mWarn: ceUserWarn(it.m)
      result = false


proc parseSingleBuild(
  build: TomlValueRef,
  langExt: string,
  c: var Context): Option[BuildOption] =

  if not build.hasKey("name"):
    ceUserError0("Missing name")
    return

  let name = build["name"]

  if not checkList(
    # IDEA contracts + toml validation DSL. This can be similar to
    # conditional expressions for argparse2
    [(build.hasKey("ext") and build["ext"].getStr() == langExt,
      "", mLog),
     (build.hasKey("ext"),
      fmt"Build '{name}' is missing 'ext'", mWarn),
     (build.hasKey("uname"),
      fmt"Build '{name}' is missing 'uname'", mError),
     (build.hasKey("build_command"),
      fmt"Build '{name}' is missing 'build_command'", mError)
    ]):
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
        ceUserError0(fmt"""
Incorred format for file globs when parsing
'{build["name"].getStr()}'. Expected string
or array of strings""")
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


proc parseBuildOpts(
  buildConf: string,
  langExt: string,
  c: var Context,
  buildOpts: seq[BuildOption] = @[]):
    seq[BuildOption] =
  ceUserInfo0("Parsing config")

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
        valRange = (0, buildOpts.len - 1))
    else:
      0

  result = buildOpts[selected]


proc getBuildOpts(inputFile: string): seq[BuildOption] =
  var c: Context = newContext()

  c["input_file"] = inputFile
  c["fsm_build_bin_dir"] = getCallPath().splitPath()[0]
  let ext = inputFile.getLastExt()
  let conf = getCallPath().splitPath()[0] & "/config/build_commands.toml"

  result = conf.parseBuildOpts(langExt = ext, c = c)



#~#==== File change loops
proc startBuilder(
  selected: BuildOption,
  waitUtil: bool = true,
  maxRepeat: Opt[int]) {.async.} =
    var monitor = newMonitor()
    ceUserInfo0("List of watched files:")

    for file in selected.files.sorted.deduplicate(true):
      ceUserLog0(file)
      monitor.add(file, {MonitorModify})

    proc doBuild(cmd: string): bool =
      printSeparator("upper")
      #echo  "Building using:", cmd
      result = execShellCmd("/bin/bash -c \"$#\"" % cmd) == 0
      printSeparator("lower")

    proc doRunBuild(): bool =
      if doBuild(selected.buildCommand):
        ceUserInfo0("Build succeded, running")
        startUtility(selected.runCommand)
        true
      else:
        ceUserWarn("Build failed")
        false





    let cooldownSec: float = 1
    var lastBuild: float = epochTime()

    ceUserInfo0("Initial build")
    discard doRunBuild()



    var lastBuildState: bool = true

    # TODO Print message about waiting for changes in according places
    while true:
      let events: seq[MonitorEvent] = await monitor.read()
      # TODO analyze list of changes and reduce list of events to
      # avoid repetitive rebuilds or wait at least certain amount of
      # time between rebuilds ignoring all changes to ignore events
      # that happened in sequence but in different event batches
      if events.mapIt(it.kind == MonitorModify).foldl(a or b) and
         lastBuild + cooldownSec < epochTime():
        block: # Kill utility if needed
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
            elif lastBuildState:
              ceUserInfo0("Run finished")

        block: # Rebuild and restart
          lastBuild = epochTime()
          ceUserInfo0 "Rebuilding ..."

          lastBuildState = doRunBuild()
          if not lastBuildState:
            ceUserInfo0 "Waiting for new changes"


proc runDev(opts: CmdParsed) =
  let inputFile = opts.targetFile
  let fileExt = inputFile.getLastExt()
  if not fileExists(inputFile):
    if promptYN("File is missing. Create files?"):
      let templ = templateFromExt(fileExt)
      inputFile.writeFile(templ.body)

      inputFile.addUExec()

      ceUserInfo2(fmt"Created file {inputFile} and added execution permission")

    else:
      ceUserWarn("No file created, exiting development")
      return


  let buildOpts = getBuildOpts(inputFile)
  if buildOpts.len == 0:
    ceUserError0(fmt"""
No build opts found for '{inputFile}'. Make sure
that *at least one* build command with extension '{fileExt}'
or with no extension at all
exists in configuraion file and can be parsed without errors
(see previous messages for error clarifications).
""")
    quit (1)


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
  let buildOpts = getBuildOpts(inputFile)
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

# IDEA hide cursor in terminal on start and show it when program is
# terminated. Requires to catch shortcuts.
