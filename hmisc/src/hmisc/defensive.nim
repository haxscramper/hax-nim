import colechopkg/lib
import helpers
import sequtils
import strformat
import os
import strutils
import logging
import shell

export shell

## Collection of helper functions to provide verbose messages for
## common errors

const noShellMsg*: set[DebugOutputKind] = {}

proc die*() {.discardable, noreturn.} =
  ceUserInfo2("Terminating program")
  quit 1

proc findFirstFile*(
  pattern: string, filePurpose: string, debug = true): string =
  ## Find first file that matches glob
  let notebooks = toSeq(walkFiles(pattern))
  if notebooks.len < 1:
    ceUserError0(&"No {filePurpose}s found in directory")
    die()
  elif notebooks.len > 1:
    ceUserWarn(&"Multiple {filePurpose}s found in directory, using first")
    for n in notebooks:
      ceUserLog0(n, 2)
    notebooks[0]
  else:
    ceUserInfo0(&"Found single {filePurpose}")
    notebooks[0]

template initDefense*(logfile: string = "", prefix: string = ""): untyped =
  ##[ Create necessary variables and procs for debugging.

  All settings and procedures are scope-local. `logile` is a name of
  the file to use for creating file logger - if empty (default) file
  logging is disabled. `prefix` is a prefix string used for log
  messages.

  .. code-block::
      initDefence(prefix = "FFF: ")
      showLog("hello", "world")
      showError("test", "ddd")

  .. code-block:: text
      >>> FFF: hello world
      !!! FFF: test ddd
  ]##
  var defensiveFileLogEnabled {.inject.} = logFile != ""
  var filelogger {.inject.}: FileLogger
  if defensiveFileLogEnabled:
    fileLogger = newFileLogger(logfile)

  var defensiveLogLevel {.inject.} = lvlAll
  var defensiveLogPrefix {.inject.} = prefix
  var defensiveLogIndentation {.inject.} = 0

  proc increaseLogIndent() = defensiveLogIndentation += 2
  proc decreaseLogIndent() = defensiveLogIndentation -= 2

  template runIndentedLog(body: untyped): untyped =
    increaseLogIndent()
    body
    decreaseLogIndent()

  proc saveLog(text: string, logLevel = lvlAll): void =
    if defensiveFileLogEnabled:
      fileLogger.log(lvlAll, text)

  proc showError(msgs: varargs[string, `$`]) =
    let text = msgs.join(" ")
    ceUserError0(defensiveLogPrefix & text, defensiveLogIndentation)
    saveLog(text, lvlError)
    # fileLogger.log(lvlError, text)

  proc showLog(msgs: varargs[string, `$`]) =
    let text = msgs.join(" ")
    ceUserLog0(defensiveLogPrefix & text, defensiveLogIndentation)
    saveLog(text, lvlDebug)
    # fileLogger.log(lvlDebug, text)

  proc showInfo(msgs: varargs[string, `$`]) =
    let text = msgs.join(" ")
    ceUserInfo2(defensiveLogPrefix & text, defensiveLogIndentation)
    saveLog(text, lvlInfo)
    # fileLogger.log(lvlInfo, text)

  proc showWarn(msgs: varargs[string, `$`]) =
    let text = msgs.join(" ")
    ceUserWarn(defensiveLogPrefix & text, defensiveLogIndentation)
    saveLog(text, lvlWarn)
    # fileLogger.log(lvlWarn, text)

  proc showPlain(msgs: varargs[string, `$`]) =
    let text = msgs.join(" ")
    echo text

template safeRunCommand*(
  msg: string,
  runConf: set[DebugOutputKind],
  hideerror: bool = false,
  body: untyped): bool =
  ## Execute shell command and return `true/false` based on execution
  ## results. This template internally uses syntax from `shell`
  ## module. Execution results are not returned.
  ##
  ## :hideerror: if `true` do not print stderr after error in shell
  ## :runConf: passed to `shellVerboseErr`
  ## :msg: Print 'msg' and execution succeded/failed. Leave empty (`""`)
  ##       for not messages.
  runnableExamples:
    initDefense()
    let res = safeRunCommand("test", {dokCommand}):
      ls

    assert res == true

  block:
    var resOk = false
    let (res, err, code) = shellVerboseErr runConf:
      body

    if code != 0:
      if msg.len > 0:
        showError(msg, "exited with non-zero output code")


      if not hideerror:
        showError("Error output")
        showPlain(err)
        saveLog(err)
        saveLog(res)
    else:
      if msg.len > 0:
        showInfo(msg, "exited with output code 0")

      resOk = true

    resOk


when isMainModule:
  setCurrentDir("/tmp")
  initDefense(prefix = "FFF: ")
  showWarn("hello", "world")
  showError("test", "ddd")

  let res = safeRunCommand("test", {dokCommand}, false):
    ls

  assert res == true
