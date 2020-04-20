import colechopkg/lib
import helpers
import tables
import sequtils
import strformat
import os
export os
import strutils
import logging
import shell
import colechopkg/types

export shell

##[

.. contents::

   Panacea for paranoia

]##

## Collection of helper functions to provide verbose messages for
## common errors

const noShellMsg*: set[DebugOutputKind] = {}

var defLogIndentation {.inject.} = 0

proc die*() {.discardable, noreturn.} =
  ceUserInfo2("Terminating program", defLogIndentation)
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


#===========================  logging support  ===========================#

type
  LoggingConf = enum
    lcFileLogging
    lcPrintLogging

    lcUsePrefix
    lcUseFileName
    lcUseLine

  IInfo = tuple[filename: string, line: int, column: int]

var defLogPrefixMap: Table[string, string]
var defLogConfMap: Table[string, set[LoggingConf]]
var defLogLoggerMap: Table[string, FileLogger]
var defLogLevelMap: Table[string, Level]

proc increaseLogIndent*() = defLogIndentation += 4
proc decreaseLogIndent*() = defLogIndentation -= 4

template runIndentedLog*(body: untyped): untyped =
  increaseLogIndent()
  body
  decreaseLogIndent()

template getLogConf*(f: string): untyped =
  if defLogConfMap.hasKey(f):
    defLogConfMap[f]
  else:
    raise newException(
      AssertionError,
      "Attempt to use logging without init configuration"
    )

template getLogLevel*(f: string): untyped =
  if defLogLevelMap.hasKey(f):
    defLogLevelMap[f]
  else:
    raise newException(
      AssertionError,
      "Attempt to use loggint without init configuration"
    )

template saveLog*(text: string, logLevel = lvlAll): void =
  let f = instantiationInfo().filename
  if lcFileLogging in getLogConf(f):
    defLogLoggerMap[f].log(lvlAll, text)

template getDefPrefix(iinfo: IInfo): string =
  let file = iinfo.filename
  let confPrefix =
    if defLogPrefixMap.hasKey(file):
      defLogPrefixMap[file]
    else:
      ""

  (if lcUsePrefix in getLogConf(file): confPrefix & ": " else: "") &
  (
    block:
      if lcUseFileName in getLogConf(file):
        let (_, name, ext) = iinfo.filename.splitFile()
        "[" & name & "] "
      else:
        ""
  ) &
  (if lcUseLine in getLogConf(file): "(" & $iinfo.line & ") " else: "")

template showError*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")
  ceUserError0(getDefPrefix(
    iinfo = instantiationInfo()
  ) & text, defLogIndentation)
  saveLog(text, lvlError)

template showLog*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")
  ceUserLog0(getDefPrefix(
    iinfo = instantiationInfo()
  ) & text, defLogIndentation)
  saveLog(text, lvlDebug)

template showInfo*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")
  ceUserInfo2(getDefPrefix(
    iinfo = instantiationInfo()
  ) & text, defLogIndentation)
  saveLog(text, lvlInfo)

template showWarn*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")
  ceUserWarn(getDefPrefix(
    iinfo = instantiationInfo()
  ) & text, defLogIndentation)
  saveLog(text, lvlWarn)
  # fileLogger.log(lvlWarn, text)

template showPlain*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")
  echo text


template initDefense*(
  logfile: string = "",
  prefix: string = "",
  logPath: bool = false,
  logLine: bool = false
         ): untyped =
  ##[ Create necessary variables and procs for debugging.

  All settings and procedures are scope-local. `logile` is a name of
  the file to use for creating file logger - if empty (default) file
  logging is disabled. `prefix` is a prefix string used for log
  messages.

  .. code-block::
      initDefence(prefix = "FFF")
      showLog("hello", "world")
      showError("test", "ddd")

  .. code-block:: text
      >>> FFF: hello world
      !!! FFF: test ddd
  ]##

  let file = instantiationInfo().filename
  defLogConfMap[file] = {lcPrintLogging}

  if logfile != "":
    defLogConfMap[file].incl lcFileLogging

  if prefix != "":
    defLogConfMap[file].incl lcUsePrefix
    defLogPrefixMap[file] = prefix

  if logLine: defLogConfMap[file].incl lcUseLine
  if logPath: defLogConfMap[file].incl lcUseFileName

  if lcFileLogging in getLogConf(file):
    defLogLoggerMap[file] = newFileLogger(logfile)


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

template getCEx(t: untyped): untyped =
  cast[t](getCurrentException())

template pprintErr(body: untyped): untyped =
  template pprintStackTrace(): untyped =
    let e = getCurrentException()
    let choosenim = getHomeDir() & ".choosenim"

    for tr in e.getStackTraceEntries():
      let filename: string = $tr.filename

      if not filename.startsWith(choosenim):
        let (_, name, ext) = filename.splitFile()
        ceUserLog0(
          $name.toDefault(style = { styleDim }) &
            ":" &
            $($tr.line).toDefault(style = { styleUnderscore }) &
            " " &
            $($tr.procname).toYellow())

    let idx = e.msg.find('(')
    echo ""
    ceUserError0(e.msg[(if idx > 0: idx else: 0)..^1])

  try:
    body
  except:
    pprintStackTrace()

proc e() = raise newException(AssertionError, """
Ass SeRtIoN eRrOr with easf sadfas
Ass SeRtIoN eRrOr with easf sadfas
Ass SeRtIoN eRrOr with easf sadfas
Ass SeRtIoN eRrOr with easf sadfas
dfsdf as dfa sdf asdf asd fa sdf adsf as df
as f sadf asdf w f dsvafgd aerfg a rgffd ga
dfsdf as dfa sdf asdf asd fa sdf adsf as df
as f sadf asdf w f dsvafgd aerfg a rgffd ga
dfsdf as dfa sdf asdf asd fa sdf adsf as df
as f sadf asdf w f dsvafgd aerfg a rgffd ga
dfsdf as dfa sdf asdf asd fa sdf adsf as df
as f sadf asdf w f dsvafgd aerfg a rgffd ga
asd asdf asdfasdf sf sadf asdf werfdfasdf waf asd fasdf
fa""")
proc d() = e()
proc c() = d()
proc b() = c()
proc a() = b()

when isMainModule:
  setCurrentDir("/tmp")
  initDefense(prefix = "FFF: ")
  showWarn("hello", "world")
  showError("test", "ddd")

  let res = safeRunCommand("test", {dokCommand}, false):
    ls

  assert res == true
  pprintErr():
    a()
