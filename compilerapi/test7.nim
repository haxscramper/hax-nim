import common

import std/[httpclient, strformat, uri, strutils, deques, times]
import compiler/[
  nimeval, llstream, ast, renderer, modulegraphs, vmdef, vm,
  passes, idents, options, lineinfos
]

import hnimast/hast_common

import hmisc/other/[oswrap, hjson]
import fusion/matching
import hpprint
import test6

let url = "https://raw.githubusercontent.com/nim-lang/packages/master/packages.json"
let file = RelFile("packages.json")

let client = newHttpClient()

if not exists(file):
  client.downloadFile(url, file.string)


proc implementRoutine*(
    graph: ModuleGraph;
    pkg, module, name: string;
    impl: proc (a: VmArgs) {.closure, gcsafe.}
  ) =

  let vm = PCtx(graph.vm)
  vm.registerCallback(pkg & "." & module & "." & name, impl)

var requires: seq[string]

var (graph, m) = newGraphStdin(withEval = false)

graph.config.structuredErrorHook =
  proc(config: ConfigRef; info: TLineInfo; msg: string; level: Severity) =
    echo msg
    echo info

type
  CutoutContext = ref object of PPassContext
    module: PSym

# graph.registerPass(makePass(
#   (
#     proc(graph: ModuleGraph, module: PSym): PPassContext {.nimcall.} =
#       return CutoutContext(module: module)
#   ),
#   (
#     proc(c: PPassContext, n: PNode): PNode {.nimcall.} =

#       var ctx = CutoutContext(c)

#       if sfMainModule in ctx.module.flags:
#         result = n

#       else:
#         result = nnkDiscardStmt.newPTree()
#   ),
#   (
#     proc(graph: ModuleGraph; p: PPassContext, n: PNode): PNode {.nimcall.} =
#       discard
#   )
# ))

graph.registerPass(evalPass)

graph.implementRoutine("*", "stdin", "requires",
  proc(args: VmArgs) {.nimcall, gcsafe.} =
    # echo "\e[32m###################\e[39m"
    for idx in 0 ..< args.rc - 1:
      # echo treeRepr(args.getNode(idx))
      for node in args.getNode(idx):
        requires.add node.getStrVal()
)



proc processCode(graph: ModuleGraph, m: PSym, str: string) =
  var lineQue = initDeque[string]()
  for line in split(str, '\n'):
    lineQue.addLast line

  proc scriptReader(s: PLLStream, buf: pointer, bufLen: int): int =
    return readLine(lineQue, s, buf, bufLen)

  processModule(graph, m, llStreamOpenStdIn(scriptReader))

graph.processCode(
  m,
  """
proc requires*(args: varargs[string]) = discard
""")

var cnt = 0
var stats: tuple[downTime, evalTime, totalTime: float]
for pack in parseJson(file):
  if { "name" : (asStr: @name), "web" : (asStr: @web) } ?= pack:
    stats.totalTime = cpuTime()
    let nimble = &"{web}/{name}.nimble"
    var raw = parseUri(nimble)

    if raw.hostname != "github.com": continue

    inc cnt; if cnt > 20: break

    raw.hostname = "raw.githubusercontent.com"
    let spl = split(raw.path, "/")
    raw.path = join(spl[0 .. 2] & @["master"] & spl[3 ..^ 1], "/")

    try:
      stats.downTime = cpuTime()
      var content = client.getContent($raw)
      stats.downTime = cpuTime() - stats.downTime

      stats.evalTime = cpuTime()
      content = "proc requires*(args: varargs[string]) = discard\n" &
        content

      try:
        graph.processCode(m, content)
        stats.evalTime = cpuTime() - stats.evalTime
        stats.totalTime = cpuTime() - stats.totalTime

        proc toMs(s: float): int = int(s * 1000)
        echo &"""
name          : {name}
requires      : {requires}
download time : {stats.downTime.toMS()} ms
eval time     : {stats.evalTime.toMS()} ms
total time    : {stats.totalTime.toMS()} ms
"""

      except:
        discard
        # echo getCurrentExceptionMsg()
        # echo content

      requires = @[]

    except HttpRequestError as e:
      discard

echo "Finished"
