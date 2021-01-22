import hmisc/other/oswrap
import std/deques
import compiler/[
  ast, astalgo, modules, passes, condsyms,
  options, sem, semdata, llstream, vm, vmdef,
  modulegraphs, idents, pathutils, passaux,
  scriptconfig
]

import common

let searchPaths = toSeqString(@[
    stdlib,
    stdlib / "pure",
    stdlib / "core",
    stdlib / "pure" / "collections"
])

var conf = newConfigRef()
var cache = newIdentCache()
var graph = newModuleGraph(cache, conf)

for p in searchPaths:
  conf.searchPaths.add(AbsoluteDir p)
  if conf.libpath.isEmpty:
    conf.libpath = AbsoluteDir p

conf.cmd = cmdInteractive

conf.errorMax = high(int)

initDefines(conf.symbols)

defineSymbol(conf.symbols, "nimscript")
defineSymbol(conf.symbols, "nimconfig")

registerPass(graph, semPass)
registerPass(graph, evalPass)

var m = graph.makeStdinModule()
incl(m.flags, sfMainModule)
graph.vm = setupVM(m, cache, "stdin", graph)
graph.compileSystemModule()


var lineQue = initDeque[string]()
lineQue.addLast "echo 12"
lineQue.addLast "echo 90-" # Malformed AST
lineQue.addLast "echo 90"
lineQue.addLast "proc userProc() = discard"
lineQue.addLast "proc userProc() = discard"
lineQue.addLast "echo 100"

proc llStreamReader(s: PLLStream, buf: pointer, bufLen: int): int =
  # `compiler/passes/processModule()` has two nested `while true` loops -
  # outermost one repeatedly calls `openParser()` on input text, and
  # internal one processes all toplevel statements. After inner loop
  # finishes, input stream is checked for `if s.kind != llsStdIn: break`,
  # meaning it is not possible to break out of the loop freely. It might be
  # possible to implement in async manner though, I'm not entirely sure.
  if lineQue.len == 0:
    return 0


  s.s = ""
  s.rd = 0
  var line = lineQue.popFirst()

  add(s.s, line)

  inc(s.lineOffset)
  result = min(bufLen, len(s.s) - s.rd)

  if result > 0:
    copyMem(buf, addr(s.s[s.rd]), result)
    inc(s.rd, result)

processModule(graph, m, llStreamOpenStdIn(llStreamReader))