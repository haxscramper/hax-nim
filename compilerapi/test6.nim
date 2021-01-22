import hmisc/other/oswrap
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


proc llStreamReader(s: PLLStream, buf: pointer, bufLen: int): int =
  echo "Reading stream ", bufLen
  result = 0

processModule(graph, m, llStreamOpenStdIn(llStreamReader))
