import std/[os, strformat]
import Nim / compiler /
  [ idents, options, modulegraphs, passes, lineinfos, sem, pathutils, ast,
    astalgo, modules, condsyms, passaux
  ]

let file = "/tmp/ee.nim"

file.writeFile("""
proc test*(arg: int) = echo arg
type A* = int
macro nice() = echo "test"
""")


var graph: ModuleGraph = block:
  var
    cache: IdentCache = newIdentCache()
    config: ConfigRef = newConfigRef()

  let path = getCurrentDir() / "Nim/lib"

  config.libpath = AbsoluteDir(path)
  config.searchPaths.add config.libpath
  config.projectFull = AbsoluteFile(file)
  initDefines(config.symbols)

  newModuleGraph(cache, config)


type
  CustomContext = ref object of PPassContext
    module: PSym

proc passOpen(graph: ModuleGraph; module: PSym): PPassContext =
  CustomContext(module: module)

proc passNode(c: PPassContext, n: PNode): PNode =
  result = n
  if sfMainModule in CustomContext(c).module.flags:
    echo n.kind

proc passClose(graph: ModuleGraph; p: PPassContext, n: PNode): PNode =
  discard


registerPass(graph, semPass)
registerPass(graph, makePass(passOpen, passNode, passClose))
compileProject(graph)
