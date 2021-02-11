import compiler/[passes, ast, modulegraphs, semdata, renderer]
import std/[tables, options, hashes]

type
  SemLogEntry* = object
    actionName: string
    resultNode: PNode

  SemLogContext* = ref object of PContext
    expansionPairs: Table[PNode, SemLogEntry]

proc hash*(node: PNode): Hash = hash(unsafeAddr node)

proc recordSemExpansion*(
    ctx: PPassContext,
    actionName: string,
    originalNode, resultNode: PNode) =

  echo "-->"
  echo originalNode
  echo "<--"
  echo resultNode
  echo "---"
  echo ""


  SemLogContext(ctx).expansionPairs[originalNode] = SemLogEntry(
    resultNode: resultNode,
    actionName: actionName
  )

proc newLogContext*(graph: ModuleGraph, module: PSym): PContext =
  result = SemLogContext()
  var context = newContext(graph, module)
  for name, resField, subField in fieldPairs(
    PContext(result)[], context[]
  ):
    resField = subField
  # result = SemLogContext()
