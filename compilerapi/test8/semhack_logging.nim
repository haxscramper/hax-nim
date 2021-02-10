import compiler/[passes, ast, modulegraphs]
import std/[tables, options, hashes]

type
  SemLogEntry = object
    actionName: string
    resultNode: PNode

  SemLogContext = ref object of PPassContext
    expansionPairs: Table[PNode, SemLogEntry]

proc hash*(node: PNode): Hash = hash(node.id)

proc recordSemAction*(
    ctx: PPassContext,
    actionName: string,
    originalNode, resultNode: PNode) =

  SemLogContext(ctx).expansionPairs[originalNode] = SemLogEntry(
    resultNode: resultNode,
    actionName: actionName
  )
