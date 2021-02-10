import compiler/[passes, ast]
import std/[tables]

type
  SemLogEntry = object
    actionName: string
    resultNode: PNode

  SemLogContext = ref object of PPassContext
    expansionPairs: Table[PNode, SemLogEntry]

proc recordSemAction*(
    ctx: PPassContext,
    actionName: string,
    originalNode, resultNode: PNode) =

  SemLogContext(ctx).expansionPairs[originalNode] = SemLogEntry(
    resultNode: resultNode,
    actionName: actionName
  )
