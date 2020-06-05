import macros
import sugar

import hmisc/[hterms_callback, hterms_tree, halgorithm]

const constantNodes =
  {
    nnkNone, nnkEmpty, nnkNilLit, # Empty node
    nnkCharLit..nnkUInt64Lit, # Int literal
    nnkFloatLit..nnkFloat64Lit, # Float literal
    nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym # Str lit
  }

const functorNodes = { low(NimNodeKind) .. high(NimNodeKind) } - constantNodes

proc makeNimNode(kind: NimNodeKind, sons: seq[NimNode]): NimNode =
  return newTree(kind, sons)

type
  NodeReduction* = RedSystem[string, CaseTerm[NimNode, NimNodeKind]]
  NodeEnv* = TermEnv[string, CaseTerm[NimNode, NimNodeKind]]

defineTermSystemFor[NimNode, NimNodeKind](
  kindField = kind,
  sonsField = children,
  implName = nimAstImpl,
  treeMaker = makeNimNode,
  functorKinds = functorNodes,
  constantKinds = constantNodes
)

proc mapDFSpost[InTree, OutTree](
  tree: InTree,
  map: proc(n: InTree, path: seq[int], subn: seq[OutTree]): OutTree,
  getSubnodes: proc(tree: InTree): seq[InTree],
  path: seq[int] = @[0]
                               ): OutTree =
  ## Convert one tree type into another using post order DFS traversal
  let nodeRes: seq[OutTree] = collect(newSeq):
    for idx, node in getSubnodes(tree):
      mapDFSpost(node, map, getSubnodes, path & @[idx])

  return map(tree, path, nodeRes)

macro mapItTreeDFS(
  subnodeCall, outType, inTree, op: untyped): untyped =
  runnableExamples:
    type
      InTest = object
        val: int
        sub: seq[InTest]

      OutTest = object
        val: string
        sub: seq[OutTest]

    block:
      let tmp = InTest()
      echo mapItTreeDFS(
        sub, OutTest, InTest(),
        OutTest(val: $it.val & "+"))

  let
    itIdent = ident "it"
    pathIdent = ident "path"
    subnIdent = ident "subn"

  quote do:
    mapDFSpost(
      `inTree`,
      map =
        proc(
          `itIdent`: typeof(`inTree`), `pathIdent`: seq[int],
          `subnIdent`: seq[`outType`]): `outType` =
            `op`
      ,
      getSubnodes = proc(node: typeof(`inTree`)): seq[typeof(`inTree`)] =
                      node.`subnodeCall`
    )



macro makeNodeRewriteSystem(body: untyped): untyped =
  let rules = collect(newSeq):
    for node in body:
      if node.kind == nnkCall and node[0] == ident("rule"):
        node

  for rule in rules:
    for section in rule[1]:
      if section.kind == nnkCall:
        let sectBody = section[1]
        case section[0].strVal():
          of "patt":
            echo "found pattern"
          of "outp":
            echo "found output"

        echo sectBody.treeRepr()


makeNodeRewriteSystem:
  rule:
    patt: Call(Ident("hello"), [[other]])
    outp:
      echo "calling proc hello with one argument"
      Call(Ident("hello"), [[other]])
