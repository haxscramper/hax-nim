import macros
import sugar

import hmisc/[hterms_callback, hterms_tree, halgorithm, helpers]

const constantNodes =
  {
    nnkNone, nnkEmpty, nnkNilLit, # Empty node
    nnkCharLit..nnkUInt64Lit, # Int literal
    nnkFloatLit..nnkFloat64Lit, # Float literal
    nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym # Str lit
  }

const functorNodes = { low(NimNodeKind) .. high(NimNodeKind) } - constantNodes
proc subnodes(node: NimNode): seq[NimNode] = toSeq(node.children())

proc makeNimNode(kind: NimNodeKind, sons: seq[NimNode]): NimNode =
  return newTree(kind, sons)

type
  NodeTerm = CaseTerm[NimNode, NimNodeKind]
  NodeReduction* = RedSystem[string, NodeTerm]
  NodeMatcher* = TermMatcher[string, NodeTerm]
  NodeEnv* = TermEnv[string, NodeTerm]

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
  # TODO add proc for checking if futher recursion is not needed (trim
  # down arbitrary branches from tree)
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
    subnIdent = ident "subt"

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

proc makePatternDecl(sectBody: NimNode): tuple[node: NimNode, vars: seq[string]] =
  var vars: seq[string]
  let ruleMatcherDef = mapItTreeDFS(subnodes, NimNode, sectBody,
    block:
      # AST to declare part of the matcher rule
      var res: NimNode

      if it.kind == nnkBracket and it[0].kind == nnkBracket:
        # Nested brackets are used to annotate variables
        let content = toSeq(it[0].children())
        let varStr = newStrLitNode($content[0])
        vars.add $content[0]
        res = quote do:
          NodeTerm(tkind: tkVaariable, tname: `varStr`)

      elif it.kind == nnkIdent and it.strVal == "_":
        # Placeholder variable
        res = quote do:
          NodeTerm(tkind: tkPlaceholder)
      else:
        # not placeholder, not variable => regular term or functor
        let term = it.toTerm()
        case term.tkind:
          of tkFunctor:
            let funcSym = ident($term.functor)
            let subterms = newTree(nnkBracket, subt)
            res = quote do:
              NodeTerm(tkind: tkFunctor, tsym: `funcSym`, tsubt: @`subterms`)

          of tkConstant:
            discard
          else:
            discard

      res
    )

  return (node: ruleMatcherDef, vars: vars)


proc makeGeneratorDecl(sectBody: NimNode, vars: seq[string]): NimNode =
  ## Declare section for value generator
  discard

macro makeNodeRewriteSystem(body: untyped): untyped =
  let rules = collect(newSeq):
    for node in body:
      if node.kind == nnkCall and node[0] == ident("rule"):
        node

  for rule in rules:
    let pattSection = toSeq(rule[1].children()).findItFirst(it[0].strVal() == "patt")
    let (matcherDecl, varList) = makePatternDecl(pattSection)

    let genSection = toSeq(rule[1].children()).findItFirst(it[0].strVal() == "outp")
    let generator = makeGeneratorDecl(genSection, varList)





makeNodeRewriteSystem:
  rule:
    patt: Call(Ident("hello"), [[other]])
    outp:
      quote do:
        echo "calling proc hello with one argument"
        echo "argument value: ", `other`
        hello(`other`)

# dumpTree:
#   NodeMatcher(
#     patt: NodeTerm(tkind: tkFunctor, tsym: aopAdd, tsubt: @[
#       NodeTerm(tkind: tkVariable, tname: "A"),
#       NodeTerm(tkind: tkConstant, tval: 0)
#     ]),
#     isPattern: true
#   )
