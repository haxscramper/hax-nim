import macros
import tables
import strutils
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
  val2String = (proc(n: NimNode): string = n.treeRepr()),
  treeMaker = makeNimNode,
  functorKinds = functorNodes,
  constantKinds = constantNodes,
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

proc buildPatternDecl(
  node: NimNode, path: seq[int],
  subt: seq[NimNode], vars: var seq[string]): NimNode =
  # AST to declare part of the matcher rule
  if node.kind == nnkBracket and node[0].kind == nnkBracket:
    # Nested brackets are used to annotate variables
    let content = toSeq(node[0].children())
    let varStr = newStrLitNode($content[0])
    vars.add $content[0]
    return quote do:
      NodeTerm(tkind: tkVariable, name: `varStr`)

  elif node.kind == nnkIdent and node.strVal == "_":
    # Placeholder variable
    return quote do:
      NodeTerm(tkind: tkPlaceholder)
  else:
    # not placeholder, not variable => regular term or functor
    if node.kind == nnkCall and ($node[0])[0].isUpperAscii():
      let callSym = $node[0]
      let funcName = ident("nnk" & callSym)
      let funcEnum: NimNodeKind = parseEnum[NimNodeKind]("nnk" & callSym)

      if funcEnum in functorNodes:
        let subtMatchers = newTree(nnkBracket, subt.filterIt(not it.isNil))
        return quote do:
          NodeTerm(tkind: tkFunctor, functor: `funcName`, sons: @`subtMatchers`)
      else:
        var termValue: NimNode
        if callSym.endsWith("Lit"):
          let nodeTyle = callSym[0..^3]
          let valueLit = quote do: 12
          let valueSym = ident "value"
          termValue = quote do:
            ((let `valueSym` = `valueLit`; newLit(value)))

        elif callSym in @["CommentStmt", "Ident", "Sym"]:
          let strLit = node[1]
          let nodeMaker = ident("new" & callSym & "Node")
          let valueSym = ident "value"
          termValue = quote do:
            ((let `valueSym` = `strLit`; `nodeMaker`(`strLit`)))

        return quote do:
          NodeTerm(tkind: tkConstant, value: `termValue`)
    else:
      assert (node.kind notin {nnkStmtList}),
            "Unexpected element kind: " &
              $node.kind() & " lit: " & $node.toStrLit()


proc makePatternDecl(sectBody: NimNode): tuple[node: NimNode, vars: seq[string]] =
  var vars: seq[string]
  let ruleMatcherDef = mapItTreeDFS(
    subnodes, NimNode, sectBody,
    buildPatternDecl(it, path, subt, vars))

  return (node: ruleMatcherDef, vars: vars)


proc makeGeneratorDecl(sectBody: NimNode, vars: seq[string]): NimNode =
  ## Declare section for value generator
  let varDecls = collect(newSeq):
    for v in vars:
      let id = ident(v)
      let strl = newLit(v)
      quote do:
        let `id`: NimNode = env[`strl`].fromTerm()

  let varStmts = varDecls.newStmtList()

  let envs = ident "env"
  result = quote do:
    block:
      proc tmp(`envs`: NodeEnv): NodeTerm =
        `varStmts`
        let res {.inject.} =
          block:
            `sectBody`
 
        res.toTerm()

      tmp



macro makeNodeRewriteSystem(body: untyped): untyped =
  let rules = collect(newSeq):
    for node in body:
      if node.kind == nnkCall and node[0] == ident("rule"):
        node

  var matcherTuples: seq[NimNode]
  for rule in rules:
    let pattSection = toSeq(rule[1].children()).findItFirst(
      it[0].strVal() == "patt")[1][0]

    let (matcherDecl, varList) = makePatternDecl(pattSection)

    let genSection = toSeq(rule[1].children()).findItFirst(
      it[0].strVal() == "outp")[1].newStmtList()

    let generator = makeGeneratorDecl(genSection, varList)

    matcherTuples.add quote do:
      makeRulePair[string, NodeTerm](
        NodeMatcher(patt: `matcherDecl`, isPattern: true),
        `generator`
      )

  result = newTree(nnkBracket, matcherTuples)
  result = quote do:
    RedSystem[string, NodeTerm](rules: @`result`)


proc treeRepr*[Tree, Enum](
  term: CaseTerm[Tree, Enum],
  treeStr: proc(tree: Tree): string,
  depth: int = 0): string =

  let ind = "  ".repeat(depth)
  case term.tkind:
    of tkConstant:
      return treeStr(term.value).split("\n").mapIt(ind & "cst " & it).join("\n")
    of tkPlaceholder:
      return ind & "plh _"
    of tkVariable:
      return ind & "var " & term.name
    of tkFunctor:
      return ind & "fun " & $term.functor & "\n" &
        term.sons.mapIt(it.treeRepr(treeStr, depth + 1)).join("\n")



macro rewriteTest(body: untyped): untyped =
  let rewrite = makeNodeRewriteSystem:
    rule:
      patt: Call(Ident("hello"), [[other]])
      outp:
        let exprStr = ($other.toStrLit()).newLit()
        quote do:
          echo "calling proc hello with one argument"
          echo "expr: ", `exprStr`
          echo "argument value: ", `other`
          hello(`other`)

  let term = body.toTerm()
  let nodeTree = proc(n: NimNode): string = n.treeRepr()

  let reduced = reduce(
    term, rewrite, nimAstImpl
  )
  if reduced.ok:
    result = reduced.term.fromTerm()

proc hello(param: int) = echo param

rewriteTest:
  hello(12 + 999)
