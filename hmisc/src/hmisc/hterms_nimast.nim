import macros
import tables
import strutils
import sugar

import hmisc/[hterms_callback, hterms_tree, halgorithm, helpers]

export hterms_callback, hterms_tree, macros

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

defineTermSystemFor(
  treeType = NimNode, # Use `NimNode` for case term value type
  enumType = NimNodeKind, # Use `NimNodeKind` as functor
  kindField = kind, # Use `kind` command for accessing node fields
  sonsField = children, # Use `children` command for accessing noe fields
  implName = nimAstImpl, # Implementation const will be called `nimAstImpl`

  # To avoid exceptions for non-strlit nodes use this proc to get
  # string for nim node
  val2String = (proc(n: NimNode): string = n.treeRepr()),

  # Procedure to create new instance of nim node
  treeMaker = makeNimNode,

  # Node kinds that should be considered functors
  functorKinds = functorNodes,

  # Node kinds that should be considered 'constants'
  constantKinds = constantNodes,
)


proc buildPatternDecl(
  node: NimNode, path: seq[int],
  subt: seq[NimNode], vars: var seq[string]): NimNode =
  ## Convert pattern declaraion into pattern matcher.
  ## 
  ## :params:
  ##    :node: current node to convert into pattern matchers
  ##    :path: path for current node
  ##    :subt: List of subterms for which patterns have already been
  ##           created
  ##    :vars: List of variables discovered during tree traversal.
  ##           Newfound variables are appended to it.  
  # IDEA TODO write generalized version of this proc for generatic
  # patern matchers for any kind of homogenous AST wit enum as functor

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
        # Genreate matcher for functor
        let subtMatchers = newTree(nnkBracket, subt.filterIt(not it.isNil))
        return quote do:
          NodeTerm(
            tkind: tkFunctor, # Will match functor nodes in tree
            functor: `funcName`, # Literal value for functor kind
            sons: @`subtMatchers` # Wrap all subnode matchers into sequence
          )
      else:
        # Literal value. It is necessary to implement two-level hop
        # with values: *generate code* that will generate NimNode
        # literals.
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


proc makePatternDecl(
  sectBody: NimNode): tuple[node: NimNode, vars: seq[string]] =
  ## Declare pattern matcher for section body
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



macro makeNodeRewriteSystem*(body: untyped): untyped =
  ## Create term rewriting system instance for nim node ast
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
  ## Generate tree representation for `CaseTerm`

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
