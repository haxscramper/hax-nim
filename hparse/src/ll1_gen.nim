import macros
import sugar
import strformat, strutils, sequtils, algorithm
import hmisc/[defensive, helpers]
import sets
import grammars

import hashes, tables, sets
export tables, sets

import parser_common
export parser_common

import lexer
export lexer


type
  LogicError = ref object of CatchableError



func topoSort[Vertex](
  verts: openarray[Vertex],
  deps: proc(vert: Vertex): seq[Hash],
  idgen: proc(vert: Vertex): Hash,
  revese: bool = true): seq[Vertex] =

  runnableExamples:
    assert @[3, 2, 1] == topoSort(
      verts = @[1, 2, 3],
      deps = proc(v: int): seq[Hash] =
                 case v:
                   of 1: @[Hash(2), Hash(3)]
                   of 2: @[Hash(3)]
                   else: @[]
    )

  var adjList: Table[Hash, HashSet[Hash]]
  var vertData: Table[Hash, Vertex]
  var inCounts: Table[Hash, int]

  for vert in verts:
    let depsList = deps(vert)
    let vHash = idgen(vert)

    adjList[vHash] = depsList.toHashSet()
    vertData[vHash] = vert


    for dep in depsList:
      # For each dependency - increase number of items that depend on this one
      inc inCounts.mgetOrPut(dep, 0)

  let counts: seq[(Hash, int)] = adjList.mapPairs(
    (lhs in inCounts).tern((lhs, inCounts[lhs]), (lhs, 0))
  ).filterIt(it[1] == 0)

  assert counts.len > 0,
      "Graph contains no vertices that have zero in-edges"

  var noincoming: seq[Hash] = counts.mapIt(it[0])
  var sortednodes: seq[Hash]

  while noincoming.len > 0:
    let node = noincoming.pop()
    sortednodes.add node
    # For all adjacent
    for adj in toSeq(adjList[node]):
      # Remove edge `(adj, node)`
      adjList[node].excl adj
      # Decrease number of incoming edges for `adj`
      dec inCounts[adj]


      # If has no incoming
      if inCounts[adj] == 0:
        noincoming.add adj

  for vert, adj in adjList:
    if adj.len > 0:
      raise LogicError(msg: "Cannot perform topological sort on graph with cycles")

  if revese:
    return sortednodes.reversed().mapIt(vertData[it])
  else:
    return sortednodes.mapIt(vertData[it])


func topoSort[Vertex](
  verts: openarray[Vertex],
  deps: proc(vert: Vertex): seq[Hash],
  revese: bool = true): seq[Vertex] =
  topoSort(verts, deps, reverse, (r) => hash(r))

# Only `kind` for token is considered while parsing - some additional
# information can be generated by lexer (string value for token,
# start/end position etc.). This information is not used in LL(1)
# parser.


proc computeFirst[TKind](patt: Patt[TKind], other: NTermSets[TKind]): FirstSet[TKind] =
  ## Generate FIRST set for `patt`
  case patt.kind:
    of pkTerm:
      result.incl patt.tok
    of pkConcat:
      result.incl computeFirst(patt.patts[0], other)
    of pkAlternative:
      for p in patt.patts:
        result.incl computeFirst(p, other)
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      result.incl computeFirst(patt.opt, other)
    of pkNterm:
      result.incl other.first[patt.sym]

proc computePatt[TKind](patt: Patt[TKind], sets: NTermSets[TKind]): CompPatt[TKind] =
  ## Generate FIRST set for pattern `patt`
  let kind = patt.kind
  case kind:
    of pkTerm:
      result = CompPatt[TKind](kind: pkTerm, tok: patt.tok)
      result.first.incl patt.tok
    of pkConcat:
      result = CompPatt[TKind](kind: pkConcat, patts: patt.patts.mapIt(computePatt(it, sets)))
      result.first.incl computeFirst(patt.patts[0], sets)
    of pkAlternative:
      result = CompPatt[TKind](kind: pkConcat, patts: patt.patts.mapIt(computePatt(it, sets)))
      for p in patt.patts:
        result.first.incl computeFirst(p, sets)
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      result = CompPatt[TKind](kind: kind, opt: @[computePatt(patt.opt, sets)])
      result.first.incl computeFirst(patt.opt, sets)
    of pkNterm:
      # FIRST sets for nonterminals are stored in `sets`
      result = CompPatt[TKind](kind: pkNterm, sym: patt.sym)

template doIt(s, action: untyped): untyped =
  type Item = type((s[0]))
  for it {.inject.} in s:
    action

  s


proc necessaryTerms[TKind](rhs: Patt[TKind]): seq[NTermSym] =
  ## Generate list of nonterminals that might appear at rhs of production
  case rhs.kind:
    of pkAlternative:
      return rhs.patts.filterIt(it.kind == pkNTerm).mapIt(it.sym)
    of pkNTerm:
      return @[rhs.sym]
    of pkConcat:
      return necessaryTerms(rhs.patts[0])
    else:
      return @[]

proc computeGrammar*[TKind](g: Grammar[TKind]
  ): CompGrammar[TKind] =
  ## Generate first/follow sets for all rules in grammar. Rules in
  ## resulting grammar are ordered based on topological sorting.
  var sets: NTermSets[TKind]
  # Just because I can sqeeze it into <= 4 lines does not mean that it
  # is a good idea. But code above performs topological sort of the
  # shole grammar based on which terms depend on which. If there is a
  # cycle in grammar exception is thrown - it means grammar is
  # left-recursive. (REVIEW: check if left recursion cannot occur from
  # some other type of grammar)

  let sortedRules = g.rules.topoSort(
    deps = ((r) => (r.patts.necessaryTerms().mapIt(it.hash))),
    idgen = ((r) => hash(r.nterm))
  )

  for rule in sortedRules:
    let compPatt = computePatt(rule.patts, sets)
    sets.first[rule.nterm] = compPatt.first
    result.rules.add CompRule[TKind](nterm: rule.nterm, patts: compPatt)

  result.sets = sets

proc makeSetLiteral[T](s: set[T]): NimNode =
  ## Create new set literal
  result = nnkCurly.newTree()
  for elem in s:
    result.add ident($elem)

type
  CodeGenConf = object
    laIdent: string
    toksIdent: string ## Identifier name for token stream object
    parsIdent: string ## Identifier name for parser object

proc makeParseBlock[TKind](
  patt: CompPatt[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind]): NimNode

proc makeAltBlock[TKind](
  alt: CompPatt[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind]): NimNode =
  ## Create code block for parsing alternative pattern
  assert alt.kind == pkAlternative
  result = nnkCaseStmt.newTree(ident conf.laIdent)
  for patt in alt.patts:
    result.add nnkOfBranch.newTree(
      makeSetLiteral(patt.first),
      makeParseBlock(patt, conf, sets)
    )

  let elseBody = quote do:
    raise CodeError(msg: "Unexpected token")

  result.add nnkElse.newTree(elsebody)

  echo "generated parser for alt block"

proc makeParserName(sym: NTermSym): string =
  ## Converter nonterminal name into parsing proc name
  "parse" & sym.capitalizeAscii()

proc makeTermBlock[TKind](term: CompPatt[TKind], conf: CodeGenConf): NimNode =
  assert term.kind == pkTerm
  let toksIdent = ident(conf.toksIdent)
  let tokIdent = ident($term.tok)
  return quote do:
    `toksIdent`.peek().kind == `tokIdent`

proc makeNTermBlock[TKind](nterm: CompPatt[TKind], conf: CodeGenConf): NimNode =
  assert nterm.kind == pkNTerm
  let ntermIdent = ident(makeParserName(nterm.sym))
  let lexerIdent = ident(conf.toksIdent)
  quote do:
    `ntermIdent`(`lexerIdent`)

proc makeConcatBlock[TKind](
  nterm: CompPatt[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind]): NimNode =
  assert nterm.kind == pkConcat
  let parseStmts = collect(newSeq):
    for patt in nterm.patts:
      makeParseBlock(patt, conf, sets)

  return parseStmts.newStmtList()

proc makeNtoMTimesBlock[TKind](
  nterm: CompPatt[TKind], conf: CodeGenConf, sets: NtermSets[TKind],
  mintimes, maxtimes: int): NimNode =
  assert nterm.kind in {pkZeroOrMore, pkOneOrMore, pkOptional}
  let
    toksIdent = ident(conf.toksIdent)
    laLiteral = makeSetLiteral(nterm.first)
    bodyParse = makeParseBlock(nterm.opt[0], conf, sets)
    minLit = newLit(mintimes)
    maxLit = newLit(maxtimes)
    cnt = ident("cnt")

  let countConstraints =
    if maxtimes > 0:
      nnkPar.newTree(nnkInfix.newTree(
        ident "<",
        ident "cnt",
        maxLit))
    else:
      ident("true")

  let minNumAssert =
    if mintimes > 0:
      quote do:
        if `cnt` < `minLit`:
          raise SyntaxError(
            msg: "Expected at least " & $(`minLit`) & " elements but found only " & $`cnt`
          )
    else:
      newEmptyNode()


  return quote do:
    # TEST WARNING possible variable shadowing if parsing rule
    # contains nested `{N,M}` rules.
    var `cnt` = 0
    while `countConstraints` and `toksIdent`.peek().kind in `laLiteral`:
      `bodyParse`
      inc `cnt`
      `minNumAssert`



proc makeParseBlock[TKind](
  patt: CompPatt[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind]): NimNode =
  ## Generate code block to parse pattern `patt`.
  echo "dispatching ", patt.kind
  return case patt.kind:
    of pkTerm:
      makeTermBlock(patt, conf)
    of pkOptional:
      makeNtoMTimesBlock(patt, conf, sets, 0, 1)
    of pkNterm:
      makeNTermBlock(patt, conf)
    of pkAlternative:
      makeAltBlock(patt, conf, sets)
    of pkConcat:
      makeConcatBlock(patt, conf, sets)
    of pkZeroOrMore:
      makeNtoMTimesBlock(patt, conf, sets, 0, -1)
    of pkOneOrMore:
      makeNtoMTimesBlock(patt, conf, sets, 1, -1)

proc makeRuleParser[TKind](
  rule: CompRule[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind]): tuple[decl, impl: NimNode] =
  ## Generate implementation for proc to parse rule
  let procName = ident(rule.nterm.makeParserName())
  let toks = ident(conf.toksIdent)
  let parser = ident(conf.parsIdent)

  let decl = quote do:
    # Declare procedure to parse `rule`. `toks` is instance of token
    # stream used to get lookahead.
    proc `procName`[Tok](`toks`: var TokStream[Tok])

  let parseBody = rule.patts.makeParseBlock(conf, sets)
  let impl = quote do:
    proc `procName`[Tok](`toks`: var TokStream[Tok]) =
      `parseBody`

  return (decl: decl, impl: impl)


proc makeGrammarParser*[TKind](
  gram: CompGrammar[TKind],
  conf: CodeGenConf =
      CodeGenConf(
        toksIdent: "toks",
        parsIdent: "pars"
      )): NimNode =

  ## Generate code for parsing grammar `gram`
  var decls: seq[NimNode]
  var impls: seq[NimNode]
  for rule in gram.rules:
    let (decl, impl) = makeRuleParser(rule, conf, gram.sets)
    decls.add decl
    impls.add impl

  let declsBlock = decls.newStmtList()
  let implsBlock = impls.newStmtList()

  echo $declsBlock.toStrLit()
  echo $implsBlock.toStrLit()
