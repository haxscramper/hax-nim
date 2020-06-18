import macros
import sugar
import strformat, strutils, sequtils, algorithm
import hmisc/[defensive, helpers]
import sets
import grammars

export helpers

import hashes, tables, sets
export tables, sets

import parser_common
export parser_common

import lexer
export lexer

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
    sets.first[rule.nterm] = compPatt.first(sets)
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
  sets: NTermSets[TKind],
  resName: string = "res"): NimNode

proc makeAltBlock[TKind](
  alt: CompPatt[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind]): NimNode =
  ## Create code block for parsing alternative pattern
  assert alt.kind == pkAlternative
  let toksIdent = ident conf.toksIdent
  result = nnkCaseStmt.newTree(quote do: `toksIdent`.peek().kind)

  for idx, patt in alt.patts:
    let resName = &"patt{idx}res"
    let resIdent = ident resName
    let parseBlock = makeParseBlock(patt, conf, sets, resName)
    result.add nnkOfBranch.newTree(
      makeSetLiteral(patt.first(sets)),
      quote do:
        `parseBlock`
        `resIdent`
    )

  let elseBody = quote do:
    raise CodeError(msg: "Unexpected token")

  result.add nnkElse.newTree(elsebody)

proc makeParserName(sym: NTermSym): string =
  ## Converter nonterminal name into parsing proc name
  "parse" & sym.capitalizeAscii()

proc makeTermBlock[TKind](term: CompPatt[TKind], conf: CodeGenConf): NimNode =
  assert term.kind == pkTerm
  let toksIdent = ident(conf.toksIdent)
  let tokIdent = ident($term.tok)
  let tokType = ident "Tok"
  return quote do:
    let tok = `toksIdent`.next()
    assert tok.kind == `tokIdent`
    # ParseTree[`tokType`](kind: pkTerm, tok: tok)
    newTree(tok)

proc makeNTermBlock[TKind](nterm: CompPatt[TKind], conf: CodeGenConf): NimNode =
  assert nterm.kind == pkNTerm
  let ntermIdent = ident(makeParserName(nterm.sym))
  let lexerIdent = ident(conf.toksIdent)
  let tree = ident "tree"
  quote do:
    var ntermTree: typeof(`tree`)
    `ntermIdent`(`lexerIdent`, ntermTree)
    ntermTree

proc makeConcatBlock[TKind](
  nterm: CompPatt[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind]): NimNode =
  assert nterm.kind == pkConcat
  let parseStmts = collect(newSeq):
    for idx, patt in nterm.patts:
      makeParseBlock(patt, conf, sets, &"patt{idx}res")

  let valueVars = nnkBracket.newTree(
    nterm.patts
      .enumerate()
      .mapIt(ident &"patt{it[0]}res")
  )

  let tokIdent = ident "Tok"
  return (parseStmts & @[
    quote do:
      newTree[`tokIdent`](
        kind = pkConcat,
        @`valueVars`
      )
      # ParseTree[`tokIdent`](
      #   kind: pkConcat,
      #   values: @`valueVars`
      # )
  ]).newStmtList()

proc makeNtoMTimesBlock[TKind](
  nterm: CompPatt[TKind], conf: CodeGenConf, sets: NtermSets[TKind],
  mintimes, maxtimes: int): NimNode =
  assert nterm.kind in {pkZeroOrMore, pkOneOrMore, pkOptional}
  let
    toksIdent = ident(conf.toksIdent)
    laLiteral = makeSetLiteral(nterm.first(sets))
    bodyParse = makeParseBlock(nterm.opt[0], conf, sets, "itemRes")
    minLit = newLit(mintimes)
    maxLit = newLit(maxtimes)
    cnt = ident("cnt")
    tokType = ident("Tok")
    itemIdent = ident("itemRes")
    kindLiteral = ident($nterm.kind)
    subItems = ident "subItems"

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

  let finalValue =
    if nterm.kind == pkOptional:
      quote do:
        if subItems.len == 1:
          ParseTree[`tokType`](kind: pkOptional, optValue: some(`subItems`[0]))
        else:
          ParseTree[`tokType`](kind: pkOptional)
    else:
      quote do:
        newTree[`tokType`](
          kind = `kindLiteral`,
          `subItems`
        )
        # ParseTree[`tokType`](kind: `kindLiteral`, values: `subItems`)


  return quote do:
    # TEST WARNING possible variable shadowing if parsing rule
    # contains nested `{N,M}` rules.
    var `cnt` = 0
    var `subItems`: seq[ParseTree[`tokType`]]
    while `countConstraints` and `toksIdent`.peek().kind in `laLiteral`:
      `bodyParse`
      inc `cnt`
      `subItems`.add `itemIdent`

    `minNumAssert`
    `finalValue`



proc makeParseBlock[TKind](
  patt: CompPatt[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind],
  resName: string = "res"): NimNode =
  ## Generate code block to parse pattern `patt`.
  result = case patt.kind:
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

  let
    resIdent = ident resName
    argTree = ident "tree"
  return newStmtList(
    newCommentStmtNode($patt & " " & $patt.kind),
    quote do:
      let `resIdent` = block:
        `result`

      `argTree` = `resIdent`
    )

proc makeRuleParser[TKind](
  rule: CompRule[TKind],
  conf: CodeGenConf,
  sets: NTermSets[TKind]): tuple[decl, impl: NimNode] =
  ## Generate implementation for proc to parse rule
  let procName = ident(rule.nterm.makeParserName())
  let toks = ident(conf.toksIdent)
  let parser = ident(conf.parsIdent)
  let tree = ident "tree"

  let decl = quote do:
    # Declare procedure to parse `rule`. `toks` is instance of token
    # stream used to get lookahead.
    proc `procName`[Tok](`toks`: var TokStream[Tok], `tree`: var ParseTree[Tok])

  let parseBody = rule.patts.makeParseBlock(conf, sets)
  let impl = quote do:
    proc `procName`[Tok](`toks`: var TokStream[Tok], `tree`: var ParseTree[Tok]) =
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

  result = newStmtList(
    decls.newStmtList(),
    impls.newStmtList()
  )
