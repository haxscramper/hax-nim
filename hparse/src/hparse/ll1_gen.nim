import macros, options, sugar
import strformat, strutils, sequtils, algorithm
import hmisc/[defensive, helpers]
import sets
import grammars

export helpers

import hashes, tables, sets

import lexer
import parse_primitives, parser_common, parse_tree, parse_helpers


## LL1 parser generator code



template doIt(s, action: untyped): untyped =
  ## Execute action for each element in sequence, return original
  ## action.
  type Item = type((s[0]))
  for it {.inject.} in s:
    action

  s

type
  FirstSet*[TKind] = set[TKind]
  NTermSets*[TKind] = object
    first*: Table[NTermSym, FirstSet[TKind]]

  CompPatt*[TKind] = object
    action*: TreeAct
    first: FirstSet[TKind]
    case kind*: PattKind
      of pkNterm:
        nterm*: NTermSym ## Nonterminal to parse
      of pkTerm:
        tok*: TKind ## Single token to match literally
      of pkAlternative, pkConcat:
        patts*: seq[CompPatt[TKind]]
      of pkOptional, pkZeroOrMore, pkOneOrMore:
        opt*: seq[CompPatt[TKind]] ## Single instance that will be repeated
        # I could've used `Option[]` but decided to go with `seq`
        # since I should not have a situation where `opt` field is
        # `none` - it is just a workaround to allow recursive field


  CompRule*[TKind] = object
    nterm*: NTermSym
    patts*: CompPatt[TKind]

  CompGrammar*[TKind] = object
    sets*: NTermSets[TKind]
    rules*: seq[CompRule[TKind]]

#=========================  FIRST set computat  ==========================#

proc computeFirst*[TKind](
  patt: Patt[TKind], other: NTermSets[TKind]): FirstSet[TKind] =
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
      result.incl other.first[patt.nterm]

proc computePatt*[TKind](
  patt: Patt[TKind], sets: NTermSets[TKind]): CompPatt[TKind] =
  ## Generate FIRST set for pattern `patt`
  let kind = patt.kind
  case kind:
    of pkTerm:
      result = CompPatt[TKind](kind: pkTerm, tok: patt.tok)
      result.first.incl patt.tok
    of pkConcat:
      result = CompPatt[TKind](
        kind: pkConcat, patts: patt.patts.mapIt(computePatt(it, sets)))
      result.first.incl computeFirst(patt.patts[0], sets)
    of pkAlternative:
      result = CompPatt[TKind](
        kind: pkAlternative, patts: patt.patts.mapIt(computePatt(it, sets)))
      for p in patt.patts:
        result.first.incl computeFirst(p, sets)
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      result = CompPatt[TKind](
        kind: kind, opt: @[computePatt(patt.opt, sets)])
      result.first.incl computeFirst(patt.opt, sets)
    of pkNterm:
      # FIRST sets for nonterminals are stored in `sets`
      result = CompPatt[TKind](kind: pkNterm, nterm: patt.nterm)

  result.action = patt.action

func first*[TKind](
  patt: CompPatt[TKind], sets: NTermSets[TKind]): FirstSet[TKind] =
  case patt.kind:
    of pkNTerm: sets.first[patt.nterm]
    else: patt.first


proc necessaryTerms[TKind](rhs: Patt[TKind]): seq[NTermSym] =
  ## Generate list of nonterminals that might appear at rhs of production
  case rhs.kind:
    of pkAlternative:
      return rhs.patts.filterIt(it.kind == pkNTerm).mapIt(it.nterm)
    of pkNTerm:
      return @[rhs.nterm]
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
  # whole grammar based on which terms depend on which. If there is a
  # cycle in grammar exception is thrown - it means grammar is
  # left-recursive. (REVIEW: check if left recursion cannot occur from
  # some other type of grammar)

  let sortedRules = g.rules.topoSort(
    deps = ((r) => (r.patts.necessaryTerms().mapIt(it.hash))),
    idgen = ((r) => hash(r.nterm))
  )

  for rule in sortedRules:
    let compPatt = computePatt(rule.patts, sets)
    sets.first[rule.nterm] = first(compPatt, sets)
    result.rules.add CompRule[TKind](nterm: rule.nterm, patts: compPatt)

  result.sets = sets

proc makeSetLiteral[T](s: set[T]): NimNode =
  ## Create new set literal
  result = nnkCurly.newTree()
  for elem in s:
    result.add ident($elem)

proc makeParseBlock[TKind](
  patt: CompPatt[TKind],
  sets: NTermSets[TKind],
  resName: string = "res"): NimNode

proc makeAltBlock[TKind](alt: CompPatt[TKind], sets: NTermSets[TKind]): NimNode =
  ## Create code block for parsing alternative pattern
  assert alt.kind == pkAlternative
  let toksIdent = ident "toks"
  result = nnkCaseStmt.newTree(quote do: `toksIdent`.peek().kind)

  for idx, patt in alt.patts:
    let resName = &"patt{idx}res"
    let resIdent = ident resName
    let parseBlock = makeParseBlock(patt, sets, resName)
    result.add nnkOfBranch.newTree(
      makeSetLiteral(first(patt, sets)),
      quote do:
        `parseBlock`
        `resIdent`
    )

  let elseBody = quote do:
    raise CodeError(msg: "Unexpected token")

  result.add nnkElse.newTree(elsebody)

proc makeParserName*(nterm: NTermSym): string =
  ## Converter nonterminal name into parsing proc name
  "parse" & nterm.capitalizeAscii()

proc makeTermBlock[TKind](term: CompPatt[TKind]): NimNode =
  assert term.kind == pkTerm
  let tokIdent = ident($term.tok)
  let tokType = ident "Tok"
  let toksIdent = ident "toks"
  return quote do:
    let tok = next(`toksIdent`)
    assert tok.kind == `tokIdent`
    newTree(tok)

proc makeNTermBlock[TKind](nterm: CompPatt[TKind]): NimNode =
  assert nterm.kind == pkNTerm
  let
    ntermIdent = ident(makeParserName(nterm.nterm))
    toksIdent = ident "toks"
  quote do:
    `ntermIdent`(`toksIdent`)

proc makeConcatBlock[TKind](nterm: CompPatt[TKind], sets: NTermSets[TKind]): NimNode =
  assert nterm.kind == pkConcat
  let parseStmts = collect(newSeq):
    for idx, patt in nterm.patts:
      makeParseBlock(patt, sets, &"patt{idx}res")

  let valueVars = nnkBracket.newTree(
    nterm.patts
      .enumerate()
      .mapIt(ident &"patt{it[0]}res")
  )

  let tokIdent = ident "Tok"
  return (parseStmts & @[
    quote do:
      newTree[`tokIdent`](@`valueVars`)
      # ParseTree[`tokIdent`](
      #   kind: pkConcat,
      #   values: @`valueVars`
      # )
  ]).newStmtList()

proc makeNtoMTimesBlock[TKind](
  nterm: CompPatt[TKind], sets: NtermSets[TKind],
  mintimes, maxtimes: int): NimNode =
  assert nterm.kind in {pkZeroOrMore, pkOneOrMore, pkOptional}
  let
    laLiteral = makeSetLiteral(first(nterm, sets))
    bodyParse = makeParseBlock(nterm.opt[0], sets, "itemRes")
    minLit = newLit(mintimes)
    maxLit = newLit(maxtimes)
    cnt = ident("cnt")
    tokType = ident("Tok")
    itemIdent = ident("itemRes")
    kindLiteral = ident($nterm.kind)
    subItems = ident "subItems"
    toksIdent = ident "toks"


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
        newTree[`tokType`](`subItems`)
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
  sets: NTermSets[TKind],
  resName: string = "res"): NimNode =
  ## Generate code block to parse pattern `patt`.
  result = case patt.kind:
    of pkTerm:
      makeTermBlock(patt)
    of pkOptional:
      makeNtoMTimesBlock(patt, sets, 0, 1)
    of pkNterm:
      makeNTermBlock(patt)
    of pkAlternative:
      makeAltBlock(patt, sets)
    of pkConcat:
      makeConcatBlock(patt, sets)
    of pkZeroOrMore:
      makeNtoMTimesBlock(patt, sets, 0, -1)
    of pkOneOrMore:
      makeNtoMTimesBlock(patt, sets, 1, -1)

  let
    resIdent = ident resName
    toksIdent = ident "toks"

  let actAssgn =
    if patt.action != taDefault:
      let actLit = ident($patt.action)
      quote do:
        `resIdent`.action = `actLit`
    else:
      newEmptyNode()

  return newStmtList(
    newCommentStmtNode($patt & " " & $patt.kind),
    quote do:
      var `resIdent` = block:
        `result`

      runTreeActions(`resIdent`)
      `actAssgn`
    )

proc makeRuleParser[TKind](
  rule: CompRule[TKind],
  sets: NTermSets[TKind]): tuple[decl, impl: NimNode] =
  ## Generate implementation for proc to parse rule
  let
    procName = ident(rule.nterm.makeParserName())
    toksIdent = ident "toks"
    resIdent = ident "res"

  let decl = quote do:
    # Declare procedure to parse `rule`. `toks` is instance of token
    # stream used to get lookahead.
    proc `procName`[Tok](`toksIdent`: var TokStream[Tok]): ParseTree[Tok]

  let ntermNterm = newLit(rule.nterm)
  let parseBody = rule.patts.makeParseBlock(sets)
  let impl = quote do:
    proc `procName`[Tok](`toksIdent`: var TokStream[Tok]): ParseTree[Tok] =
      `parseBody`
      case `resIdent`.kind:
        of ptkTerm, ptkNTerm:
          return newTree(name = `ntermNterm`, subnodes = @[`resIdent`])
        of ptkList:
          return newTree(name = `ntermNterm`, subnodes = `resIdent`.getSubnodes())

  return (decl: decl, impl: impl)


proc makeGrammarParser*[TKind](gram: CompGrammar[TKind]): NimNode =
  ## Generate code for parsing grammar `gram`
  var decls: seq[NimNode]
  var impls: seq[NimNode]
  for rule in gram.rules:
    let (decl, impl) = makeRuleParser(rule, gram.sets)
    decls.add decl
    impls.add impl

  result = newStmtList(
    decls.newStmtList(),
    impls.newStmtList()
  )

proc `$`*[TKind](patt: CompPatt[TKind]): string =
  case patt.kind:
    of pkNterm:
       &"<{patt.nterm}>"
    of pkTerm:
       &"'{patt.tok}'"
    of pkAlternative:
      patt.patts.mapIt($it).join(" | ")
    of pkConcat:
      patt.patts.mapIt($it).join(" , ")
    of pkOptional:
      &"({patt.opt})?"
    of pkZeroOrMore:
      &"({patt.opt})*"
    of pkOneOrMore:
      &"({patt.opt})+"


#=======================  Parser type definition  ========================#

type
  LL1RecursiveDescentParser*[Tok] = object
    startCb: proc(toks: var TokStream[Tok]): ParseTree[Tok]

func newLL1RecursiveDescent*[Tok](
  cb: proc(toks: var TokStream[Tok]): ParseTree[Tok]): LL1RecursiveDescentParser[Tok] =
  result.startCb = cb

proc parse*[Tok](
  parser: LL1RecursiveDescentParser[Tok],
  toks: var TokStream[Tok]): ParseTree[Tok] =
  parser.startCb(toks)
