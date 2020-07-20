import grammars, lexer
import hmisc/helpers
import hmisc/algo/hseq_mapping
import hmisc/types/[seq2d, hdrawing, hterm_buf]
import sugar, sequtils, hashes, tables, strutils, strformat, deques

type
  FirstTable*[Tk] = OrderedTable[RuleId, TkindSet[Tk]]
  FollowTable*[Tk] = OrderedTable[RuleId, TKindSet[Tk]]
  LL1Table*[Tk] = Table[BnfNTerm, Table[Tk, RuleId]]
    # [Current term + Current token] -> Rule to use

func `[]`*[A, B, C](
  table: Table[A, Table[B, C]], aKey: A, bKey: B): C =
  table[aKey][bKey]

func contains*[A, B, C](table: Table[A, Table[B, C]], pair: (A, B)): bool =
  (pair[0] in table) and (pair[1] in table[pair[0]])

iterator iterrules*[Tk](grammar: BnfGrammar[Tk]): tuple[
  id: RuleId, alt: BnfPatt[Tk]] =
  for head, patts in grammar.rules:
    for altId, alt in patts:
      yield (id: ruleId(head, altId), alt: alt)

proc necessaryTerms*[Tk](
  id: RuleId, grammar: BnfGrammar[Tk]): seq[BnfNTerm] =
  ## Generate list of nonterminals that might appear at rhs of production
  let patt: BnfPatt[Tk] = grammar.rules[id.head][id.alt]
  if patt.elems[0].kind == fbkNterm:
    result.add patt.elems[0].nterm


func getFirst*[Tk](grammar: BnfGrammar[Tk]): FirstTable[Tk] =
  let rules: seq[(RuleId, BnfPatt[Tk])] = collect(newSeq):
    for id, alt in grammar.iterrules():
      (id, alt)

  debugecho "\e[33munsorted rules\e[39m"
  for (id, rule) in rules:
    debugecho id.exprRepr(true), " ", rule.exprRepr()

  let sortedRules: seq[(RuleId, BnfPatt[Tk])] = rules.topoSort(
    deps = (
      proc(r: (RuleId, BnfPatt[Tk])): seq[Hash] =
        necessaryTerms[Tk](r[0], grammar).mapIt(it.hash)
    ),
    idgen = (
      proc(r: (RuleId, BnfPatt[Tk])): Hash = r[0].head.hash
    )
  )

  debugecho "\e[33msorted rules\e[39m"
  for (id, rule) in sortedRules:
    debugecho id.exprRepr(), " ", rule.exprRepr()



  for (id, patt) in sortedRules:
    if id notin result:
      result[id] = makeTkindSet[Tk]()

    case patt.first.kind:
      of fbkEmpty:
        discard
      of fbkTerm:
        result[id].incl patt.first.tok
      of fbkNTerm:
        let nterm = patt.first.nterm
        for altId, alt in grammar.rules[nterm]:
          result[id].incl result[ruleId(nterm, altId)]

func getFirst*[Tk](
  elem: FlatBnf[Tk],
  table: FirstTable[Tk], grammar: BnfGrammar[Tk]): TKindSet[Tk] =
  case elem.kind:
    of fbkNTerm:
      for altId, alt in grammar.rules[elem.nterm]:
        case alt.elems[0].kind:
          of fbkEmpty:
            discard
          of fbkTerm:
            result.incl alt.elems[0].tok
          of fbkNterm:
            result.incl table[ruleId(elem.nterm, altId)]
    of fbkTerm:
      return {elem.tok}.toTkind()
    of fbkEmpty:
      return makeTKindSet[Tk]()


func getFollow*[Tk](
  grammar: BnfGrammar[Tk], first: FirstTable[Tk]): FollowTable[Tk] =

  for head, patts in grammar.rules:
    for altIdx, alt in patts:
      let alt: seq[FlatBnf[Tk]] = alt.elems
      for i in 0 ..< alt.len - 1:
        if alt[i].kind == fbkNterm:
          let first = getFirst(alt[i + 1], first, grammar)
          let id = ruleId(alt[i].nterm, altIdx)
          if id notin result:
            result[id] = first
          else:
            result[id].incl first

      if alt.last.kind == fbkNterm:
        let id = ruleId(alt.last.nterm, altIdx)
        if id notin result:
          result[id] = makeTKindSet[Tk](eofTok)
        else:
          result[id].incl eofTok
        discard
        #[ IMPLEMENT get final token ]#
        # result[alt.last.nterm].incl getFinalTok[Tk]()

func toGrid[A, B, C](
  table: Table[A, Table[B, C]],
  aConvCb: proc(a: A): string {.noSideEffect.} = nil,
  bConvCb: proc(b: B): string {.noSideEffect.} = nil,
  cConvCb: proc(a: C): string {.noSideEffect.} = nil): Seq2D[string] =
  let aConvCb = (aConvCb != nil).tern(aConvCb, proc(a: A): string = $a)
  let bConvCb = (bConvCb != nil).tern(bConvCb, proc(b: B): string = $b)
  let cConvCb = (cConvCb != nil).tern(cConvCb, proc(c: C): string = $c)

  let aIdx: Table[string, int] = collect(initTable(2)):
    for rowIdx, key in toSeq(table.keys).mapIt(aConvCb(it)).sorted():
      {key : rowIdx + 1}

  var bIdx: Table[string, int] = block:
    let bKeys: seq[string] = collect(newSeq):
      for _, subtable in table:
        for bKey, _ in subtable:
          bConvCb(bKey)

    collect(initTable):
      for colIdx, key in bKeys.deduplicate():
        {key : colIdx + 1}

  result.fillToSize(rows = aIdx.len + 1, cols = bIdx.len + 1, default = "")
  for aKey, subtable in table:
    for bKey, cVal in subtable:
      result[aIdx[aConvCb(aKey)], bIdx[bConvCb(bKey)]] = cConvCb(cVal)

  for key, rowIdx in aIdx:
    result[rowIdx, 0] = $key

  for key, colIdx in bIdx:
    result[0, colIdx] = $key


const pconf* = GrammarPrintConf(
  prodArrow: "->",
  emptyProd: "''",
  ntermWrap: ("", ""),
  concatSep: " ",
  normalizeNterms: true
)

func makeLL1TableParser*[Tk](grammar: BnfGrammar[Tk]): LL1Table[Tk] =
  let firstTable = getFirst(grammar)
  let followTable = getFollow(grammar, firstTable)

  debugecho "\e[35mFIRST\e[39m set"
  for rule, first in firstTable:
    debugecho fmt("{rule.exprRepr():>20}: {first:<20} -> {grammar[rule].exprRepr(pconf)}")

  debugecho "\e[35mFOLLOW\e[39m set"
  for rule, follow in followTable:
    debugecho fmt("{rule.exprRepr():>20}: {follow:<20}")

  for id, alt in grammar.iterrules():
    if id notin firstTable:
      #[ IMPLEMENT REVIEW what has to be done ]#
      discard
    else:
      for first in firstTable[id]:
        if id.head notin result:
          let newt = toTable({first : id})
          result[id.head] = newt
        else:
          result[id.head][first] = id

  debugecho "Parse table:\n", newTermGrid(
    (0,0),
    toGrid(
      result,
      # aConvCb = matchCurry2(BnfNterm, true, exprRepr),
      # # bConvCb = matchCurry2(Tk, pconf, exprRepr),
      # cConvCb = matchCurry2(RuleId, true, exprRepr)
    ).toTermBufGrid(),
    makeThinLineGridBorders()
  ).toTermBuf().toString()

#============================  Parser object  ============================#

type
  LL1TableParser*[Tk] = ref object of Parser
    start: BnfNterm
    grammar: BnfGrammar[Tk]
    parseTable: LL1Table[Tk]

func newLL1TableParser*[Tk](grammar: Grammar[Tk]): LL1TableParser[Tk] =
  new(result)
  let bnfg = grammar.toBNF()
  debugecho "\e[41mInput grammar\e[49m:\n", grammar.exprRepr()
  debugecho "\e[41mBNF grammar\e[49m:\n", bnfg.exprRepr(true, conf = pconf), "\n"
  result.parseTable = makeLL1TableParser(bnfg)
  result.start = bnfg.start
  result.grammar = bnfg

method parse*[Tok, Tk](parser: LL1TableParser[Tk], toks: var TokStream[Tok]): ParseTree[Tok] =
  var stack: seq[FlatBnf[Tk]]
  stack.add FlatBnf[Tk](kind: fbkNterm, nterm: parser.start)
  var curr: Tok = toks.next()
  while true:
    let top: FlatBnf[Tk] = stack.pop()
    case top.kind:
      of fbkTerm:
        if top.tok == curr.kind:
          echo "Accepted token ", curr
          curr = toks.next()
        else:
          # ERROR IMPLEMENT
          discard
          echo "unexpected token ", curr
      of fbkNterm:
        if parser.parseTable.contains((top.nterm, curr.kind)):
          let rule: RuleId = parser.parseTable[top.nterm, curr.kind]
          echo "Used rule ", rule.exprRepr()
          stack &= parser.grammar.getProductions(rule).reversed()
        else:
          raiseAssert msgjoin("Cannot reduce \e[32m", top.exprRepr(),
           "\e[39m, current token: \e[33m'", curr, "'\e[39m ")

      of fbkEmpty:
        echo "Empty token"
        discard # ERROR ?

    echo fmt " \e[94m{\"Stack\":^20}\e[39m "
    for it in stack.reversed():
      echo fmt("[{it.exprRepr():^20}]")

    if toks.finished():
      echo "token stream finished"
      break
