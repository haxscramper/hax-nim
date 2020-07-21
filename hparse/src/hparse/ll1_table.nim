import grammars, lexer
import hmisc/helpers
import hmisc/algo/hseq_mapping
import hmisc/types/[seq2d, hdrawing, hterm_buf]
import sugar, sequtils, hashes, tables, strutils, strformat, deques, sets

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

  # debugecho "\e[33munsorted rules\e[39m"
  # for (id, rule) in rules:
  #   debugecho id.exprRepr(true), " ", rule.exprRepr()

  let sortedRules: seq[(RuleId, BnfPatt[Tk])] = rules.topoSort(
    deps = (
      proc(r: (RuleId, BnfPatt[Tk])): seq[Hash] =
        necessaryTerms[Tk](r[0], grammar).mapIt(it.hash)
    ),
    idgen = (
      proc(r: (RuleId, BnfPatt[Tk])): Hash = r[0].head.hash
    )
  )

  # debugecho "\e[33msorted rules\e[39m"
  # for (id, rule) in sortedRules:
  #   debugecho id.exprRepr(), " ", rule.exprRepr()



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

func getNullable*[Tk](grammar: BnfGrammar[Tk]): seq[RuleId] =
  ## Get list of all nonterminals which can generate empty string
  let noTerms: seq[(RuleId, BnfPatt[Tk])] = collect(newSeq):
    for ruleId, alt in grammar.iterrules():
      if alt.elems.noneOfIt(it.kind == fbkTerm):
        (ruleId, alt)

  let null: HashSet[RuleId] = noTerms.filterIt(
    it[1].elems.len == 1 and it[1].first.kind == fbkEmpty
  ).mapIt(it[0]).toHashSet()

  #[ IMPLEMENT check for all rules which might use null ones ]#

  for n in null:
    result.add n

func getFollow*[Tk](
  grammar: BnfGrammar[Tk], first: FirstTable[Tk]): FollowTable[Tk] =

  block:
    var endNterms = initDeque[RuleId]()
    endNterms.addLast ruleId(grammar.start, 0)
    while endNterms.len > 0:
      let rule = endNterms.popFirst()
      result[rule] = makeTKindSet[Tk](eofTok)
      for altId, alt in grammar.rules[rule.head]:
        if alt.elems.last.kind == fbkNterm:
          endNterms.addLast ruleId(alt.elems.last.nterm, altId)


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

func getSets*[Tk](grammar: BnfGrammar[Tk]): tuple[
  first: FirstTable[Tk],
  follow: FollowTable[Tk],
  nullable: HashSet[BnfNterm]] =

  for ruleId, altBody in grammar.iterrules():
    result.first[ruleId] = makeTKindSet[Tk]()
    result.follow[ruleId] = makeTKindSet[Tk]()

  while true:
    var updated: bool = false
    for ruleId, body in grammar.iterrules():
      if body.isEmpty():
        updated = result.nullable.containsOrIncl(ruleId.head)
      else:
        for elem in body.elems:
          discard
          # case elem.kind:
          #   of fbkNterm:
          #     updated = result.first[ruleId].containsOrIncl(
          #       result.first[elem.nterm]
          #     )

    if not updated:
      break



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

  block:
    let (first, follow, nullable) = getSets(grammar)

  debugecho "\e[35mFIRST\e[39m set"
  for rule, first in firstTable:
    debugecho fmt("{rule.exprRepr():>20}: {first:<20} -> {grammar[rule].exprRepr(pconf)}")

  debugecho "\e[35mFOLLOW\e[39m set"
  for rule, follow in followTable:
    {.noSideEffect.}:
      stdout.write fmt("{rule.exprRepr():>20}: {follow:<20} -> ")
      debugecho fmt("{grammar[rule].exprRepr(pconf)}")

  # if true: quit 0
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

  for rule in grammar.getNullable():
    if rule in followTable:
      for tok in followTable[rule]:
        result[rule.head][tok] = rule
    else:
      debugecho fmt("Rule \e[32m{rule.exprRepr()}\e[39m does not have FOLLOW")

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
  quit 0

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
    var stackshots: seq[TermBuf]
    var msg: string

    block:
      var stackstr: seq[string]
      stackstr.add fmt(" {\"Stack\":^20} ")
      for it in stack.reversed():
        stackstr.add fmt("[{it.exprRepr():^20}]")

      stackshots.add stackstr.toTermBuf()


    let top: FlatBnf[Tk] = stack.pop()
    case top.kind:
      of fbkTerm:
        if top.tok == curr.kind:
          msg = fmt("Accepted token {curr}")
          curr = toks.next()
        else:
          # ERROR IMPLEMENT
          discard
          echo "unexpected token ", curr
      of fbkNterm:
        if parser.parseTable.contains((top.nterm, curr.kind)):
          let rule: RuleId = parser.parseTable[top.nterm, curr.kind]
          msg = fmt("{top.exprRepr()}, {curr.kind} => {rule.exprRepr()}")
          stack &= parser.grammar.getProductions(rule).reversed()
        else:
          raiseAssert msgjoin("Cannot reduce \e[32m", top.exprRepr(),
           "\e[39m, current token: \e[33m'", curr, "'\e[39m ")

      of fbkEmpty:
        echo "Empty token"
        discard # ERROR ?

    stackshots.add toTermBufFast(fmt "  {msg:^40}  ")

    block:
      var stackstr: seq[string]
      stackstr.add fmt(" {\"Stack\":^20} ")
      for it in stack.reversed():
        stackstr.add fmt("[{it.exprRepr():^20}]")

      stackshots.add stackstr.toTermBuf()

    echo stackshots.toTermBuf().toString()
    echo "----"

    if toks.finished():
      echo "token stream finished"
      break
