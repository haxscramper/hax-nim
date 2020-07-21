import grammars, lexer
import hmisc/[helpers, defensive]
import hmisc/algo/hseq_mapping
import hmisc/types/[seq2d, hdrawing, hterm_buf]
import sugar, sequtils, hashes, tables, strutils, strformat, deques, sets

initDefense()

type
  AltId* = int
  FirstTable*[Tk] = Table[BnfNterm, Table[AltId, TkindSet[Tk]]]
  FollowTable*[Tk] = Table[BnfNterm, TKindSet[Tk]]
  LL1Table*[Tk] = Table[BnfNTerm, Table[Tk, RuleId]]
    # [Current term + Current token] -> Rule to use

func `[]`*[A, B, C](
  table: Table[A, Table[B, C]], aKey: A, bKey: B): C =
  table[aKey][bKey]

func `|=`*(a: var bool, b: bool): void = a = a or b

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


# func getFirst*[Tk](grammar: BnfGrammar[Tk]): FirstTable[Tk] =
#   let rules: seq[(RuleId, BnfPatt[Tk])] = collect(newSeq):
#     for id, alt in grammar.iterrules():
#       (id, alt)

#   # debugecho "\e[33munsorted rules\e[39m"
#   # for (id, rule) in rules:
#   #   debugecho id.exprRepr(true), " ", rule.exprRepr()

#   let sortedRules: seq[(RuleId, BnfPatt[Tk])] = rules.topoSort(
#     deps = (
#       proc(r: (RuleId, BnfPatt[Tk])): seq[Hash] =
#         necessaryTerms[Tk](r[0], grammar).mapIt(it.hash)
#     ),
#     idgen = (
#       proc(r: (RuleId, BnfPatt[Tk])): Hash = r[0].head.hash
#     )
#   )

#   # debugecho "\e[33msorted rules\e[39m"
#   # for (id, rule) in sortedRules:
#   #   debugecho id.exprRepr(), " ", rule.exprRepr()



#   for (id, patt) in sortedRules:
#     if id notin result:
#       result[id] = makeTkindSet[Tk]()

#     case patt.first.kind:
#       of fbkEmpty:
#         discard
#       of fbkTerm:
#         result[id].incl patt.first.tok
#       of fbkNTerm:
#         let nterm = patt.first.nterm
#         for altId, alt in grammar.rules[nterm]:
#           result[id].incl result[ruleId(nterm, altId)]

# func getFirst*[Tk](
#   elem: FlatBnf[Tk],
#   table: FirstTable[Tk], grammar: BnfGrammar[Tk]): TKindSet[Tk] =
#   case elem.kind:
#     of fbkNTerm:
#       for altId, alt in grammar.rules[elem.nterm]:
#         case alt.elems[0].kind:
#           of fbkEmpty:
#             discard
#           of fbkTerm:
#             result.incl alt.elems[0].tok
#           of fbkNterm:
#             result.incl table[ruleId(elem.nterm, altId)]
#     of fbkTerm:
#       return {elem.tok}.toTkind()
#     of fbkEmpty:
#       return makeTKindSet[Tk]()

# func getNullable*[Tk](grammar: BnfGrammar[Tk]): seq[RuleId] =
#   ## Get list of all nonterminals which can generate empty string
#   let noTerms: seq[(RuleId, BnfPatt[Tk])] = collect(newSeq):
#     for ruleId, alt in grammar.iterrules():
#       if alt.elems.noneOfIt(it.kind == fbkTerm):
#         (ruleId, alt)

#   let null: HashSet[RuleId] = noTerms.filterIt(
#     it[1].elems.len == 1 and it[1].first.kind == fbkEmpty
#   ).mapIt(it[0]).toHashSet()

#   #[ IMPLEMENT check for all rules which might use null ones ]#

#   for n in null:
#     result.add n

# func getFollow*[Tk](
#   grammar: BnfGrammar[Tk], first: FirstTable[Tk]): FollowTable[Tk] =

#   block:
#     var endNterms = initDeque[RuleId]()
#     endNterms.addLast ruleId(grammar.start, 0)
#     while endNterms.len > 0:
#       let rule = endNterms.popFirst()
#       result[rule] = makeTKindSet[Tk](eofTok)
#       for altId, alt in grammar.rules[rule.head]:
#         if alt.elems.last.kind == fbkNterm:
#           endNterms.addLast ruleId(alt.elems.last.nterm, altId)


#   for head, patts in grammar.rules:
#     for altIdx, alt in patts:
#       let alt: seq[FlatBnf[Tk]] = alt.elems
#       for i in 0 ..< alt.len - 1:
#         if alt[i].kind == fbkNterm:
#           let first = getFirst(alt[i + 1], first, grammar)
#           let id = ruleId(alt[i].nterm, altIdx)
#           if id notin result:
#             result[id] = first
#           else:
#             result[id].incl first

func isNullable[Tk](
  fbnf: FlatBnf[Tk],
  nulls: Table[BnfNterm, seq[AltId]]): bool =
  case fbnf.kind:
    of fbkEmpty: true
    of fbkTerm: false
    of fbkNterm: fbnf.nterm in nulls

proc getSets*[Tk](grammar: BnfGrammar[Tk]): tuple[
  first: FirstTable[Tk],
  follow: FollowTable[Tk],
  nullable: Table[BnfNterm, seq[AltId]]] =
  echo "werwe"

  for head, alts in grammar.rules:
    for altId, altBody in alts:
      # Generate empty `FIRST/FOLLOW` sets - each rule+alternative pair
      # corresponds to empty set.
      if head notin result.first:
        result.first[head] = {altId : makeTKindSet[Tk]()}.toTable()
      else:
        result.first[head][altId] = makeTKindSet[Tk]()
      # result.follow[rule.head] = {AltId(rule.alt) : makeTKindSet[Tk]()}.toTable()
      result.follow[head] = makeTKindSet[Tk]()

  block: # Add end token to `FOLLOW`
    var endNterms = initDeque[BnfNterm]()
    endNterms.addLast grammar.start
    while endNterms.len > 0:
      let nterm = endNterms.popFirst()
      result.follow[nterm] = makeTKindSet[Tk](eofTok)
      for altId, alt in grammar.rules[nterm]:
        if alt.elems.last.kind == fbkNterm:
          endNterms.addLast alt.elems.last.nterm

  while true:
    var updated: bool = false
    for rule, body in grammar.iterrules():
      showLog fmt "Processing {rule.exprRepr()} {body.exprRepr()}"
      runIndentedLog: # `FIRST` set construction
        # Iterate over all rules in grammar
        if body.isEmpty():
          if rule.head notin result.nullable:
            result.nullable[rule.head] = @[ rule.alt ] # Remember index of nullable alternative
            updated |= true
          else:
            updated |= rule.alt notin result.nullable[rule.head]
            result.nullable[rule.head].add rule.alt
        else:
          for idx, elem in body.elems: # Iterate over all elements in `X ->
            # Y1 Y2` Store `FIRST` sets separately for each
            # alternative. Finish execution after first non-nullable
            # element is found.
            let first = case elem.kind:
              of fbkNterm:
                # Add elements from `FIRST[Yi]` to `FIRST[X, <alt>]`.
                # Since `Yi` might have more than one alternative in
                # grammar we have to merge all possible `FIRST` sets.
                result.first[elem.nterm].mapPairs(rhs).union()
              of fbkTerm:
                # Add token to `FIRST[X]` directly
                showLog fmt "Found {elem.tok} @ {body.exprRepr()}[{idx}]"
                makeTKindSet(elem.tok)
              of fbkEmpty:
                # QUESTION ERROR?
                makeTKindSet[Tk]()


            showLog fmt "Adding {first:>30} to FIRST of {rule.head}[{rule.alt}]"
            updated |= result.first[rule.head][rule.alt].containsOrIncl(first)


            if not elem.isNullable(result.nullable):
              showInfo fmt "Found non-nullable element {elem.exprRepr()}"
              break # Found non-nullable element, finishing FIRST computation
            else:
              showInfo fmt "Element {elem.exprRepr()} of kind {elem.kind} is nullable"

          showInfo fmt "Finished processing {body.exprRepr()}"

      block: # `FOLLOW` set construction
        var tailFollow: TKindSet[Tk] = result.follow[rule.head] # `FOLLOW`
        # for the nonterminal we are working with - need to add this
        # as `FOLLOW` for element at the end
        for elem in body.elems.reversed(): # Iterate over all elements
          # in production in reverse order: `Y1 Y2 Y3 <- X`
          case elem.kind:
            of fbkTerm:
              discard

            of fbkNterm:
              updated |= result.follow[elem.nterm].containsOrIncl(tailFollow)

            of fbkEmpty:
              discard
              break # QUESTION

          if elem.isNullable(result.nullable):
            # Continue snowballing `FOLLOW` tail - current element is
            # nullable => whatever we have accumulated in tail can
            # possibly appear in production.
            if elem.kind != fbkEmpty: # QUESTION ERROR?
              tailFollow.incl(result.first[elem.nterm].mapPairs(rhs).union())
          else:
            # Current elemen is not nullable => current tail is no
            # longer needed anc can be replaced with whatever
            # `FIRST[Yi]` contains.
            case elem.kind:
              of fbkNterm:
                tailFollow = result.first[elem.nterm].mapPairs(rhs).union()
              of fbkTerm:
                tailFollow = makeTKindSet(elem.tok)
              else:
                discard # ERROR?


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

proc makeLL1TableParser*[Tk](grammar: BnfGrammar[Tk]): LL1Table[Tk] =
  # let firstTable = getFirst(grammar)
  # let followTable = getFollow(grammar, firstTable)
  let (firstTable, followTable, nullable) = getSets(grammar)

  debugecho "\e[35mFIRST\e[39m set"
  for head, alts in firstTable:
    for id, alt in alts:
      debugecho fmt("{head.exprRepr():>20}[{id}] -> {alt}")

  debugecho "\e[35mFOLLOW\e[39m set"
  for head, alts in followTable:
    dechofmt "{head.exprRepr():>20} -> {alts}"

  # if true: quit 0
  # if true: quit 0
  for ruleId, alt in grammar.iterrules():
    if ruleId.head notin firstTable:
      #[ IMPLEMENT REVIEW what has to be done ]#
      discard
    else:
      for first in firstTable[ruleId.head][ruleId.alt]:
        if ruleId.head notin result:
          result[ruleId.head] = toTable({first : ruleId})
        else:
          result[ruleId.head][first] = ruleId

  for nterm, nullAlts in nullable:
    for tok in followTable[nterm]:
      for nullAlt in nullAlts: # QUESTION ERROR?
        result[nterm][tok] = ruleId(nterm, nullAlt)

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

proc newLL1TableParser*[Tk](grammar: Grammar[Tk]): LL1TableParser[Tk] =
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
  var prevPop: FlatBnf[Tk]
  while true:
    var stackshots: seq[TermBuf]
    var msg: string

    block:
      var stackstr: seq[string]
      stackstr.add fmt(" {\"VV\":^20} ")
      for idx, it in stack.reversed():
        stackstr.add fmt("[{idx:2} {it.exprRepr():<17}]")

      stackshots.add stackstr.toTermBuf()


    let top: FlatBnf[Tk] = stack.pop()
    # if prevPop.kind == fbkTerm and top.kind == fbkNterm:
    #   msg &= fmt "Processed all rules for {top.exprRepr()}\n"

    # prevPop = top

    msg &= fmt "Popped {top.exprRepr()} from top\n"
    case top.kind:
      of fbkTerm:
        if top.tok == curr.kind:
          if toks.finished():
            echo "token stream finished"
            break
          else:
            msg &= fmt("Accepted token '{curr}' ({curr.kind})\n")
            curr = toks.next()
            msg &= fmt "Read token {curr} ({curr.kind})"
        else:
          # ERROR IMPLEMENT
          discard
          echo "unexpected token ", curr
      of fbkNterm:
        if parser.parseTable.contains((top.nterm, curr.kind)):
          let rule: RuleId = parser.parseTable[top.nterm, curr.kind]
          msg &= fmt("[{top.exprRepr()} + {curr.kind}] => {rule.exprRepr()}\n")
          msg &= fmt "Started parsing <{rule.head}> using alt {rule.alt}\n"
          let stackadd = parser.grammar.getProductions(rule).reversed().filterIt(
            it.kind != fbkEmpty)
          msg &= fmt "Added {stackadd.mapIt(it.exprRepr()).joinw()}"
          stack &= stackadd
        else:
          raiseAssert msgjoin("Cannot reduce \e[32m", top.exprRepr(),
           "\e[39m, current token: \e[33m'", curr, "'\e[39m ",
           fmt("({curr.kind})")
          )

      of fbkEmpty:
        echo "Empty token"
        discard # ERROR ?

    stackshots.add toTermBuf(msg.split("\n").mapIt(fmt "  {it:<47}  "))

    block:
      var stackstr: seq[string]
      # stackstr.add fmt(" {\"Stack\":^20} ")
      for idx, it in stack.reversed():
        stackstr.add fmt("[{idx:2} {it.exprRepr():<17}]")

      stackshots.add stackstr.toTermBuf()

    echo stackshots.toTermBuf().toString()
    echo "\e[92m", fmt"""{"-":-^95}""", "\e[39m"
