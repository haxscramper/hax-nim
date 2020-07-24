import grammars, lexer
import hmisc/[helpers]
import hmisc/algo/hseq_mapping
import hmisc/types/[seq2d, hdrawing, hterm_buf]
import sugar, sequtils, hashes, tables, strutils, strformat, deques, sets

import bnf_grammars, grammars, parse_helpers, parser_base, parse_tree

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

func isNullable[Tk](
  fbnf: FlatBnf[Tk],
  nulls: Table[BnfNterm, seq[AltId]]): bool =
  case fbnf.kind:
    of fbkEmpty: true
    of fbkTerm: false
    of fbkNterm: fbnf.nterm in nulls

func getSets*[Tk](grammar: BnfGrammar[Tk]): tuple[
  first: FirstTable[Tk],
  follow: FollowTable[Tk],
  nullable: Table[BnfNterm, seq[AltId]]] =
  # echo "werwe"

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
      # showLog fmt "Processing {rule.exprRepr()} {body.exprRepr()}"
      # runIndentedLog:
      block: # `FIRST` set construction
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
                debugecho fmt("[ {elem.nterm.exprRepr()} ]")
                result.first[elem.nterm].mapPairs(rhs).union()
              of fbkTerm:
                # Add token to `FIRST[X]` directly
                # showLog fmt "Found {elem.tok} @ {body.exprRepr()}[{idx}]"
                makeTKindSet(elem.tok)
              of fbkEmpty:
                # QUESTION ERROR?
                makeTKindSet[Tk]()


            # showLog fmt "Adding {first:>30} to FIRST of {rule.head}[{rule.alt}]"
            updated |= result.first[rule.head][rule.alt].containsOrIncl(first)


            if not elem.isNullable(result.nullable):
              # showInfo fmt "Found non-nullable element {elem.exprRepr()}"
              break # Found non-nullable element, finishing FIRST computation
            # else:
              # showInfo fmt "Element {elem.exprRepr()} of kind {elem.kind} is nullable"

          # showInfo fmt "Finished processing {body.exprRepr()}"

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
  mixin items
  let (firstTable, followTable, nullable) = getSets(grammar)
  for ruleId, alt in grammar.iterrules():
    if ruleId.head notin firstTable:
      #[ IMPLEMENT REVIEW what has to be done ]#
      discard
    else:
      for first in items(firstTable[ruleId.head][ruleId.alt]):
        if ruleId.head notin result:
          result[ruleId.head] = toTable({first : ruleId})
        else:
          result[ruleId.head][first] = ruleId

  for nterm, nullAlts in nullable:
    for tok in items(followTable[nterm]):
      for nullAlt in nullAlts: # QUESTION ERROR?
        result[nterm][tok] = ruleId(nterm, nullAlt)



  plog:
    debugecho "\e[35mFIRST\e[39m set"
    for head, alts in firstTable:
      for id, alt in alts:
        debugecho fmt("{head.exprRepr():>20}[{id}] -> {alt}")

    debugecho "\e[35mFOLLOW\e[39m set"
    for head, alts in followTable:
      dechofmt "{head.exprRepr():>20} -> {alts}"

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
    retainGenerated: bool

func getGrammar*[Tk](parser: LL1TableParser[Tk]): BnfGrammar[Tk] =
  parser.grammar

proc newLL1TableParser*[Tk](
  grammar: Grammar[Tk],
  retainGenerated: bool = false): LL1TableParser[Tk] =
  new(result)
  let bnfg = grammar.toBNF()
  plog:
    debugecho "\e[41mInput grammar\e[49m:\n", grammar.exprRepr()
    debugecho "\e[41mBNF grammar\e[49m:\n", bnfg.exprRepr(true, conf = pconf), "\n"
  result.parseTable = makeLL1TableParser(bnfg)
  result.start = bnfg.start
  result.grammar = bnfg
  result.retainGenerated = retainGenerated

type
  TermProgress[Tok] = object
    nterm: BnfNterm
    expected: int
    elems: seq[ParseTree[Tok]]

method parse*[Tok, Tk](
  parser: LL1TableParser[Tk],
  toks: var TokStream[Tok]): ParseTree[Tok] =
  var stack: seq[FlatBnf[Tk]]
  stack.add FlatBnf[Tk](kind: fbkNterm, nterm: parser.start)
  var curr: Tok = toks.next()
  var ntermStack: seq[TermProgress[Tok]] = @[]
  var done = false
  var parseDone: bool = false
  while not done:
    plog:
      var stackshots: seq[TermBuf]
      var msg: string

      block:
        var stackstr: seq[string]
        stackstr.add fmt(" {\"VV\":^20} ")
        for idx, it in stack.reversed():
          stackstr.add fmt("[{idx:2} {it.exprRepr():<17}]")

        stackstr.add ""
        for idx, nterm in ntermStack.reversed():
          stackstr.add fmt(
            "@ {nterm.nterm.exprRepr():14} [{nterm.elems.len}/{nterm.expected}]")

        stackshots.add stackstr.toTermBuf()

    let top: FlatBnf[Tk] = stack.pop()

    plog:
      case top.kind:
        of fbkTerm:
          msg &= fmt "Expecting {top.exprRepr()}\n"

        of fbkNterm:
          msg &= fmt "Started parsing {top.exprRepr()}\n"
        else:
          discard

    case top.kind:
      of fbkTerm:
        if top.tok == curr.kind:
          plog:
            msg &= fmt("Accepted token '{curr}' ({curr.kind})\n")
            ntermStack.last().elems.add newTree(curr)

          if toks.finished():
            done = true
          else:
            curr = toks.next()
            # plog: msg &= fmt "Read token '{curr}' ({curr.kind})"
        else:
          # ERROR IMPLEMENT
          discard
          plog: echo "unexpected token ", curr
      of fbkNterm:
        if parser.parseTable.contains((top.nterm, curr.kind)):
          let rule: RuleId = parser.parseTable[top.nterm, curr.kind]
          let stackadd = parser.grammar.getProductions(rule)

          plog:
            msg &= fmt("Current token is '{curr}' ({curr.kind})\n")
            msg &= fmt("[{top.exprRepr()} + {curr.kind}] => {rule.exprRepr()}\n")
            msg &= fmt "Started parsing <{rule.head}> using alt {rule.alt}\n"

          if stackadd.len == 1 and stackadd[0].kind == fbkEmpty:
            plog: msg &= "Empty production\n"
            ntermStack.add TermProgress[Tok](nterm: rule.head, expected: 0)
          else:
            ntermStack.add TermProgress[Tok](nterm: rule.head, expected: stackadd.len)

            plog:
              msg &= fmt "- [ {top.exprRepr():25} ]\n"
              for elem in stackadd:
                msg &= fmt("+ [ {elem.exprRepr:25} ]\n")

          stack &= stackadd.reversed().filterIt(it.kind != fbkEmpty)
        else:
          raiseAssert msgjoin("Cannot reduce \e[32m", top.exprRepr(),
           "\e[39m, current token: \e[33m'", curr, "'\e[39m ",
           fmt("({curr.kind})")
          )

      of fbkEmpty:
        plog: echo "Empty token"
        discard # ERROR ?

    while (ntermStack.len > 0) and (ntermStack.last().elems.len == ntermStack.last().expected):
      let last = ntermStack.pop()
      plog: msg &= fmt("Finished parsing {last.nterm} with {last.expected} elems\n")
      if ntermStack.len > 0:
        ntermStack.last().elems.add(
          if last.nterm.generated:
            if parser.retainGenerated:
              newTree(last.nterm.exprRepr(), last.elems)
            else:
              newTree(last.elems)
          else:
            newTree(last.nterm.name, last.elems)
        )
      else:
        plog: msg &= "Completely finished input sequence\n"
        result = newTree(last.nterm.name, last.elems)
        parseDone = true

    plog:
      stackshots.add toTermBuf(msg.split("\n").mapIt(fmt "  {it:<47}  "))
      block:
        var stackstr: seq[string]
        stackstr.add fmt(" {\"VV\":^20} ")
        for idx, it in stack.reversed():
          stackstr.add fmt("[{idx:2} {it.exprRepr():<17}]")

        stackshots.add stackstr.toTermBuf()

      echo stackshots.toTermBuf().toString()
      echo "\e[92m", fmt"""{"-":-^95}""", "\e[39m"

  if parseDone:
    return result
