import grammars
import hmisc/algo/hseq_mapping
import hmisc/types/[seq2d, hdrawing, hterm_buf]
import sugar, sequtils, hashes, tables

type
  FirstTable*[Tk] = Table[RuleId, set[Tk]]
  FollowTable*[Tk] = Table[RuleId, set[Tk]]
  LL1Table*[Tk] = Table[BnfNTerm, Table[Tk, RuleId]]
    # [Current term + Current token] -> Rule to use

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


  let sortedRules: seq[(RuleId, BnfPatt[Tk])] = rules.topoSort(
    deps = (
      proc(r: (RuleId, BnfPatt[Tk])): seq[Hash] =
        necessaryTerms[Tk](r[0], grammar).mapIt(it.hash)
    ),
    idgen = (
      proc(r: (RuleId, BnfPatt[Tk])): Hash = r[0].hash
    )
  )


  for (id, patt) in sortedRules:
    if id notin result:
      result[id] = {}

    case patt.first.kind:
      of fbkEmpty:
        discard
      of fbkTerm:
        result[id].incl patt.first.tok
      of fbkNTerm:
        result[id].incl result[id]

func getFirst*[Tk](
  elem: FlatBnf[Tk],
  table: FirstTable[Tk], grammar: BnfGrammar[Tk]): set[Tk] =
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
      return {elem.tok}
    of fbkEmpty:
      return {}


func getFollow*[Tk](
  grammar: BnfGrammar[Tk], first: FirstTable[Tk]): FollowTable[Tk] =

  for head, patts in grammar.rules:
    for altIdx, alt in patts:
      let alt: seq[FlatBnf[Tk]] = alt.elems
      for i in 0 ..< alt.len - 1:
        if alt[i].kind == fbkNterm:
          result[ruleId(alt[i].nterm, altIdx)].incl getFirst(
            alt[i + 1], first, grammar)

      if alt.last.kind == fbkNterm:
        discard
        #[ IMPLEMENT get final token ]#
        # result[alt.last.nterm].incl getFinalTok[Tk]()

func toGrid[A, B, C](table: Table[A, Table[B, C]]): Seq2D[string] =
  let aIdx: Table[string, int] = collect(initTable(2)):
    for rowIdx, key in toSeq(table.keys).mapIt($it).sorted():
      {key : rowIdx + 1}

  var bIdx: Table[string, int] = block:
    let bKeys: seq[string] = collect(newSeq):
      for _, subtable in table:
        for bKey, _ in subtable:
          $bKey

    collect(initTable):
      for colIdx, key in bKeys.deduplicate():
        {key : colIdx + 1}

  result.fillToSize(rows = aIdx.len + 1, cols = bIdx.len + 1, default = "")
  for aKey, subtable in table:
    for bKey, cVal in subtable:
      result[aIdx[$aKey], bIdx[$bKey]] = $cVal

  for key, rowIdx in aIdx:
    result[rowIdx, 0] = $key

  for key, colIdx in bIdx:
    result[0, colIdx] = $key

func makeLL1TableParser*[Tk](grammar: BnfGrammar[Tk]): LL1Table[Tk] =
  let firstTable = getFirst(grammar)
  let followTable = getFollow(grammar, firstTable)

  for head, first in firstTable:
    debugecho head.exprRepr(), ": ", first

  for head, follow in followTable:
    debugecho head.exprRepr(), ": ", follow

  for id, alt in grammar.iterrules():
    for first in firstTable[id]:
      if id.head notin result:
        let newt = toTable({first : id})
        result[id.head] = newt
      else:
        result[id.head][first] = id


  debugecho newTermGrid(
    (0,0),
    toGrid(result).toTermBufGrid(),
    makeThinLineGridBorders()
  ).toTermBuf().toString()
