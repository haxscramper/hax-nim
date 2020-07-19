import grammars
import hmisc/algo/hseq_mapping
import sugar, sequtils, hashes

type
  FirstTable*[Tk] = Table[RuleId, set[Tk]]
  FollowTable*[Tk] = Table[RuleId, set[Tk]]
  LL1Table*[Tk] = object
    f1: int

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


func makeLL1TableParser*[Tk](grammar: BnfGrammar[Tk]): LL1Table[Tk] =
  let firstTable = getFirst(grammar)
  let followTable = getFollow(grammar, firstTable)

  for head, first in firstTable:
    debugecho head.exprRepr(), ": ", first

  for head, follow in followTable:
    debugecho head.exprRepr(), ": ", follow
