import grammars
import hmisc/algo/hseq_mapping
import sugar, sequtils

type
  FirstTable*[Tk] = Table[BnfNterm, set[Tk]]
  FollowTable*[Tk] = Table[BnfNterm, set[Tk]]
  LL1Table*[Tk] = object
    f1: int


proc necessaryTerms[TKind](patts: seq[BnfPatt[TKind]]): seq[BnfNterm] =
  ## Generate list of nonterminals that might appear at rhs of production
  for patt in patts:
    assert patt.flat
    if patt.elems[0].kind == fbkNterm:
      result.add patt.elems[0].nterm

func getFirst*[Tk](grammar: BnfGrammar[Tk]): FirstTable[Tk] =
  let sortedRules: seq[(BnfNTerm, seq[BnfPatt[Tk]])] = grammar.rules.mapPairs(
    (head: lhs, patts: rhs)).topoSort(
    deps = ((r) => (r.patts.necessaryTerms().mapIt(it.hash))),
    idgen = ((r) => hash(r.head))
  )

  for (rule, patts) in sortedRules:
    if rule notin result:
      result[rule] = {}

    for alt in patts:
      case alt.first.kind:
        of fbkEmpty:
          discard
        of fbkTerm:
          result[rule].incl alt.first.tok
        of fbkNTerm:
          result[rule].incl result[alt.first.nterm]

func getFirst*[Tk](elem: FlatBnf[Tk], table: FirstTable[Tk]): set[Tk] =
  case elem.kind:
    of fbkNTerm:
      table[elem.nterm]
    of fbkTerm:
      {elem.tok}
    of fbkEmpty:
      {}


func getFollow*[Tk](
  grammar: BnfGrammar[Tk], first: FirstTable[Tk]): FollowTable[Tk] =

  for head, patts in grammar.rules:
    for alt in patts:
      let alt: seq[FlatBnf[Tk]] = alt.elems
      for i in 0 ..< alt.len - 1:
        if alt[i].kind == fbkNterm:
          result[alt[i].nterm].incl getFirst(alt[i + 1], first)

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
