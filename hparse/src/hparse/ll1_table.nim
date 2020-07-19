import grammars
import hmisc/algo/hseq_mapping
import sugar, sequtils

type
  LL1Table*[Tk] = object
    f1: int


proc necessaryTerms[TKind](patts: seq[BnfPatt[TKind]]): seq[BnfNterm] =
  ## Generate list of nonterminals that might appear at rhs of production
  for patt in patts:
    assert patt.flat
    if patt.elems[0].kind == fbkNterm:
      result.add patt.elems[0].nterm

func getFirst*[Tk](grammar: BnfGrammar[Tk]): Table[BnfNterm, set[Tk]] =
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


func makeLL1TableParser*[Tk](grammar: BnfGrammar[Tk]): LL1Table[Tk] =
  let firstSet = getFirst(grammar)
