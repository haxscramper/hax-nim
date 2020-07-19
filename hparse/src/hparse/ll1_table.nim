import grammars

type
  LL1Table*[Tk] = object
    f1: int

func getFirst*[Tk](grammar: BnfGrammar[Tk]): Table[BnfNterm, set[Tk]] =
  for rule, patts in grammar.rules:
    if rule notin result:
      result[rule] = {}

    for alt in patts:
      case alt.first.kind:
        of fbkEmpty:
          discard
        of fbkTerm:
          result[rule].incl alt.first.tok
        of fbkNTerm:
          result[rule].incl result[alt.first.sym]


func makeLL1TableParser*[Tk](grammar: BnfGrammar[Tk]): LL1Table[Tk] =
  discard
