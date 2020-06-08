import tables

type
  NTermSym* = string
  PattKind* = enum
    pkTerm ## Terminal token
    pkOptional ## Optional (non)terminal
    pkNterm ## Nonterminal symbol

    pkAlternative ## Any of several (non)terminals. `OR` for (non)terminals
    pkConcat ## All (non)terminals in sequence `AND` for (non)terminals
    pkZeroOrMore ## Zero or more occurencies of (non)terminal
    pkOneOrMore ## One or more occurence of (non)terminal


  Patt*[TKind] = ref object
    ## Ebnf grammar pattern. `Tok` is a type for token object.
    head*: NTermSym ## Nonterminal symbol
    case kind*: PattKind
      of pkNterm:
        sym*: NTermSym ## Nonterminal to parse
      of pkTerm:
        tok*: TKind ## Single token to match literally
      of pkAlternative, pkConcat:
        patts*: seq[Patt[TKind]]
      of pkOptional, pkZeroOrMore, pkOneOrMore:
        opt*: Patt ## Single instance that will be repeated [0..1],
        ## [0..n] or [1..n] times respectively

  Rule*[TKind] = object
    nterm: NTermSym
    patts: Patt[TKind]

  Grammar*[TKind] = object
    rules*: seq[Rule[TKind]]

type
  FirstSet*[TKind] = set[TKind]
  NTermSets*[TKind] = object
    first: Table[NTermSym, FirstSet[TKind]]

  CompPatt*[Tok] = object
    patt: Patt
    first: FirstSet


  CompRule*[TKind] = object
    nterm: NTermSym
    patts: CompPatt[TKind]

  CompGrammar*[TKind] = object
    rules*: seq[CompRule[TKind]]
