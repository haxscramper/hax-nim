import tables, sugar, sequtils
export tables


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
    # head*: NTermSym ## Nonterminal symbol
    case kind*: PattKind
      of pkNterm:
        sym*: NTermSym ## Nonterminal to parse
      of pkTerm:
        tok*: TKind ## Single token to match literally
      of pkAlternative, pkConcat:
        patts*: seq[Patt[TKind]]
      of pkOptional, pkZeroOrMore, pkOneOrMore:
        opt*: Patt[TKind] ## Single instance that will be repeated
        ## [0..1], [0..n] or [1..n] times respectively

  Rule*[TKind] = object
    nterm*: NTermSym
    patts*: Patt[TKind]

  Grammar*[TKind] = object
    rules*: seq[Rule[TKind]]

type
  FirstSet*[TKind] = set[TKind]
  NTermSets*[TKind] = object
    first*: Table[NTermSym, FirstSet[TKind]]

  CompPatt*[TKind] = object
    first*: FirstSet[TKind]
    case kind*: PattKind
      of pkNterm:
        sym*: NTermSym ## Nonterminal to parse
      of pkTerm:
        tok*: TKind ## Single token to match literally
      of pkAlternative, pkConcat:
        patts*: seq[CompPatt[TKind]]
      of pkOptional, pkZeroOrMore, pkOneOrMore:
        opt*: seq[CompPatt[TKind]] ## Single instance that will be repeated
        # I could've used `Option[]` but decided to go with `seq`
        # since I should not have a situation where `opt` field is
        # `none` - it is just a workaround to allow recursive field


  CompRule*[TKind] = object
    nterm*: NTermSym
    patts*: CompPatt[TKind]

  CompGrammar*[TKind] = object
    sets*: NTermSets[TKind]
    rules*: seq[CompRule[TKind]]

proc nthType1*[T1, T2](a: (T1, T2)): T1 = discard
proc nthType2*[T1, T2](a: (T1, T2)): T2 = discard

# iterator pairs*[T1, T2](s: openarray[(T1, T2)]): (T1, T2) =
#   for item in s:
#     yield item

template mapPairs*(s: untyped, op: untyped): untyped =
  const openarrPairs = (s is array) or (s is seq) or (s is openarray)
  when openarrPairs:
    type TLhs = type((s[0][0]))
    type TRhs = type((s[0][1]))
  else:
    type TLhs = type((pairs(s).nthType1))
    type TRhs = type((pairs(s).nthType2))

  type TRes = type((
    block:
      var lhs {.inject.}: TLhs
      var rhs {.inject.}: TRhs
      op))

  var res: seq[TRes]

  when openarrPairs:
    for (lhsTmp, rhsTmp) in s:
      let lhs {.inject.} = lhsTmp
      let rhs {.inject.} = rhsTmp
      res.add op
  else:
    for lhsTmp, rhsTmp in s:
      let lhs {.inject.} = lhsTmp
      let rhs {.inject.} = rhsTmp
      res.add op

  res

proc toGrammar*[TKind](
  table: openarray[(string, Patt[TKind])]): Grammar[TKind] =
  result.rules = table.mapPairs(Rule[TKind](nterm: lhs, patts: rhs))
