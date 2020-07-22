## Types/functions for BNF grammars

import grammars

#*************************************************************************#
#**************************  Type declarations  **************************#
#*************************************************************************#

#=============================  Primitives  ==============================#

type
  BnfPattKind* = enum
    ## Types of patters in BNF grammar
    bnfEmpty
    bnfTerm
    bnfNTerm
    bnfConcat
    bnfAlternative

  BnfNTerm* = object
    case generated*: bool
      of true:
        idx*: seq[int]
        parent*: NtermSym
      of false:
        name*: NtermSym

  FlatBnfKind* = enum
    ## Flat BNF description item
    fbkEmpty ## Empty production
    fbkNterm ## Nonterminal element
    fbkTerm ## Terminal element - token

  FlatBnf*[Tk] = object
    case kind*: FlatBnfKind
      of fbkEmpty:
        nil
      of fbkNterm:
        nterm*: BnfNterm
      of fbkTerm:
        tok*: Tk ## Token kind

#=====================  Grammar & grammar elements  ======================#

type
  BnfPatt*[TKind] = ref object # REVIEW is it necessary to use `ref`?
    ## Recursive of flat bnf pattern
    action*: TreeAct
    case flat*: bool
      of false:
        case kind*: BnfPattKind
          of bnfEmpty:
            nil
          of bnfNterm:
            nterm*: BnfNTerm ## Nonterminal to parse
          of bnfTerm:
            tok*: TKind ## Single token to match literally
          of bnfAlternative, bnfConcat:
            patts*: seq[BnfPatt[TKind]] ## Concatenation/list of alternatives
      of true:
        elems*: seq[FlatBnf[Tkind]] ## Flat bnf - concatenation of (non)terminals

  BnfRule*[TKind] = object
    ## Single rule with one production
    nterm*: BnfNterm ## Nonterminal name
    patt*: BnfPatt[TKind] ## Elements

  RuleId* = object
    ## Nonterminal head and alternative index
    head*: BnfNterm
    alt*: int ## Index of alternative in `rules` field in grammar

  BnfGrammar*[Tk] = object
    start*: BnfNterm ## Start element in grammar
    rules*: Table[BnfNterm, seq[BnfPatt[Tk]]] ## Bnf rule and sequence
    ## of alternatives. Each item in sequence is expected to be
    ## `BnfPatt.flat == true`.

#============================  Aux functions  ============================#

func hash*(nterm: BnfNTerm): Hash =
  var h: Hash = 0
  h = h !& hash(nterm.generated)
  case nterm.generated:
    of true:
      h = h !& hash(nterm.parent)
      h = h !& hash(nterm.idx)
    of false:
      h = h !& hash(nterm.name)

  result = !$h

func hash*(id: RuleId): Hash =
  var h: Hash = 0
  h = h !& hash(id.head) !& hash(id.alt)
  result = !$h

func `==`*(lhs, rhs: BnfNterm): bool =
  lhs.generated == rhs.generated and (
    case lhs.generated:
      of true:
        (lhs.parent == rhs.parent) and
        (lhs.idx == rhs.idx)
      of false:
        (lhs.name == rhs.name)
  )

#============================  Constructors  =============================#

func makeGrammar*[Tk](rules: seq[BnfRule[Tk]]): BnfGrammar[Tk] =
  ## Construction grammar from sequence of rules
  for rule in rules:
    if rule.nterm notin result.rules:
      result.rules[rule.nterm] = @[rule.patt]
    else:
      result.rules[rule.nterm].add rule.patt


# TODO rename into `makeRuleId`
func ruleId*(nterm: BnfNterm, alt: int): RuleId =
  ## Make new rule id
  RuleId(head: nterm, alt: alt)

func makeBnfNterm(parent: string, idx: seq[int]): BnfNTerm =
  BnfNterm(generated: true, idx: idx, parent: parent)

func makeBnfNterm(name: string): BnfNTerm =

func rule*[Tk](nterm: BnfNterm, patt: BnfPatt[Tk]): BnfRule[Tk] =
  ## Construct new BNF rule using `nterm` as head and `patt` as production
  BnfRule[Tk](nterm: nterm, patt: patt)




#========================  Predicates/accessors  =========================#

func isEmpty*[Tk](patt: BnfPatt[Tk]): bool =
  ## Check if pattern describes empty production
  (patt.elems.len == 1) and (patt.elems[0].kind == fbkEmpty)

func `[]`*[Tk](grammar: BnfGrammar[Tk], rule: RuleId): BnfPatt[Tk] =
  ## Get BNF pattern for rule
  grammar.rules[rule.head][rule.alt]

func getProductions*[Tk](
  grammar: BnfGrammar[Tk], id: RuleId): seq[FlatBnf[Tk]] =
  ## Get list of productions from flat bnf pattern at `id`
  grammar.rules[id.head][id.alt].elems
 BnfNterm(generated: false, name: name)

func patt*[Tk](elems: seq[FlatBnf[Tk]]): BnfPatt[Tk] =
  BnfPatt[Tk](flat: true, elems: elems)

func first*[Tk](patt: BnfPatt[Tk]): FlatBnf[Tk] =
  assert patt.flat
  return patt.elems[0]
