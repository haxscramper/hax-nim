## Types/functions for BNF grammars

import grammars
import parse_primitives
import sets, hashes, sequtils, strformat, strutils
import hmisc/helpers

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
  mixin contains, hash
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
  BnfNterm(generated: false, name: name)

func rule*[Tk](nterm: BnfNterm, patt: BnfPatt[Tk]): BnfRule[Tk] =
  ## Construct new BNF rule using `nterm` as head and `patt` as production
  BnfRule[Tk](nterm: nterm, patt: patt)

func patt*[Tk](elems: seq[FlatBnf[Tk]]): BnfPatt[Tk] =
  BnfPatt[Tk](flat: true, elems: elems)


#===================  Conversion from regular grammar  ===================#


func subrules*[Tk](patt: Patt[Tk]): seq[Patt[Tk]] =
  case patt.kind:
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      @[patt.opt]
    of pkAlternative, pkConcat:
      patt.patts
    else:
      raiseAssert(msgjoin("Invalid patt: {patt.kind} does not have subrules"))

func isNested*[Tk](patt: Patt[Tk]): bool =
  patt.kind in {pkAlternative .. pkOneOrMore}

func toBNF*[Tk](
  patt: Patt[Tk],
  parent: string,
  idx: seq[int] = @[]): tuple[
    toprule: BnfPatt[Tk],
    newrules: seq[BnfRule[Tk]]] =

  case patt.kind:
    of pkTerm:
      result.toprule = BnfPatt[Tk](flat: false, kind: bnfTerm, tok: patt.tok)
    of pkNterm:
      result.toprule = BnfPatt[Tk](
        flat: false,
        kind: bnfNTerm,
        nterm: makeBnfNterm(patt.nterm)
      )
    of pkAlternative, pkConcat:
      var newsubp: seq[BnfPatt[Tk]]

      for pos, subp in patt.patts:
        let (bnfPatt, bnfRules) = subp.toBNF(parent, idx = idx & @[pos])
        newsubp.add bnfPatt
        result.newrules &= bnfRules

      if patt.kind == pkAlternative:
        result.toprule = BnfPatt[Tk](flat: false, kind: bnfAlternative, patts: newsubp)
      else:
        result.toprule = BnfPatt[Tk](flat: false, kind: bnfConcat, patts: newsubp)
    of pkZeroOrMore:
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[Tk](flat: false, kind: bnfNterm, nterm: newsym)
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
      result.newrules = @[
        BnfRule[Tk](
          nterm: newsym,
          patt: BnfPatt[Tk](
            flat: false,
            kind: bnfAlternative,
            patts: @[
              BnfPatt[Tk](flat: false, kind: bnfEmpty),
              BnfPatt[Tk](flat: false, kind: bnfConcat, patts: @[
                body,
                BnfPatt[Tk](flat: false, kind: bnfNterm, nterm: newsym)
        ])]))
      ] & subnewrules
    of pkOneOrMore:
      # IMPLEMENT
      # NOTE I'm not 100% sure if this is correct way to convert
      # one-or-more to bnf
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[Tk](flat: false, kind: bnfNterm, nterm: newsym)
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
      result.newrules = @[
        BnfRule[Tk](
          nterm: newsym,
          patt: BnfPatt[Tk](
            flat: false,
            kind: bnfConcat,
            patts: @[
              body,
              BnfPatt[Tk](flat: false, kind: bnfAlternative, patts: @[
                BnfPatt[Tk](flat: false, kind: bnfEmpty),
                BnfPatt[Tk](flat: false, kind: bnfNterm, nterm: newsym)
        ])]))
      ] & subnewrules
    of pkOptional:
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[Tk](flat: false, kind: bnfNterm, nterm: newsym)
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
      result.newrules = @[
        BnfRule[Tk](
          nterm: newsym,
          patt: BnfPatt[Tk](
            flat: false,
            kind: bnfAlternative,
            patts: @[BnfPatt[Tk](flat: false, kind: bnfEmpty), body]))
      ] & subnewrules


func flatten[Tk](patt: BnfPatt[Tk]): seq[seq[FlatBnf[Tk]]] =
 if patt.flat:
   return @[ patt.elems ]
 else:
   case patt.kind:
     of bnfEmpty:
       return @[ @[ FlatBnf[Tk](kind: fbkEmpty) ] ]
     of bnfTerm:
       return @[ @[ FlatBnf[Tk](kind: fbkTerm, tok: patt.tok) ] ]
     of bnfNterm:
       return @[ @[ FlatBnf[Tk](kind: fbkNterm, nterm: patt.nterm) ] ]
     of bnfConcat:
       for idx, sub in patt.patts:
         var newpatts: seq[seq[FlatBnf[Tk]]]
         for patt in sub.flatten():
           if result.len == 0:
             newpatts.add patt
           else:
             for val in result:
               newpatts.add val & patt

         result = newpatts
     of bnfAlternative:
       for alt in patt.patts:
         result &= alt.flatten()


func toBNF*[Tk](
  rule: Rule[Tk],
  noAltFlatten: bool = false,
  renumerate: bool = true): seq[BnfRule[Tk]] =
  let (top, newrules) = rule.patts.toBnf(rule.nterm)
  if noAltFlatten:
    block:
      let topflat = top.flatten()
      for elems in topflat:
        mixin makeBnfNterm, patt
        #[ IMPLEMENT expression cannot be called error ]#
        # FIXME XXXX
        result.add rule(makeBnfNterm(rule.nterm), patt(elems))

    for rule in newrules:
      let newpatts = rule.patt.flatten()
      for idx, elems in newpatts:
        let nterm = makeBnfNterm(rule.nterm.parent, rule.nterm.idx)
        if elems.allOfIt(it.kind == fbkEmpty):
          discard
          result.add (rule(nterm, patt(elems))) # FIXME XXXX
        else:
          let elems = elems.filterIt(it.kind != fbkEmpty)
          result.add rule(nterm, patt(elems)) # FIXME XXXX
  else:
    result.add rule(makeBnfNterm(rule.nterm), top) # FIXME XXXX
    result &= newrules

  if renumerate:
    #[ IMPLEMENT replace nonterminal names inside rules too ]#
    #[ IMPLEMENT do not change numbering of non-generated terms ]#
    for idx, rule in result:
      if rule.nterm.generated:
        var tmp = rule
        tmp.nterm.idx = @[idx]
        result[idx] = tmp

func toBNF*[Tk](grammar: Grammar[Tk]): BnfGrammar[Tk] =
  mixin toBNF
  result = makeGrammar(
    grammar.rules.mapIt(it.toBNF(noAltFlatten = true, renumerate = false)).concat())

  result.start = BnfNterm(generated: false, name: grammar.start)



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

func first*[Tk](patt: BnfPatt[Tk]): FlatBnf[Tk] =
  assert patt.flat
  return patt.elems[0]


#===========================  Pretty-printing  ===========================#

func exprRepr*(nterm: BnfNTerm, normalize: bool = false): string =
  if nterm.generated:
    if normalize:
      fmt("{nterm.parent.toUpperAscii()}{nterm.idx.join(\"\")}")
    else:
      fmt("{nterm.parent}{nterm.idx.join(\"_\")}")
  else:
    if normalize:
      nterm.name.toUpperAscii()
    else:
      nterm.name

func exprRepr*[Tk](
  fbnf: FlatBnf[Tk],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  case fbnf.kind:
    of fbkNterm:
      (fbnf.nterm.exprRepr(conf.normalizeNterms)).wrap(conf.ntermWrap)
    of fbkTerm:
      ($fbnf.tok).wrap(conf.termWrap)
    of fbkEmpty:
      conf.emptyProd

func exprRepr*[TKind](
  bnf: BnfPatt[TKind],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  case bnf.flat:
    of true:
      bnf.elems.mapIt(exprRepr(it, conf)).join(conf.concatSep)
    of false:
      case bnf.kind:
        of bnfEmpty:
          conf.emptyProd
        of bnfTerm:
          ($bnf.tok).wrap(conf.termWrap)
        of bnfNTerm:
          (bnf.nterm.exprRepr()).wrap(conf.ntermWrap)
        of bnfAlternative, bnfConcat:
          bnf.patts.mapIt(exprRepr(it, conf)).join(
            (bnf.kind == bnfConcat).tern(conf.concatSep, conf.alternSep)
          ).wrap("{  }")


func exprRepr*[TKind](
  rule: BnfRule[TKind],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  let head = rule.nterm.exprRepr(conf.normalizeNterms)
  return fmt("{head:<12} {conf.prodArrow} {rule.patt.exprRepr(conf)}")

func exprRepr*[Tk](
  rule: Rule[Tk],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  return fmt("{rule.nterm:<12} {conf.prodArrow} {rule.patts.exprRepr(conf)}")

func exprRepr*[Tk](
  grammar: BnfGrammar[Tk],
  nojoin: bool = false,
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  var buf: seq[string]
  if nojoin:
    for head, patts in grammar.rules:
      for idx, alt in patts:
        let head =
          if conf.normalizeNterms:
            fmt("{head.exprRepr(true)}")
          else:
            fmt("{head.exprRepr()}.{idx}")

        buf.add fmt("{head:<12} {conf.prodArrow} {alt.exprRepr(conf)}")

  else:
    for head, patts in grammar.rules:
      let head = head.exprRepr(conf.normalizeNterms)
      buf.add fmt("{head}  {conf.prodArrow} ")
      for idx, alt in patts:
        buf.add fmt(".{idx}{conf.alternSep}{alt.exprRepr(conf)}")
      buf.add ""

  return buf.join("\n")

func exprRepr*(id: RuleId, normalize: bool = false): string =
  if normalize:
    fmt("{id.head.exprRepr(true)}{id.alt}")
  else:
    fmt("{id.head.exprRepr(false)}.{id.alt}")

func `$`*(id: RuleId): string = id.exprRepr()
func `$`*(nterm: BnfNterm): string = nterm.exprRepr()
