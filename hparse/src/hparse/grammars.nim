import tables, hashes, sugar, sequtils, strformat, options, colors
import strutils
export tables
import hmisc/helpers
import hmisc/types/[graphviz_ast, hvariant]
import hmisc/algo/[halgorithm, htree_mapping, hseq_mapping]
import lexer


type
  NTermSym* = string
  PattKind* = enum
    pkTerm ## Terminal token
    pkNterm ## Nonterminal symbol

    # 'nested' patterns
    pkAlternative ## Any of several (non)terminals. `OR` for (non)terminals
    pkConcat ## All (non)terminals in sequence `AND` for (non)terminals

    pkOptional ## Optional (non)terminal
    pkZeroOrMore ## Zero or more occurencies of (non)terminal
    pkOneOrMore ## One or more occurence of (non)terminal

  Patt*[TKind] = ref object
    ## Ebnf grammar pattern. `Tok` is a type for token object.
    # head*: NTermSym ## Nonterminal symbol
    action*: TreeAct
    case kind*: PattKind
      of pkNterm:
        nterm*: NTermSym ## Nonterminal to parse
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
    start*: NtermSym
    rules*: seq[Rule[TKind]]



func addAction*[TKind](patt: Patt[TKind], act: TreeAct): Patt[TKind] =
  result = patt
  result.action = act


#=================================  ===  =================================#

type
  FirstSet*[TKind] = set[TKind]
  NTermSets*[TKind] = object
    first*: Table[NTermSym, FirstSet[TKind]]

  CompPatt*[TKind] = object
    action*: TreeAct
    first: FirstSet[TKind]
    case kind*: PattKind
      of pkNterm:
        nterm*: NTermSym ## Nonterminal to parse
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

type
  ParseTree*[Tok] = ref object
    ## Parse tree object
    subnodes*: seq[ParseTree[Tok]]
    action*: TreeAct

    case kind*: PattKind
      of pkTerm:
        tok: Tok
      of pkNTerm:
        name: NTermSym
      else:
        nil

#====================  generic pattern construction  =====================#

func rule*[Tk](name: string, patt: Patt[Tk]): Rule[Tk] =
  Rule[Tk](nterm: name, patts: patt)

func zeroP*[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkZeroOrMore, opt: patt)

func oneP*[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkOneOrMore, opt: patt)

func optP*[TKind](patt: Patt[TKind]): Patt[TKind] =
  Patt[TKind](kind: pkOptional, opt: patt)

func andP*[TKind](patts: varargs[Patt[TKind]]): Patt[TKind] =
  Patt[TKind](kind: pkConcat, patts: toSeq(patts))

func orP*[TKind](patts: varargs[Patt[TKind]]): Patt[TKind] =
  Patt[TKind](kind: pkAlternative, patts: toSeq(patts))

func tok*[TKind](tok: TKind): Patt[TKind] =
  Patt[TKind](kind: pkTerm, tok: tok)

func nterm*[TKind](nterm: string): Patt[TKind] =
  Patt[TKind](kind: pkNTerm, nterm: nterm)



proc newTree*[Tok](kind: PattKind, subtree: varargs[ParseTree[Tok]]): ParseTree[Tok] =
  ## Create new parse tree object
  case kind:
    of pkTerm:
      raiseAssert("Cannot create new tree of kind `pkTerm`" &
        "- use newTree overload")
    of pkNTerm:
      raiseAssert("Cannot create new tree of kind `pkNterm` without" &
        "name - use newTree overload")
    else:
      case kind:
        of pkOptional:
          assert subtree.len < 2,
           "Optional tree cannot have more than one subnode"
        of pkOneOrMore:
          assert subtree.len > 0,
           "One or more tree cannot have zero elements"
        else:
          discard

      ParseTree[Tok](kind: kind, subnodes: toSeq(subtree))

proc newTree*[Tok](tok: Tok): ParseTree[Tok] =
  ParseTree[Tok](kind: pkTerm, tok: tok)

proc newTree*[Tok](name: NTermSym, subnodes: varargs[ParseTree[Tok]]): ParseTree[Tok] =
  ParseTree[Tok](kind: pkNTerm, name: name, subnodes: toSeq(subnodes))

func tok*[Tok](tree: ParseTree[Tok]): Tok =
  assert tree.kind == pkTerm
  return tree.tok

# iterator subnodes*[Tok](tree: ParseTree[Tok]): ParseTree[Tok] =
#   assert tree.kind != pkTerm, "Cannot iterate over subnodes of terminal"
#   for item in tree.subnodes:
#     yield item


func getSubnodes*[Tok](tree: ParseTree[Tok]): seq[ParseTree[Tok]] =
  assert tree.kind != pkTerm, "Cannot iterate over subnodes of terminal"
  tree.subnodes

proc toGrammar*[TKind](
  table: openarray[(string, Patt[TKind])]): Grammar[TKind] =
  result.rules = table.mapPairs(Rule[TKind](nterm: lhs, patts: rhs))
  result.start = result.rules[0].nterm

import strutils

proc `$`*[TKind](patt: CompPatt[TKind]): string =
  case patt.kind:
    of pkNterm:
       &"<{patt.nterm}>"
    of pkTerm:
       &"'{patt.tok}'"
    of pkAlternative:
      patt.patts.mapIt($it).join(" | ")
    of pkConcat:
      patt.patts.mapIt($it).join(" , ")
    of pkOptional:
      &"({patt.opt})?"
    of pkZeroOrMore:
      &"({patt.opt})*"
    of pkOneOrMore:
      &"({patt.opt})+"


import hashes, sequtils, algorithm, sets

type
  LogicError = ref object of CatchableError



func topoSort*[Vertex](
  verts: openarray[Vertex],
  deps: proc(vert: Vertex): seq[Hash],
  idgen: proc(vert: Vertex): Hash,
  revese: bool = true): seq[Vertex] =

  runnableExamples:
    assert @[3, 2, 1] == topoSort(
      verts = @[1, 2, 3],
      deps = proc(v: int): seq[Hash] =
                 case v:
                   of 1: @[Hash(2), Hash(3)]
                   of 2: @[Hash(3)]
                   else: @[]
    )

  var adjList: Table[Hash, HashSet[Hash]]
  var vertData: Table[Hash, seq[Vertex]]
  var inCounts: Table[Hash, int]

  for vert in verts:
    let depsList = deps(vert)
    let vHash = idgen(vert)
    # debugecho "Deps for vert ", $vert, ": ", depsList
    # debugecho "Id: ", vHash, "\n-----"

    adjList[vHash] = depsList.toHashSet()
    vertData.mgetOrPut(vHash, @[]).add vert


    for dep in depsList:
      # For each dependency - increase number of items that depend on this one
      inc inCounts.mgetOrPut(dep, 0)

  let counts: seq[(Hash, int)] = adjList.mapPairs(
    (lhs in inCounts).tern((lhs, inCounts[lhs]), (lhs, 0))
  ).filterIt(it[1] == 0)

  assert counts.len > 0,
      "Graph contains no vertices that have zero in-edges"

  var noincoming: seq[Hash] = counts.mapIt(it[0])
  var sortednodes: seq[Hash]

  while noincoming.len > 0:
    let node = noincoming.pop()
    sortednodes.add node
    # For all adjacent
    for adj in toSeq(adjList[node]):
      # Remove edge `(adj, node)`
      adjList[node].excl adj
      # Decrease number of incoming edges for `adj`
      dec inCounts[adj]


      # If has no incoming
      if inCounts[adj] == 0:
        noincoming.add adj

  for vert, adj in adjList:
    if adj.len > 0:
      raiseAssert(msgjoin(
        "Cannot perform topological sort on graph with cycles",
        adj
      ))

  if revese:
    return sortednodes.reversed().mapIt(vertData[it]).concat()
  else:
    return sortednodes.mapIt(vertData[it]).concat()


func topoSort*[Vertex](
  verts: openarray[Vertex],
  deps: proc(vert: Vertex): seq[Hash],
  revese: bool = true): seq[Vertex] =
  topoSort(verts, deps, reverse, (r) => hash(r))

#=============================  EBNF -> BNF  =============================#

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
    # else:
    #   new(result.toprule)

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
        result.add rule(makeBnfNterm(rule.nterm), patt(elems))

    for rule in newrules:
      let newpatts = rule.patt.flatten()
      for idx, elems in newpatts:
        let nterm = makeBnfNterm(rule.nterm.parent, rule.nterm.idx)
        if elems.allOfIt(it.kind == fbkEmpty):
          result.add rule(nterm, patt(elems))
        else:
          let elems = elems.filterIt(it.kind != fbkEmpty)
          result.add rule(nterm, patt(elems))
  else:
    result.add rule(makeBnfNterm(rule.nterm), top)
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
  result = makeGrammar(
    grammar.rules.mapIt(it.toBNF(noAltFlatten = true, renumerate = false)).concat()
  )

  result.start = BnfNterm(generated: false, name: grammar.start)


#=========================  FIRST set computat  ==========================#

proc computeFirst*[TKind](
  patt: Patt[TKind], other: NTermSets[TKind]): FirstSet[TKind] =
  ## Generate FIRST set for `patt`
  case patt.kind:
    of pkTerm:
      result.incl patt.tok
    of pkConcat:
      result.incl computeFirst(patt.patts[0], other)
    of pkAlternative:
      for p in patt.patts:
        result.incl computeFirst(p, other)
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      result.incl computeFirst(patt.opt, other)
    of pkNterm:
      result.incl other.first[patt.nterm]

proc computePatt*[TKind](
  patt: Patt[TKind], sets: NTermSets[TKind]): CompPatt[TKind] =
  ## Generate FIRST set for pattern `patt`
  let kind = patt.kind
  case kind:
    of pkTerm:
      result = CompPatt[TKind](kind: pkTerm, tok: patt.tok)
      result.first.incl patt.tok
    of pkConcat:
      result = CompPatt[TKind](
        kind: pkConcat, patts: patt.patts.mapIt(computePatt(it, sets)))
      result.first.incl computeFirst(patt.patts[0], sets)
    of pkAlternative:
      result = CompPatt[TKind](
        kind: pkAlternative, patts: patt.patts.mapIt(computePatt(it, sets)))
      for p in patt.patts:
        result.first.incl computeFirst(p, sets)
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      result = CompPatt[TKind](
        kind: kind, opt: @[computePatt(patt.opt, sets)])
      result.first.incl computeFirst(patt.opt, sets)
    of pkNterm:
      # FIRST sets for nonterminals are stored in `sets`
      result = CompPatt[TKind](kind: pkNterm, nterm: patt.nterm)

  result.action = patt.action

func first*[TKind](
  patt: CompPatt[TKind], sets: NTermSets[TKind]): FirstSet[TKind] =
  case patt.kind:
    of pkNTerm: sets.first[patt.nterm]
    else: patt.first

#*************************************************************************#
#***************************  pretty-printing  ***************************#
#*************************************************************************#

func nodeKindStr*[Tok](node: ParseTree[Tok]): string =
  case node.kind:
    of pkOptional: "?"
    of pkAlternative: "or"
    of pkOneOrMore: "1+"
    of pkZeroOrMore: "0+"
    of pkConcat: "and"
    of pkNTerm: node.name
    else:
      ""

func tokKindStr*[TKind](tkind: TKind, prefStr: string): string =
  result = $tkind
  if result.startsWith(prefStr):
    result = result[prefStr.len .. ^1]

#=======================  grammar representation  ========================#

type
  GrammarPrintConf* = object
    emptyProd*: string
    prodArrow*: string
    concatSep*: string
    alternSep*: string
    ntermWrap*: (string, string)
    termWrap*: (string, string)
    normalizeNterms*: bool

const defaultGrammarPrintConf*: GrammarPrintConf = GrammarPrintConf(
  emptyProd: "ε",
  prodArrow: "::=",
  concatSep: " & ",
  alternSep: " | ",
  ntermWrap: ("<", ">"),
  termWrap: ("'", "'")
)

func exprRepr*[TKind](
  patt: Patt[TKind],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  case patt.kind:
    of pkTerm:
      ($patt.tok).wrap(conf.termWrap)
    of pkNTerm:
      ($patt.nterm).wrap(conf.ntermWrap)
    of pkAlternative, pkConcat:
      patt.patts.mapIt(exprRepr(it, conf)).join(
        (patt.kind == pkConcat).tern(conf.concatSep, conf.alternSep)
      ).wrap("{  }")
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      let suff =
        case patt.kind:
          of pkOptional: "?"
          of pkZeroOrMore: "*"
          of pkOneOrMore: "+"
          else:
            ""

      fmt("( {patt.opt.exprRepr(conf)} ){suff}")

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


func exprRepr*[Tk](
  grammar: Grammar[Tk],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  grammar.rules.mapIt(exprRepr(it, conf)).joinl()

func exprRepr*(id: RuleId, normalize: bool = false): string =
  if normalize:
    fmt("{id.head.exprRepr(true)}{id.alt}")
  else:
    fmt("{id.head.exprRepr(false)}.{id.alt}")

func `$`*(id: RuleId): string = id.exprRepr()
func `$`*(nterm: BnfNterm): string = nterm.exprRepr()

#==============================  graphviz  ===============================#

func toDotGraphPretty*[Tok](
  tree: ParseTree[Tok],
  kindPref: string,
  bottomTokens: bool): Graph =
  result.styleNode.shape = nsaRect
  var tokNodes: seq[Node]

  tree.iterateItBFS(it.subnodes, it.kind != pkTerm):
    let itaddr = cast[int](addr it[])
    if it.kind == pkTerm:
      result.addEdge(makeEdge(
        itaddr,
        itaddr + 1))

      let tokNode = makeNode(
        itaddr + 1,
        ($it.tok).quote(),
        nsaCircle,
        color = colLightGrey,
        style = nstFilled
      )

      if bottomTokens:
        tokNodes.add tokNode
      else:
        result.addNode tokNode

    result.addNode(makeNode(
      itaddr,
      label = case it.kind:
        of pkNTerm: it.name
        of pkTerm: fmt("{it.tok.kind.tokKindStr(kindPref)}")
        else: it.nodeKindStr()
      ,
      shape = case it.kind:
        of pkNTerm: nsaDefault
        of pkTerm: nsaUnderline
        else: nsaEllipse
      ,
      color = case it.kind:
        of pkNTerm: colLightBlue
        else: colNoColor
      ,
      style = case it.kind:
        of pkNTerm: nstFilled
        else: nstDefault
    ))
    for tr in subt:
      result.addEdge(makeEdge(
        itaddr,
        toNodeId(addr tr[])
      ))

  if bottomTokens:
    result.addSubgraph(Graph(
      nodes: tokNodes,
      isWrapper: true,
      noderank: gnrSame
    ))

func toDotGraphPrecise*[Tok](tree: ParseTree[Tok], kindPref: string): Graph =
  result.styleNode.shape = nsaRect
  tree.iterateItBFS(it.subnodes, it.kind != pkTerm):
    let itaddr = cast[int](addr it[])
    let label = case it.kind:
      of pkNTerm: it.name
      of pkTerm: fmt("{it.tok.kind.tokKindStr(kindPref)}\n'{it.tok}'")
      else: it.nodeKindStr()

    result.addNode(makeNode(
      itaddr,
      label = label & (
        block:
          if tree.action != taDefault:
            fmt("\n{tree.action}")
          else:
            ""
      )
    ))

    for tr in subt:
      result.addEdge(makeEdge(
        itaddr,
        toNodeId(addr tr[])
      ))

func toDotGraph*[Tok](
  tree: ParseTree[Tok],
  kindPref: string = "",
  preciseRepr: bool = false,
  bottomTokens: bool = false): Graph =
  ##[

## Parameters

:bottomTokens: Put all token nodes at bottom. Works only with pretty graph

  ]##
  if preciseRepr:
    toDotGraphPrecise(tree, kindPref)
  else:
    toDotGraphPretty(tree, kindPref, bottomTokens)

proc toPng*[Tok](
  tree: ParseTree[Tok],
  path: string = "/tmp/image.png",
  kindPref: string = "",
  preciseRepr: bool = false,
  bottomTokens: bool = false): void =
  tree.toDotGraph(kindPref, preciseRepr, bottomTokens).topng(path)

#=========================  tree representation  =========================#

func treeReprImpl*[Tok](
  node: ParseTree[Tok],
  pref: seq[bool],
  parentMaxIdx, currIdx: int,
  kindPref: string): seq[string] =
  let prefStr = pref.mapIt(
    if it: "|   " else: "    "
  ).join("") & "+-> " & (node.action != taDefault).tern(
    fmt("< {node.action} > "),
    ""
  )

  result = case node.kind:
    of pkTerm:
      @[ fmt("{prefStr}{node.tok.kind.tokKindStr(kindPref)} = '{node.tok}'") ]
    of pkNTerm:
      @[ fmt("{prefStr}{node.name}") ]
    else:
      @[ fmt("{prefStr}[ {node.nodeKindStr()} ]") ]

  for idx, subn in node.subnodes:
    result &= subn.treeReprImpl(pref & @[
      currIdx != parentMaxIdx
    ],
    node.subnodes.len - 1,
    idx,
    kindPref)

func treeRepr*[Tok](node: ParseTree[Tok], kindPref: string = ""): string =
  treeReprImpl(node, @[], 0, 0, kindPref).join("\n")

func lispReprImpl*[Tok](
  node: ParseTree[Tok],
  kindPref: string, discardEmpty: bool): seq[string] =
  case node.kind:
    of pkTerm:
      var kindStr = $node.tok.kind
      if kindStr.startsWith(kindPref):
        kindStr = kindStr[kindPref.len .. ^1]

      @[ fmt("({kindStr} '{node.tok}')") ]
    else:
      if discardEmpty and (node.getSubnodes().len == 0):
        @[]
      else:
        @[ "(" & node.nodeKindStr() & " " &
          node.subnodes.mapIt(
            it.lispReprImpl(kindPref, discardEmpty)
          ).concat().join(" ") &
          ")" ]


func lispRepr*[Tok](
  node: ParseTree[Tok],
  kindPref: string = "",
  discardEmpty: bool = true): string =
  lispReprImpl(node, kindPref, discardEmpty).join(" ")

#*************************************************************************#
#*********************  tree action implementation  **********************#
#*************************************************************************#

func runTreeActions*[Tok](tree: var ParseTree[Tok]): void =
  case tree.action:
    of taDrop: # This tree should be dropped by it's parent
      return
    else:
      discard

  # debugechoi 1, "Tree actions on"
  # debugechoi 1, tree.treeRepr()

  var newsubn: seq[ParseTree[Tok]]
  var hadPromotions: bool = false
  let subnodes = tree.subnodes
  for idx in 0 ..< subnodes.len:
    let subnode = subnodes[idx]

    case subnode.kind:
      of pkTerm:
        case subnode.action:
          of taPromote:
            if subnodes.len > 1:
              raiseAssert(msgjoin(
                "Cannot promote terminal node in tree with",
                subnodes.len, "elements",
                ($subnode.tok.kind), " in tree ", tree.lispRepr()))
          of taSpliceDiscard, taSplicePromote:
            raiseAssert(msgjoin(
              "Cannot splice terminal node (it cannot have child",
              "elements): attempted splice",
              subnode.action, "of", ($subnode.tok.kind),
              " in tree ", tree.lispRepr()))
          else:
            discard
      else:
        discard

    case subnode.action:
      of taDefault:
        newsubn.add subnode
      of taDrop:
        discard
      of taSpliceDiscard:
        newsubn &= subnode.subnodes
      of taSplicePromote:
        tree = subnode
        newsubn &= subnode.subnodes
      of taPromote:
        if not hadPromotions:
          tree = subnode
          newsubn &= subnode.subnodes
        else:
          discard #[ IMPLEMENT repeated promotions ]#
      of taSubrule:
        discard #[ IMPLEMENT ]#

  tree.subnodes = newsubn
  tree.action = taDefault

  # debugechoi 1, "Result"
  # debugechoi 1, tree.treeRepr()

#===============================  parser  ================================#

type
  Parser* = ref object of RootObj

method parse*[Tok](parser: Parser, toks: var TokStream[Tok]): ParseTree[Tok] =
  raiseAssert("No implementation for base parser class")
