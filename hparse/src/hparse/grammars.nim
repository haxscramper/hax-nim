import tables, sugar, sequtils, strformat, options, colors
export tables
import hmisc/helpers
import hmisc/types/[graphviz_ast, hvariant]
import hmisc/algo/[halgorithm, htree_mapping]


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

  TreeAct* = enum
    taDefault ## No tree action specified

    taDrop
    taPromote
    taSubrule
    taSpliceDiscard
    taSplicePromote

  Patt*[TKind] = ref object
    ## Ebnf grammar pattern. `Tok` is a type for token object.
    # head*: NTermSym ## Nonterminal symbol
    action*: TreeAct
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

  BnfPattKind* = enum
    bnfEmpty
    bnfTerm
    bnfNTerm
    bnfConcat
    bnfAlternative

  Rule*[TKind] = object
    nterm*: NTermSym
    patts*: Patt[TKind]

  Grammar*[TKind] = object
    rules*: seq[Rule[TKind]]

  BnfNTerm* = object
    case generated*: bool
      of true:
        idx*: seq[int]
        parent*: string
      of false:
        name*: string

  FlatBnfKind = enum
    fbkEmpty
    fbkNterm
    fbkTerm

  FlatBnf*[Tk] = object
    case kind: FlatBnfKind
      of fbkEmpty: nil
      of fbkNterm:
        nterm*: BnfNterm
      of fbkTerm:
        tok*: Tk


  BnfPatt*[TKind] = ref object # REVIEW is it necessary to use `ref`?
    action*: TreeAct
    case flat*: bool
      of false:
        case kind*: BnfPattKind
          of bnfEmpty:
            nil
          of bnfNterm:
            sym*: BnfNTerm ## Nonterminal to parse
          of bnfTerm:
            tok*: TKind ## Single token to match literally
          of bnfAlternative, bnfConcat:
            patts*: seq[BnfPatt[TKind]]
      of true:
        elems*: seq[FlatBnf[Tkind]]

  BnfRule*[TKind] = object
    sym*: BnfNterm
    patt*: BnfPatt[TKind]

func makeBnfNterm(parent: string, idx: seq[int]): BnfNTerm =
  BnfNterm(generated: true, idx: idx, parent: parent)

func makeBnfNterm(name: string): BnfNTerm =
  BnfNterm(generated: false, name: name)

func addAction*[TKind](patt: Patt[TKind], act: TreeAct): Patt[TKind] =
  result = patt
  result.action = act

type
  FirstSet*[TKind] = set[TKind]
  NTermSets*[TKind] = object
    first*: Table[NTermSym, FirstSet[TKind]]

  CompPatt*[TKind] = object
    action*: TreeAct
    first: FirstSet[TKind]
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

func rule*[Tk](sym: BnfNterm, patt: BnfPatt[Tk]): BnfRule[Tk] =
  BnfRule[Tk](sym: sym, patt: patt)

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

func nterm*[TKind](sym: string): Patt[TKind] =
  Patt[TKind](kind: pkNTerm, sym: sym)



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

import strutils

proc `$`*[TKind](patt: CompPatt[TKind]): string =
  case patt.kind:
    of pkNterm:
       &"<{patt.sym}>"
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
  var vertData: Table[Hash, Vertex]
  var inCounts: Table[Hash, int]

  for vert in verts:
    let depsList = deps(vert)
    let vHash = idgen(vert)

    adjList[vHash] = depsList.toHashSet()
    vertData[vHash] = vert


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
      raise LogicError(
        msg: "Cannot perform topological sort on graph with cycles")

  if revese:
    return sortednodes.reversed().mapIt(vertData[it])
  else:
    return sortednodes.mapIt(vertData[it])


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
        sym: makeBnfNterm(patt.sym)
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
      result.toprule = BnfPatt[Tk](flat: false, kind: bnfNterm, sym: newsym)
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
      result.newrules = @[
        BnfRule[Tk](
          sym: newsym,
          patt: BnfPatt[Tk](
            flat: false,
            kind: bnfAlternative,
            patts: @[
              BnfPatt[Tk](flat: false, kind: bnfEmpty),
              BnfPatt[Tk](flat: false, kind: bnfConcat, patts: @[
                BnfPatt[Tk](flat: false, kind: bnfNterm, sym: newsym),
                body])]))
      ] & subnewrules
    of pkOneOrMore:
      # NOTE I'm not 100% sure if this is correct way to convert
      # one-or-more to bnf
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[Tk](flat: false, kind: bnfNterm, sym: newsym)
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
      result.newrules = @[
        BnfRule[Tk](
          sym: newsym,
          patt: BnfPatt[Tk](
            flat: false,
            kind: bnfConcat,
            patts: @[
              body,
              BnfPatt[Tk](flat: false, kind: bnfAlternative, patts: @[
                BnfPatt[Tk](flat: false, kind: bnfEmpty),
                BnfPatt[Tk](flat: false, kind: bnfNterm, sym: newsym)
        ])]))
      ] & subnewrules
    of pkOptional:
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[Tk](flat: false, kind: bnfNterm, sym: newsym)
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
      result.newrules = @[
        BnfRule[Tk](
          sym: newsym,
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
       return @[ @[ FlatBnf[Tk](kind: fbkNterm, nterm: patt.sym) ] ]
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






func toBNF*[Tk](rule: Rule[Tk], noAltFlatten: bool = false): seq[BnfRule[Tk]] =
  let (top, newrules) = rule.patts.toBnf(rule.nterm)
  if noAltFlatten:
    let toprule = rule(makeBnfNterm(rule.nterm, @[]), top)
    for rule in @[ toprule ] & newrules:
      let newpatts = rule.patt.flatten()
      for idx, patt in newpatts:
        let nterm = makeBnfNterm(rule.sym.parent, rule.sym.idx & @[idx])
        result.add BnfRule[Tk](
          sym: nterm,
          patt: BnfPatt[Tk](flat: true, elems: patt)
        )
  else:
    result.add rule(makeBnfNterm(rule.nterm), top)
    result &= newrules

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
      result.incl other.first[patt.sym]

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
      result = CompPatt[TKind](kind: pkNterm, sym: patt.sym)

  result.action = patt.action

func first*[TKind](
  patt: CompPatt[TKind], sets: NTermSets[TKind]): FirstSet[TKind] =
  case patt.kind:
    of pkNTerm: sets.first[patt.sym]
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

func exprRepr*[TKind](patt: Patt[TKind]): string =
  case patt.kind:
    of pkTerm:
      fmt("'{patt.tok}'")
    of pkNTerm:
      fmt("<{patt.sym}>")
    of pkAlternative, pkConcat:
      patt.patts.mapIt(it.exprRepr).join(
        (patt.kind == pkConcat).tern(" & ", " | ")
      ).wrap("{  }")
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      let suff =
        case patt.kind:
          of pkOptional: "?"
          of pkZeroOrMore: "*"
          of pkOneOrMore: "+"
          else:
            ""

      fmt("( {patt.opt.exprRepr()} ){suff}")

func exprRepr*(sym: BnfNTerm): string =
  if sym.generated:
    fmt("{sym.parent}`gen{sym.idx.join(\"_\")}")
  else:
    sym.name

func exprRepr*[Tk](fbnf: FlatBnf[Tk]): string =
  case fbnf.kind:
    of fbkNterm: fmt("<{fbnf.nterm.exprRepr()}>")
    of fbkTerm: fmt("'{fbnf.tok}'")
    of fbkEmpty: "ε"

func exprRepr*[TKind](bnf: BnfPatt[TKind]): string =
  case bnf.flat:
    of true:
      bnf.elems.map(exprRepr).join(" & ")
    of false:
      case bnf.kind:
        of bnfEmpty:
          "ε"
        of bnfTerm:
          fmt("'{bnf.tok}'")
        of bnfNTerm:
          fmt("<{bnf.sym.exprRepr()}>")
        of bnfAlternative, bnfConcat:
          bnf.patts.mapIt(it.exprRepr).join(
            (bnf.kind == bnfConcat).tern(" & ", " | ")
          ).wrap("{  }")


func exprRepr*[TKind](rule: BnfRule[TKind]): string =
  return fmt("{rule.sym.exprRepr()} ::= {rule.patt.exprRepr()}")

func exprRepr*[Tk](rule: Rule[Tk]): string =
  return fmt("{rule.nterm} ::= {rule.patts.exprRepr()}")

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
