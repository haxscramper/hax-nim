import tables, sugar, sequtils, strformat, options, colors
export tables
import hmisc/helpers
import hmisc/types/graphviz_ast
import hmisc/algo/[halgorithm, htree_mapping]


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

  Rule*[TKind] = object
    nterm*: NTermSym
    patts*: Patt[TKind]

  Grammar*[TKind] = object
    rules*: seq[Rule[TKind]]

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


func toDotGraphPretty*[Tok](tree: ParseTree[Tok], kindPref: string): Graph =
  result.styleNode.shape = nsaRect
  tree.iterateItBFS(it.subnodes, it.kind != pkTerm):
    let itaddr = cast[int](addr it[])
    if it.kind == pkTerm:
      result.addEdge(makeEdge(
        itaddr,
        itaddr + 1))

      result.addNode(makeNode(
        itaddr + 1,
        ($it.tok).quote(),
        nsaCircle,
        color = colLightGrey,
        style = nstFilled
      ))

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
  tree: ParseTree[Tok], kindPref: string = "", preciseRepr: bool = false): Graph =
  if preciseRepr:
    toDotGraphPrecise(tree, kindPref)
  else:
    toDotGraphPretty(tree, kindPref)

proc toPng*[Tok](
  tree: ParseTree[Tok],
  path: string = "/tmp/image.png",
  kindPref: string = "",
  preciseRepr: bool = false): void =
  tree.toDotGraph(kindPref, preciseRepr).topng(path)

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

  var newsubn: seq[ParseTree[Tok]]
  var hadPromotions: bool = false
  for subnode in tree.subnodes:
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
          debugecho "Promoting subnode ", subnode.lispRepr()
          tree = subnode
          newsubn &= subnode.subnodes
        else:
          discard #[ IMPLEMENT repeated promotions ]#
      of taSubrule:
        discard #[ IMPLEMENT ]#

  tree.subnodes = newsubn
