import parse_primitives
import sequtils, strformat, strutils, colors

import hmisc/types/graphviz_ast
import hmisc/algo/htree_mapping
import hmisc/helpers

#*************************************************************************#
#**************************  Type definitions  ***************************#
#*************************************************************************#

#=============================  Primitives  ==============================#


type
  ParseTreeKind* = enum
    ptkTerm
    ptkNterm
    ptkList

  ParseTree*[Tok] = ref object
    ##[

Parse tree object

## Notes

- `subnodes` field is on top level but is not used for `pkTerm` - value is
  instead stored in `tok` field

    ]##
    action*: TreeAct ## Tree action to execute after construction

    case kind*: ParseTreeKind
      of ptkTerm:
        tok*: Tok ## Value of parsed token
      of ptkNTerm:
        nterm*: NTermSym ## Name of parsed nonterminal
        subnodes*: seq[ParseTree[Tok]] ## Sequence of parsed subnodes
      of ptkList:
        elements*: seq[ParseTree[Tok]]




#============================  Constructors  =============================#

proc newTree*[Tok](subtree: seq[ParseTree[Tok]]): ParseTree[Tok] =
  ## Create new parse tree object
  ParseTree[Tok](kind: ptkList, elements: subtree)

# proc newTree*[Tok](subtree: seq[ParseTree[Tok]]): ParseTree[Tok] =
#   ## Create new parse tree object
#   ParseTree[Tok](kind: ptkList, elements: toSeq(subtree))

proc newTree*[Tok](tok: Tok): ParseTree[Tok] =
  ParseTree[Tok](kind: ptkTerm, tok: tok)

proc newTree*[Tok](
  name: NTermSym, subnodes: seq[ParseTree[Tok]]): ParseTree[Tok] =
  ParseTree[Tok](kind: ptkNTerm, nterm: name, subnodes: subnodes)

func tok*[Tok](tree: ParseTree[Tok]): Tok =
  assert tree.kind == pkTerm
  return tree.tok

func getSubnodes*[Tok](tree: ParseTree[Tok]): seq[ParseTree[Tok]] =
  case tree.kind:
    of ptkNterm: tree.subnodes
    of ptkList: tree.elements
    of ptkTerm: @[]

func len*[Tok](tree: ParseTree[Tok]): int =
  case tree.kind:
    of ptkTerm: 0
    of ptkNterm: tree.subnodes.len
    of ptkList: tree.elements.len

#========================  Accessors/predicates  =========================#

#===========================  Pretty-printing  ===========================#

#==============================  graphviz  ===============================#

func toDotGraphPretty*[Tok](
  tree: ParseTree[Tok],
  kindPref: string,
  bottomTokens: bool): Graph =
  result.styleNode = Node(shape: nsaRect)
  var tokNodes: seq[Node]

  tree.iterateItBFS(it.getSubnodes(), it.kind != ptkTerm):
    let itaddr = toNodeId(cast[int](addr it[]))
    let nextaddr = toNodeId(cast[int](addr it[]) + 1)
    if it.kind == ptkTerm:
      result.addEdge(makeEdge(itaddr, nextaddr))

      let tokNode = makeNode(
        nextaddr,
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
        of ptkNTerm: it.nterm
        of ptkTerm: fmt("{it.tok.kind.tokKindStr(kindPref)}")
        else: it.nodeKindStr()
      ,
      shape = case it.kind:
        of ptkNTerm: nsaDefault
        of ptkTerm: nsaUnderline
        else: nsaEllipse
      ,
      color = case it.kind:
        of ptkNTerm: colLightBlue
        else: colNoColor
      ,
      style = case it.kind:
        of ptkNTerm: nstFilled
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
  result.styleNode = Node(shape: nsaRect)
  tree.iterateItBFS(it.subnodes, it.kind != ptkTerm):
    let itaddr: int = cast[int](addr it[])
    let label = case it.kind:
      of ptkNTerm: it.nterm
      of ptkTerm: fmt("{it.tok.kind.tokKindStr(kindPref)}\n'{it.tok}'")
      of ptkList: it.nodeKindStr()

    result.addNode(makeNode(
      itaddr.toNodeId(),
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
        itaddr.toNodeId(),
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
func nodeKindStr*[Tok](node: ParseTree[Tok]): string =
  case node.kind:
    of ptkList: "[ ... ]"
    of ptkNTerm: node.nterm
    else:
      ""

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
    of ptkTerm:
      @[ fmt("{prefStr}{node.tok.kind.tokKindStr(kindPref)} = '{node.tok}'") ]
    of ptkNTerm:
      @[ fmt("{prefStr}{node.nterm}") ]
    of ptkList:
      @[ fmt("{prefStr}[ {node.nodeKindStr()} ]") ]

  for idx, subn in node.getSubnodes():
    result &= subn.treeReprImpl(pref & @[
      currIdx != parentMaxIdx
    ],
    node.len - 1, idx, kindPref)

func treeRepr*[Tok](node: ParseTree[Tok], kindPref: string = ""): string =
  treeReprImpl(node, @[], 0, 0, kindPref).join("\n")

func lispReprImpl*[Tok](
  node: ParseTree[Tok],
  kindPref: string, discardEmpty: bool): seq[string] =
  case node.kind:
    of ptkTerm:
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


#=====================  Tree actions implementation  =====================#

func runTreeActions*[Tok](tree: var ParseTree[Tok]): void =
  case tree.action:
    of taDrop: # This tree should be dropped by it's parent
      return
    else:
      discard

  var newsubn: seq[ParseTree[Tok]]
  var hadPromotions: bool = false
  let subnodes =
    case tree.kind:
      of ptkNterm: tree.subnodes
      of ptkList: tree.elements
      else: @[]

  for idx in 0 ..< subnodes.len:
    let subnode = subnodes[idx]

    case subnode.kind:
      of ptkTerm:
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
        newsubn &= subnode.getSubnodes()
      of taSplicePromote:
        tree = subnode
        newsubn &= subnode.getSubnodes()
      of taPromote:
        if not hadPromotions:
          tree = subnode
          newsubn &= subnode.getSubnodes()
        else:
          discard #[ IMPLEMENT repeated promotions ]#
      of taSubrule:
        discard #[ IMPLEMENT ]#


  case tree.kind:
    of ptkNterm:
      tree.subnodes = newsubn
    of ptkList:
      tree.elements = newsubn
    else:
      discard

  tree.action = taDefault
