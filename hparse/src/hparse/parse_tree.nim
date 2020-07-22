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
  ParseTree*[Tok] = ref object
    ##[

Parse tree object

## Notes

- `subnodes` field is on top level but is not used for `pkTerm` - value is
  instead stored in `tok` field

    ]##
    subnodes*: seq[ParseTree[Tok]] ## Sequence of parsed subnodes
    action*: TreeAct ## Tree action to execute after construction

    case kind*: PattKind
      of pkTerm:
        tok*: Tok ## Value of parsed token
      of pkNTerm:
        name*: NTermSym ## Name of parsed nonterminal
      else:
        nil




#============================  Constructors  =============================#

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

#========================  Accessors/predicates  =========================#

#===========================  Pretty-printing  ===========================#

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


#=====================  Tree actions implementation  =====================#

func runTreeActions*[Tok](tree: var ParseTree[Tok]): void =
  case tree.action:
    of taDrop: # This tree should be dropped by it's parent
      return
    else:
      discard

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
