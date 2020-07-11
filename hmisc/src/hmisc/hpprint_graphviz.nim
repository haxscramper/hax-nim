import types/[
  hnim_ast,
  graphviz_ast,
  block_grid,
  html_ast,
  sparse_grid,
  hvariant
]

export hvariant

import hcommon_converters
export hcommon_converters

import algo/htree_mapping

import hpprint

import sequtils

type
  DotGenConfig = object
    f1: int

proc toGrid*(obj: ObjTree, topId: NodeId): tuple[
  grid: BlockGrid[ObjElem],
  edges: seq[(NodeId, NodeId)]] =
  # If object is primitive convert it into grid. All non-primitive
  # child objects will be converted into port nodes - no content is
  # present, only port for connecting to external node. For each such
  # node edge is added from current node's output port to other node.

  # assert obj.isPrimitive
  echo &"converting object {obj.objId} to grid"
  # echo obj

  #[ IMPLEMENT ]#
  # case obj.kind:
  #   of okConstant:
  #     if obj.isPrimitive:
  #       # REVIEW handle non-primitive constants
  #       result.grid = makeGrid(
  #         @[@[
  #           (
  #             item: makeObjElem(obj.constType),
  #             w: obj.constType.len,
  #             h: 1
  #           ), # First cell - object type
  #           (
  #             item: makeObjElem(obj.strLit),
  #             w: obj.strLit.len,
  #             h: 1
  #           ) # Secon cell - object value
  #         ]],
  #         makeCell("", 0, 0)
  #       )
  #   else:
  #     discard
  #[ IMPLEMENT ]#

  discard

# fold object into grid, export grid into html table, convert html
# table into graphviz object.


proc toHtml*(grid: Seq2D[GridCell[ObjElem]]): HtmlElem =
  discard


proc toHtml*(grid: SparseGrid[GridCell[ObjElem]]): HtmlElem =
  discard

proc toTable*(grid: BlockGrid[ObjElem]): HtmlElem
proc toHtml*(cell: GridCell[ObjElem]): HtmlElem =
  case cell.isItem:
    of true:
      HtmlElem(
        kind: hekCell,
        cellBgColor: cell.item.color,
        elements: @[
          HtmlElem(
            kind: hekText,
            textStr: cell.item.text)])
    of false:
      cell.grid.toTable()

proc toTable*(grid: BlockGrid[ObjElem]): HtmlElem =
  grid.grid.toHtml()

proc foldObject(obj: ObjTree): tuple[node: Node, edges: seq[Edge]] =
  ##[

Recurisvely convert `ObjTree` into graphviz html-like node.

All primitive subitems are embedded into resulting node. All other
elements as converted into edges.

  ]##
  let (grid, edges) = obj.toGrid(obj.objId)
  result.node = Node(
    shape: nsaPlaintext,
    id: obj.objId,
    htmlLabel: grid.toTable()
  )

  for (src, to) in edges:
    result.edges.add Edge(
      src: src,
      to: @[ to ]
    )

proc toDotGraph*[Obj](obj: Obj, conf: DotGenConfig = DotGenConfig()): Graph =
  var counter =
    iterator(): int {.closure.} =
      var cnt: int = 0
      while true:
        yield cnt
        inc cnt

  let tree = toSimpleTree(obj, counter)
  # TO whoever reading this: I had to use life support system to not
  # die of brain overload. Just saying.
  var folded = tree.mapItDFS(
    outType = seq[Var2[Edge, Node]],
    hasSubnodes = (it.kind != okConstant),
    subnodeCall = it.getSubnodes(),
    op =
      block:
        let (node, edges) = it.foldObject()
        @[ toVar2[Edge, Node](node) ] &
          toVar2[Edge, Node](edges) &
          sequtils.concat(subt)
  )

  result = Graph(
    nodes: folded.filterIt(it.hasType(Node)).mapIt(it.get(Node)),
    edges: folded.filterIt(it.hastype(Edge)).mapIt(it.get(Edge))
  )
