import helpers, halgorithm, hmisc_types
import sequtils, colors
import tables, strformat, strutils
import math

type
  Delim* = object
    ## Block delimiters
    content*: string ## String for delimiter
    preferMultiline*: bool ## Don't try to put delimiter on the same
    ## line with content - always prefer new chunks

  DelimPair* = tuple[
    start: Delim,
    final: Delim
  ]

  PPrintConf* = object
    ##[

Pretty print configuration

    ]##

    maxWidth*: int ## Max allowed width
    identStr*: string ## String to use for indentaion
    # wrapLargerThan*: int ##

    kvSeparator*: string ## String to use when separating key-value
    ## pairs.
    tblWrapper*: DelimPair ## Pair of delimiter around table instance

    objWrapper*: DelimPair ## Pair of delimiters around object instance
    fldNameWrapper*: DelimPair ## Pair of delimiter around table key
    fldSeparator*: string ## String to place between key-value pairs in list
    nowrapMultiline*: bool ## Do not wrap mutliline objects in delimiters
    alignFieldsRight*: bool ## Use right align on fields (default - left)

    seqSeparator*: string ## String to place between items in sequence
    seqPrefix*: string ## Prefix to use for multiline sequece
    ## instance. If empty multiline string will be wrapped in regular
    ## delimiters
    seqWrapper*: DelimPair ## Delimiters for sequence instance
    hideEmptyFields*: bool ## Hide empty fields (seq of zero length,
    ## `nil` references etc.).

  ObjKind* = enum
    okConstant ## Literal value
    okSequence ## Sequence of items
    okTable ## List of key-value pairs with single types for keys and
    ## values
    okComposed ## Named list of field-value pairs with possilby
    ## different types for fields (and values). List name is optional
    ## (unnamed object), field name is optional (unnamed fields)

  FieldBranch*[Node] = object
    # IDEA three possible parameters: `NimNode` (for compile-time
    # operations), `PNode` (for analysing code at runtime) and.
    when Node is NimNode:
      ofValue*: Node ## Exact AST used in field branch
    else:
      value*: ObjTree[Node] ## Match value for case branch

    flds*: seq[Field[Node]] ## Fields in the case branch
    isElse*: bool

  Field*[Node] = object
    ## More complex representation of object's field - supports
    ## recursive fields with case objects. IMPLEMENT - not currently
    ## supported.
    name*: string
    fldType*: string ## Type of field value
    value*: ObjTree[Node]
    case isKind*: bool
      of true:
        selected*: int ## Index of selected branch
        branches*: seq[FieldBranch[Node]] ## List of all branches as
                                    ## `value-branch` pairs.
      of false:
        discard


  ObjTree*[Node] = object
    ##[

## Fields

:isPrimitive: Value is primitve or not?

  Primitive value will be added to graphviz node export as part of the
  table (in regular export) as oppposed to non-primitive value (it
  will be rendered as separate node). By default primitive values are
  `int`, `string`, `float` etc. types, tuples/objects that are (1)
  composed only from primitive types (`(int, int)`), (2) have four
  fields or less. Also tables/sequences with four elements or less are
  considered primitive if (1) both keys and values are primitive (2)
  container has four elements or less.

    ]##
    path*: seq[int] ## Path of object in original tree
    objId*: int
    isPrimitive*: bool ## Whether or not value can be considered primitive
    case kind*: ObjKind
      of okConstant:
        constType*: string ## Type of the value
        strlit*: string ## Value representation in string form
      of okSequence:
        itemType*: string ## Type of the sequence item
        valItems*: seq[ObjTree[Node]] ## List of values
      of okTable:
        keyType*: string ## Type of table key
        valType*: string ## TYpe of value key
        valPairs*: seq[tuple[key: string, val: ObjTree[Node]]] ## List of
        ## key-value pairs for table
        # XXXX TODO TEST used `ObjTree` for key too. Non-trivial types
        # can be used. Write unit tests for this functionality.

        # NOTE REFACTOR use `value` for enum field.
      of okComposed:
        namedObject*: bool ## This object's type has a name? (tuples
        ## does not have name for a tyep)
        namedFields*: bool ## Fields have dedicated names? (anonymous
        ## tuple does not have a name for fields)
        name*: string ## Name for an object
        case sectioned*: bool
          of false:
            # Simpler representation for object tree without
            # sectioning on different blocks depending on `kind`
            # fields: everything is put into single key-value
            # sequence.

            # XXX TODO Add field type
            fldPairs*: seq[tuple[name: string, value: ObjTree[Node]]] ## Sequence
            ## of field-value pairs for object representation
          of true:
            # Most of the case objects have one `kind` field named
            # 'kind' but this should account for cases with multiple
            # case fields as well as nested ones
            kindBlocks*: seq[Field[Node]] ## Object field tree. TODO -
            ## currently not implemented

  ValObjTree* = ObjTree[void] ## Object tree used at runtime.
  ValField* = Field[void] ## Field used at runtime
  ValFieldBranch* = FieldBranch[void] ## Field branch used at runtime


func `==`*[Node](lhs, rhs: Field[Node]): bool

func `==`*[Node](lhs, rhs: ObjTree[Node]): bool =
  lhs.kind == rhs.kind and
    (
      case lhs.kind:
        of okConstant:
          lhs.constType == rhs.constType and
          lhs.strLit == rhs.strLit
        of okSequence:
          lhs.itemType == rhs.itemType and
          subnodesEq(lhs, rhs, valItems)
        of okTable:
          lhs.keyType == rhs.keyType and
          lhs.valType == rhs.valType and
          zip(lhs.valPairs, rhs.valPairs).allOfIt(
            (it[0].key == it[1].key) and (it[0].val == it[1].val)
          )
        of okComposed:
          lhs.namedObject == rhs.namedObject and
          lhs.namedFields == rhs.namedFields and
          lhs.name == rhs.name and
          lhs.sectioned == rhs.sectioned and
          (
            case lhs.sectioned:
              of true:
                subnodesEq(lhs, rhs, kindBlocks)
              of false:
                zip(lhs.fldPairs, rhs.fldPairs).mapPairs(
                  (lhs.name == rhs.name) and (lhs.value == rhs.value)
                ).foldl(a and b)
          )

    )

func `==`*[Node](lhs, rhs: Field[Node]): bool =
  lhs.isKind == rhs.isKind and
    (
      case lhs.isKind:
        of true:
          lhs.name == rhs.name and
          lhs.fldType == rhs.fldType and
          lhs.value == rhs.value and
          subnodesEq(lhs, rhs, branches)
        of false:
          true
    )




type
  ObjElem* = object
    text*: string
    color*: Color

func makeObjElem*(text: string): ObjElem =
  ObjElem(text: text)

type
  RectPoint* = enum
    rpoLeftEdge
    rpoRightEdge
    rpoBottomEdge
    rpoTopEdge
    rpoTopLeft
    rpoTopRight
    rpoBottomLeft
    rpoBottomRight

  GridPoint* = enum
    gpoIntersection
    gpoTopLeft
    gpoTopRight
    gpoBottomLeft
    gpoBottomRight
    gpoLeftBorder
    gpoLeftIntersection
    gpoRightBorder
    gpoRightIntersection
    gpoTopBorder
    gpoTopIntersection
    gpoBottomBorder
    gpoBottomIntersection
    gpoHorizontalGap
    gpoVerticalGap


type
  SizePolicy* = enum
    spExpanding
    spFixed

  GridCell*[T] = object
    valid: bool
    rows: int
    cols: int
    vertPolicy: SizePolicy
    horizPolicy: SizePolicy
    borders*: Table[RectPoint, string]

    case isItem*: bool
      of true:
        item*: T
        size: Size ## Item size
      of false:
        grid*: BlockGrid[T]

  BlockGrid*[T] = object
    borders*: Table[GridPoint, string]
    grid*: SparseGrid[GridCell[T]] ## row[col[cell]]
    maxH: Map[int, int] ## Max height in each row
    maxW: Map[int, int] ## Max width in each column

  Range* = object
    a*: int
    b*: int

  Pos* = object
    row*: int
    col*: int

import hashes
func `[]`*[T](
  cell: GridCell[T], pos: RectPoint, default: string = ""): string =
  cell.borders.getOrDefault(pos, default)

func `[]`*[T](
  grid: BlockGrid[T], pos: GridPoint, default: string = ""): string =
  grid.borders.getOrDefault(pos, default)

func makeUnicodeGridBorders*(): Table[GridPoint, string] =
  {
    gpoIntersection : "┼",
    gpoTopLeft : "┌",
    gpoTopRight : "┐",
    gpoBottomLeft : "└",
    gpoBottomRight : "┘",
    gpoLeftBorder : "│",
    gpoLeftIntersection : "├",
    gpoRightBorder : "│",
    gpoRightIntersection : "┤",
    gpoTopBorder : "─",
    gpoTopIntersection : "┬",
    gpoBottomBorder : "─",
    gpoBottomIntersection : "┴",
    gpoHorizontalGap : "─",
    gpoVerticalGap : "│",
  }.toTable()

func hash*(r: Range): Hash = hash(r.a) !& hash(r.b)
func width*[T](cell: GridCell[T]): int = cell.size.width
func height*[T](cell: GridCell[T]): int = cell.size.height
func sumjoin*(a: openarray[int], sep: int): int =
  a.sum() + (a.len > 0).tern((a.len() - 1) * sep, 0)

func totalWidth*[T](grid: BlockGrid[T], colRange: Range): int =
  ## Get total width of columns in `colRange`, including horisontal
  ## grid gap
  toSeq(grid.maxW.valuesBetween(colRange.a, colRange.b)).sumjoin(
    grid[gpoHorizontalGap].len()
  )

func totalHeight*[T](grid: BlockGrid[T], rowRange: Range): int =
  ## Get total height of the rows in `rowRange`, including vertical
  ## grid gap.
  toSeq(grid.maxH.valuesBetween(rowRange.a, rowRange.b)).sumjoin(
    grid[gpoVerticalGap].len()
  )

func columns*[T](grid: BlockGrid[T]): seq[int] =
  grid.maxH.mapPairs(lhs)

func colSizes*[T](grid: BlockGrid[T]): Map[int, int] = grid.maxW
func rowSizes*[T](grid: BlockGrid[T]): Map[int, int] = grid.maxH

func colSizes*[T](grid: BlockGrid[T], a, b: int): seq[int] =
  toSeq(grid.maxW.valuesBetween(a, b))

func colAfter*[T](grid: BlockGrid[T], b: int): seq[int] =
  toSeq(grid.maxW.valuesFrom(b))

func lastCol*[T](grid: BlockGrid[T], row: int): int =
  ## Index of last column for row
  # toSeq(grid.maxH.keys()).max()
  toSeq(grid.grid.columns(row)).mapIt(it.idx).max()

func lastRow*[T](grid: BlockGrid[T]): int =
  ## Index of last row for grid
  toSeq(grid.maxW.keys()).max()
  # toSeq(grid.grid.rows(row)).mapIt(it.idx).max()

func rows*[T](grid: BlockGrid[T]): seq[int] =
  grid.maxW.mapPairs(rhs).sorted()

func width*[T](grid: BlockGrid[T]): int =
  grid.maxW.mapPairs(rhs).sum()

func height*[T](grid: BlockGrid[T]): int =
  grid.maxH.mapPairs(rhs).sum()

func rowHeight*[T](grid: BlockGrid[T], row: int): int = grid.maxH[row]
func occupied*[T](cell: GridCell[T]): Size =
  makeSize(w = cell.cols, h = cell.rows)

func internal*[T](cell: GridCell[T]): Size = cell.size

converter toRange*(elems: (int, int)): Range =
  Range(a: elems[0], b: elems[1])

func toRange*(a, b: int): Range = Range(a: a, b: b)
func toPos*(row, col: int): Pos = Pos(row: row, col: col)

func colRange*[T](grid: BlockGrid[T], pos: Pos | tuple[row, col: int]): Range =
  let start = pos.col
  var finish = pos.col

  return toRange((start, finish))

func setMax[T](a: var T, b: T): void =
  if a < b:
    a = b

func maxOrSet[K, V](tbl: var Map[K, V], key: K, val: V): void =
  if not tbl.hasKey(key):
    tbl[key] = val
  else:
    if tbl[key] < val:
      tbl[key] = val

func `[]=`*[T](
  grid: var BlockGrid[T], row, col: int, cell: GridCell[T]): void =
    grid.grid[(row, col)] = cell
    grid.maxW.maxOrSet(col, cell.width)
    grid.maxH.maxOrSet(row, cell.height)

func rowRange*[T](grid: BlockGrid[T], pos: Pos | tuple[row, col: int]): Range =
  let start = pos.row
  var finish = pos.row


  return toRange((start, finish))

func middles*(r: Range): int =
  ## Number of gaps in between range points
  (r.b - r.a - 1)

func isPoint*(r: Range): bool =
  ## If range starts is equal to end
  (r.a == r.b)
func point*(r: Range): int =
  assert r.isPoint()
  r.a

  # for idx, cell in grid.grid.columns(pos.row):
  #   if idx > pos.col:
  #     if cell.valid:
  #       finish =

func makeCell*[T](
  arg: T, w, h: int,
  sizes: (int, int) = (1, 1),
  policies: (SizePolicy, SizePolicy) = (spExpanding, spExpanding)
                ): GridCell[T] =
  GridCell[T](
    isItem: true,
    item: arg, size: makeSize(w, h),
    rows: sizes[0],
    cols: sizes[1],
    vertPolicy: policies[0],
    horizPolicy: policies[1]
  )

func makeUnicodeCell*[T](
  arg: T, w, h: int,
  sizes: (int, int) = (1, 1)): GridCell[T] =
  let borderTable = {
    rpoLeftEdge : "║",
    rpoRightEdge : "║",
    rpoBottomEdge : "═",
    rpoTopEdge : "═",
    rpoTopLeft : "╔",
    rpoTopRight : "╗",
    rpoBottomLeft : "╚",
    rpoBottomRight : "╝",
  }.toTable()

  result = makeCell(arg, w + 2, h + 2, sizes)
  result.borders = borderTable

func makeCell*(text: string): GridCell[string] =
  makeCell(arg = text, w = text.len, h = 1)

func makeCell*(text: StrSeq): GridCell[StrSeq] =
  makeCell(
    text,
    w = text.mapIt(it.len).max(),
    h = text.len
  )

func makeGrid*[T](arg: SparseGrid[GridCell[T]]): BlockGrid[T] =
  var maxColw: CountTable[int]
  var maxIdx: int = 0
  for (rowIdx, row) in arg.rows:
    for colIdx, cell in row:
      if maxColw[colIdx] < cell.width:
        maxColw[colIdx] = cell.width

      if colIdx > maxIdx:
        maxIdx = colIdx

  result = BlockGrid[T](
    grid: arg,
    maxW: maxColw.mapPairs((lhs, rhs)).toMap(),
    maxH: toMap(arg.mapItRows(
      it.mapPairs(rhs.height).max(0)
    ))
  )

func makeGrid*[T](arg: SparseGrid[tuple[item: T, w, h: int]]): BlockGrid[T] =
  makeGrid(mapIt2d(arg, it.item.makeCell(it.w, it.h)))

func makeGrid*(arg: SparseGrid[string]): BlockGrid[string] =
  makeGrid(arg.mapIt2d(makeCell(it, it.len, 1)))

func makeGrid*(arg: SparseGrid[seq[string]]): BlockGrid[seq[string]] =
  makeGrid(arg.mapIt2d(makeCell(
    it, it.mapIt(it.len).max(0), it.len
  )))

func addHeader*[T](grid: var BlockGrid[T], cell: GridCell[T]): void =
  var cell = cell
  cell.vertPolicy = spExpanding
  cell.horizPolicy = spExpanding

  grid.grid.prepend(@[cell])
