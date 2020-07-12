import tables, sequtils, sugar, options

import hgeometry_primitives, hprimitives, hdrawing
import seq2d
import ../hdebug_misc
import ../algo/[hseq_mapping, halgorithm]

## Nested table with multicol support

#===========================  type definition  ===========================#

type
  SizePolicy* = enum
    spExpanding
    spFixed

  GridCell*[T] = object
    size: Size
    vertPolicy: SizePolicy
    horizPolicy: SizePolicy
    borders*: Table[RectPoint, string]

    case isItem*: bool
      of true:
        item*: T
      of false:
        grid*: BlockGrid[T]

  BlockGrid*[T] = object
    borders*: TermGridConf
    grid*: MulticellGrid[GridCell[T]]


#=========================  accessor functions  ==========================#

func width*[T](cell: GridCell[T]): int = cell.size.width
func height*[T](cell: GridCell[T]): int = cell.size.height

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

func colSizes*[T](grid: BlockGrid[T]): seq[int] = grid.maxW
func rowSizes*[T](grid: BlockGrid[T]): seq[int] = grid.maxH

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
  let (_, hSpacing, left, right, _, _) = spacingDimensions(grid.borders)
  grid.maxW.mapPairs(rhs).sumjoin(hSpacing) + left + right

func height*[T](grid: BlockGrid[T]): int =
  let (vSpacing, _, _, _, top, bottom) = spacingDimensions(grid.borders)
  grid.maxH.mapPairs(rhs).sumjoin(vSpacing) + top + bottom

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

func rowRange*[T](grid: BlockGrid[T], pos: Pos | tuple[row, col: int]): Range =
  let start = pos.row
  var finish = pos.row


  return toRange((start, finish))

func `[]=`*[T](grid: var BlockGrid[T], row, col: int, cell: GridCell[T]): void =
  grid.grid[makePos(row, col)] = (cell.size, cell)

func `[]=`*[T](grid: var BlockGrid[T], pos: Pos, cell: GridCell[T]): void =
  grid.grid[pos] = (cell.size, cell)

iterator itercells*[T](grid: BlockGrid[T]): (Pos, Option[GridCell[T]]) =
  for (pos, cell) in grid.grid.elems.itercells():
    yield (makePos(pos), cell)

#============================  constructors  =============================#

func toMulticell*[T](grid: Seq2D[Option[GridCell[T]]]): MulticellGrid[GridCell[T]] =
  for (pos, cell) in grid.iterSomeCells():
    result.fillToSize(pos.makePos().expandSize(cell.size))
    result[pos.makePos()] = (cell.size, cell)

func makeCell*[T](
  arg: T,
  cellSize: (int, int) = (1, 1),
  policies: (SizePolicy, SizePolicy) = (spExpanding, spExpanding)
                ): GridCell[T] =
  GridCell[T](
    isItem: true,
    item: arg,
    size: makeSize(cellSize),
    vertPolicy: policies[0],
    horizPolicy: policies[1]
  )

func toCell*[T](
  grid: BlockGrid[T], size: Size = size1x1): GridCell[T] =
  GridCell[T](isItem: false, grid: grid, size: size)

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

func makeCell*(text: StrBlock): GridCell[StrBlock] =
  makeCell(text, (1, 1))

func makeGrid*[T](arg: MulticellGrid[GridCell[T]], conf: TermGridConf): BlockGrid[T] =
  result = BlockGrid[T](grid: arg, borders: conf)

func makeGrid*[T](rows, cols: int, borders: TermGridConf): BlockGrid[T] =
  result.borders = borders
  result.grid = makeMulticell[GridCell[T]](rows, cols)

func makeGrid*(arg: Seq2D[StrBlock],
               conf: TermGridConf): BlockGrid[StrBlock] =
  makeGrid(mapIt2d(
    arg,
    some(makeCell(it)),
    @[""].toBlock()
  ).toMulticell(), conf)

func addHeader*[T](grid: var BlockGrid[T], cell: GridCell[T]): void =
  var cell = cell
  cell.vertPolicy = spExpanding
  cell.horizPolicy = spExpanding

  grid.grid.prepend(@[cell])

#==========================  string conversion  ==========================#

func toStringBlock*[T](
  grid: BlockGrid[T],
  toStr: (proc(it: T): StrBlock) = (proc(it: T): StrBlock = ($it).split("\n"))): StrBlock

func toStringBlock*[T](
  cell: GridCell[T],
  toStr: proc(it: T): StrBlock = (proc(it: T): StrBlock = ($it).split("\n"))): StrBlock =
  case cell.isItem:
    of true:
      result = toStr(cell.item)
    of false:
      result = cell.grid.toStringBlock(toStr)

func toStringBlock*[T](
  grid: BlockGrid[T],
  toStr: proc(it: T): StrBlock = (proc(it: T): StrBlock = ($it).split("\n"))): StrBlock =

  let cells: Seq2D[Option[(Size, StrBlock)]] = grid.grid.elems.mapIt2D(
    block:
      expectType(it, Option[GridCell[T]])
      if it.isSome():
        some((it.get().size, it.get().toStringBlock(toStr)))
      else:
        none((Size, StrBlock))
    ,
    none((Size, StrBlock))
  )

  newTermMultiGrid((0, 0), cells, grid.borders).toStringBlock()
