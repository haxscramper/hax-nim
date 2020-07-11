## Nested table with multicol support

#===========================  type definition  ===========================#

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
    borders*: TermGridConf
    grid*: SparseGrid[GridCell[T]] ## row[col[cell]]
    maxH: Map[int, int] ## Max height in each row
    maxW: Map[int, int] ## Max width in each column


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

iterator itercells*[T](grid: BlockGrid[T]): ((int, int), GridCell[T]) =
  for (pos, cell) in grid.grid.itercells():
    yield (pos, cell)

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

func `[]=`*[T](
  grid: var BlockGrid[T], row, col: int, cell: GridCell[T]): void =
    grid.grid[(row, col)] = cell
    grid.maxW.maxOrSet(col, cell.width)
    grid.maxH.maxOrSet(row, cell.height)

#============================  constructors  =============================#

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

func makeGrid*[T](arg: Seq2D[GridCell[T]], default: GridCell[T], conf: TermGridConf): BlockGrid[T] =
  var maxColw: CountTable[int]
  var maxIdx: int = 0
  let cellws: seq[int] = collect(newSeq):
    for col in arg.itercols(default):
      col.mapIt(it.width).max(0)

  let cellhs: seq[int] = collect(newSeq):
    for row in arg.iterrows():
      row.mapIt(it.height).max(0)

  result = BlockGrid[T](
    grid: arg.toSparse(),
    maxW: cellws.toMap(),
    maxH: cellhs.toMap(),
    borders: conf
  )

func makeGrid*[T](
  arg: Seq2D[tuple[item: T, w, h: int]],
  default: tuple[item: T, w, h: int],
  conf: TermGridConf): BlockGrid[T] =
  makeGrid(
    mapIt2d(arg, it.item.makeCell(it.w, it.h)),
    makeCell(default.item, default.w, default.h),
    conf
  )

func makeGrid*(arg: Seq2D[string], conf: TermGridConf): BlockGrid[string] =
  makeGrid[string](arg.mapIt2d(makeCell(it, it.len, 1)), makeCell("", 0, 0), conf)

func makeGrid*(arg: Seq2D[seq[string]], conf: TermGridConf): BlockGrid[seq[string]] =
  makeGrid(arg.mapIt2d(makeCell(
    it, it.mapIt(it.len).max(0), it.len
  )), makeCell(@[""], 0, 0), conf)

func addHeader*[T](grid: var BlockGrid[T], cell: GridCell[T]): void =
  var cell = cell
  cell.vertPolicy = spExpanding
  cell.horizPolicy = spExpanding

  grid.grid.prepend(@[cell])

#==========================  string conversion  ==========================#

func toString*[T](grid: BlockGrid[T], conf: TermGridConf): seq[string]
func toString*[T](cell: GridCell[T], conf: TermGridConf): seq[string] =
  discard

func toString*[T](grid: BlockGrid[T], conf: TermGridConf): seq[string] =
  let cells: Seq2D[string] = grid.grid.mapIt2D(toString(it, conf))
  newTermGrid((0, 0), cells, conf).toStringBlock()
