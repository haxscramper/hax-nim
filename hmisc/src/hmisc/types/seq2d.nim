import sequtils, options, hprimitives, strformat, strutils
import ../hdebug_misc
import ../algo/[halgorithm, hseq_mapping]

type
  Seq2d*[T] = object
    colWidth: int
    elems: seq[seq[T]]

func add*[T](s: var Seq2d[T], row: seq[T]): void =
  s.elems.add row

func prepend*[T](s: var Seq2D[T], row: seq[T]): void =
  s.elems = row & s.elems

func rowlen*[T](s: Seq2D[T], row: int): void =
  s.elems[row].len

func rowNum*[T](s: Seq2D[T]): int =
  ## Get number or rows in 2d sequence
  s.elems.len

func fillToSize*[T](grid: var Seq2D[T], size: ArrSize, val: T): void =
  ## Make sure `grid` is of `size` (or lagrger). Fill missing elements
  ## using `val`.
  grid.colWidth = size.width
  for row in 0 ..< size.height:
    if not (row < grid.elems.len):
      grid.elems.add @[]

    let rowlen = grid.elems[row].len
    if rowlen < size.width:
      grid.elems[row] &= newSeqWith(size.width - rowlen, val)

func colNum*[T](s: Seq2D[T]): int =
  ## Get max number of columns in 2d sequence. If `expecUniform` check
  ## that all rows have equal lentgth
  result = s.colWidth
  for idx, row in s.elems:
    assert row.len == result,
      &"Invariant invalidated: [idx].len: {row.len}, column: {result}"


func size*[T](s: Seq2D[T]): ArrSize =
  makeArrSize(s.colNum(), s.rowNum())

func len*[T](s: Seq2D[T]): int =
  s.elems.len

func rowAppend*[T](s: var Seq2D[T], elem: T, idx: int): void =
  ## Add element to `idx` row
  s.elems[idx].add elem

func newRow*[T](s: var Seq2D[T]): void =
  ## Add new row
  var tmp: seq[T]
  s.elems.add tmp

func insertRow*[T](grid: var Seq2D[T], row: seq[T], idx: int = 0): void =
  ##[

Insert new row in the 2d grid.

`row` length MUST be equal to number of columns in `grid`. `idx` is
the index of new row. `idx` MUST be in range `[0, grid.rowNum()]`
(inclsive)

  ]##
  assert row.len == grid.colNum()
  if idx == 0:
    grid.elems = row & grid.elems
  elif idx == grid.rowNum():
    grid.elems = grid.elems & row
  else:
    discard #[ IMPLEMENT ]#

func insertCol*[T](grid: var Seq2D[T], col: seq[T], idx: int = 0): void =
  ##[

Insert new col in the 2d grid.

`col` length MUST be equal to number of rows in `grid`. `idx` is the
index of new column. `idx` MUST be in range `[0, grid.colNum()]`
(inclusive)

  ]##
  #[ IMPLEMENT ]#
  discard

func addLast*[T](s: var Seq2D[T], elem: T): void =
  ## Add new element to last row
  s.elems[^1].add elem

func makeSeq2D*[T](s: seq[seq[T]], default: T): Seq2d[T] =
  let maxlen = s.mapIt(it.len).max()
  var inseq = s
  for idx, row in s:
    if row.len < maxlen:
      inseq[idx] &= newSeqWith(maxlen - row.len, default)

  Seq2D[T](elems: inseq, colWidth: maxlen)

func makeSeq2D*[T](s: seq[seq[T]]): Seq2d[T] =
  let maxlen = s.mapIt(it.len).max()
  for idx, row in s:
    assert row.len == maxlen, "Cannot create 2d sequence from non-uniform " &
      &"sequence. Row {idx} has {row.len} elements, but expected " &
      &"{maxlen}"

  Seq2D[T](elems: s, colWidth: maxlen)

iterator items*[T](s: Seq2d[T]): seq[T] =
  for row in s.elems:
    yield row

iterator pairs*[T](s: Seq2d[T]): (int, seq[T]) =
  for idx, row in s.elems:
    yield (idx, row)

iterator columns*[T](s: Seq2D[T], row: int): T =
  for item in s.elems[row]:
    yield item

iterator iterrows*[T](s: Seq2D[T]): seq[T] =
  for row in s.elems:
    yield row

iterator itercols*[T](s: Seq2D[T]): seq[T] =
  let rowlen = s.elems.mapIt(it.len).max()
  for col in 0 ..< rowlen:
    var buf: seq[T]
    for row in s.elems:
      buf.add row[col]

    yield buf

iterator itercells*[T](s: Seq2D[T]): ((int, int), T) =
  for rowId, row in s.elems:
    for colId, cell in row:
      yield((rowId, colId), cell)

iterator iterSomeCells*[T](s: Seq2D[Option[T]]): ((int, int), T) =
  for (pos, cell) in s.itercells:
    if cell.isSome():
      yield (pos, cell.get())

func `[]`*[T](grid: Seq2d[T], row, col: int): T =
  grid.elems[row][col]

func `[]`*[T](grid: Seq2d[T], cell: (int, int)): T =
  grid.elems[cell[0]][cell[1]]

func `[]`*[T](grid: Seq2d[T], pos: ArrPos): T =
  grid.elems[pos.row][pos.col]

func `[]=`*[T](grid: var Seq2d[T], cell: (int, int), val: T): void =
  grid.elems[cell[0]][cell[1]] = val

func `[]=`*[T](grid: var Seq2d[T], row, col: int, val: T): void =
  grid.elems[row][col] = val

func `[]=`*[T](grid: var Seq2d[T], pos: ArrPos, val: T): void =
  grid.elems[pos.row][pos.col] = val

func concat*[T](inseq: Seq2d[T]): seq[T] = inseq.elems.concat()
template mapIt2d*[T](inseq: Seq2d[T], op: untyped): untyped =
  type ResT = typeof((
    block:
      var it {.inject.}: T
      op))

  var result: seq[seq[ResT]]
  for row in inseq.elems:
    result.add @[]
    for col in row:
      let it {.inject.} = col
      result[^1].add op

  var res: Seq2D[ResT]
  try:
    res = makeSeq2D(result)
  except AssertionError:
    {.noSideEffect.}:
      let msg = getCurrentExceptionMsg()
      let info = instantiationInfo()
      raiseAssert(
        msg & ". Template `mapIt2D` instantiated on line: " & $info.line &
          ", file: " & $info.filename)

  res


template mapIt2d*[T](inseq: Seq2d[T], op: untyped, default: typed): untyped =
  type ResT = typeof((var it {.inject.}: T; op))
  var res: seq[seq[ResT]]
  for row in inseq.elems:
    res.add @[]
    for col in row:
      let it {.inject.} = col
      res[^1].add op

  makeSeq2D(
    res,
    when default is ResT:
      default
    else:
      let it {.inject.}: T = default
      let defOp = op
      defOp
  )

template maximizeColIt*[T](inseq: Seq2d[T], op: untyped): seq[int] =
  ## Iiterate over all columns in grid. Execute `op` for each cell in
  ## column and save max value from each column
  type ResType = typeof((var it {.inject.}: T; op))
  var res: seq[ResType]
  var idx = 0
  for col in inseq.itercols():
    var buf: seq[ResType]
    for cell in col:
      let it {.inject.} = cell
      buf.add op

    if buf.len == 0:
      raiseAssert("Failed to get any from column " & $idx)
    else:
      res.add buf.max()

    inc idx

  res

template maximizeRowIt*[T](
  inseq: Seq2d[T], op: untyped): seq[int] =
  ## Iiterate over all rows in grid. Execute `op` for each cell in
  ## column and save max value from each column
  type ResType = typeof((var it {.inject.}: T; op))
  var res: seq[ResType]
  var idx = 0
  for col in inseq.iterrows():
    var buf: seq[ResType]
    for cell in col:
      let it {.inject.} = cell
      buf.add op

    if buf.len == 0:
      raiseAssert("Failed to get any from row " & $idx)
    else:
      res.add buf.max()

    inc idx

  res

func contains*[T](grid: Seq2D[T], pos: ArrPos): bool =
  (pos.row < grid.elems.len) and (pos.col < grid.elems[pos.row].len)

template checkIfIt*[T](grid: Seq2d[Option[T]], pos: ArrPos, op: untyped): bool =
  ## If `pos.isValid() and (pos in grid) and grid[pos].isSome()`
  ## run predicate. `it: T` is injected in scope.
  if (pos.isValid()) and (pos in grid) and (grid[pos].isSome()):
    let it {.inject} = grid[pos].get()
    static: assert ((op) is bool)
    op
  else:
    false

func toStrGrid*(grid: seq[seq[string]], default: StrBlock): Seq2D[StrBlock] =
  makeSeq2d(
    grid.mapIt(it.mapIt(it.split("\n"))),
    default
  )

#*************************************************************************#
#***************************  multicell grid  ****************************#
#*************************************************************************#

type
  MulticellLookup* = Seq2D[Option[ArrRect]]
  MulticellGrid*[T] = object
    elems*: Seq2D[Option[T]]
    lookup: MulticellLookup

#=============================  constructor  =============================#

func makeLookup*(grid: Seq2d[Option[ArrSize]]): MulticellLookup =
  result = grid.mapIt2d(none((ArrRect)))
  for (pos, size) in grid.iterSomeCells():
    for (row, col) in (
      pos.rowRange(size),
      pos.colRange(size)
    ):
      if result[row, col].isSome():
        raiseAssert &"Cannot set cell at position {pos}: {(row, col)} is already occupied"
      else:
        result[row, col] = some(makeArrrect(pos, size))



#==============================  accessor  ===============================#


func `[]=`*[T](grid: var MulticellGrid[T], rect: ArrRect, val: T): void =
  for (row, col) in rect.itercells():
    if grid.lookup[row, col].isSome():
      raiseAssert &"Cannot set cell at rec {rect}: " &
       &"{(row, col)} is already occupied"

  for (row, col) in rect.itercells():
    grid.lookup[row, col] = some(rect)

  grid.elems[rect.pos] = some(val)

iterator cellsAround*(lookup: MulticellLookup, pos: ArrPos): tuple[
  pos: ArrPos, size: ArrSize, rp: RelPos] =
  ## Iterate over all cells adjacent to cell at `pos`
  if lookup[pos].isSome():
    var fringes: seq[(ArrPos, RelPos)]
    let rect = lookup[pos].get()
    for (row, col) in rect.itercells():
      for shift in @[rpLeft, rpRight, rpBottom, rpTop]:
        let adjacent = makeArrPos(row, col).shiftRC(shift.toDiffRC())
        if lookup.checkIfIt(adjacent, it.pos != rect.pos):
          fringes.add (lookup[adjacent].get().pos, shift)

    for adjPos in fringes.deduplicateIt(it[0]):
      yield (
        pos: adjPos[0],
        size: lookup[adjPos[0]].get().size,
        rp: adjPos[1]
      )

#============================  modification  =============================#

func fillToSize*[T](grid: var MulticellGrid[T], size: ArrSize): void =
  grid.elems.fillToSize(size, none(T))
  grid.lookup.fillToSize(size, none(ArrRect))

func makeMulticell*[T](rows, cols: int): MulticellGrid[T] =
  ## Make empty multicell grid
  result.fillToSize(makeArrSize(h = rows, w = cols))

func addHeader*[T](grid: var MulticellGrid[T], colIdx, width: int, val: T): void =
  ##[

Prepend header cell to grid.

  ]##
  var newrow = newSeqWith(grid.elems.colNum(), none(T))
  var newLookup = newSeqWith(grid.lookup.colNum(), none(ArrRect))

  grid.elems.insertRow(newRow, 0)
  grid.lookup.insertRow(newLookup, 0)
  grid[makeArrRect((0, colIdx), w = width, h = 1)] = val
