import sequtils, options

type
  Seq2d*[T] = object
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

func colNum*[T](s: Seq2D[T], expectUniform: bool = true): int =
  ## Get max number of columns in 2d sequence. If `expecUniform` check
  ## that all rows have equal lentgth
  if s.elems.len == 0:
    result = 0
  else:
    result = s.elems.mapIt(it.len).max(0)
    if expectUniform:
      for idx, row in s.elems:
        assert row.len == result, "Cannot get number of columns for 2d " &
          &"sequence row {idx} has {row.len} elements, but expected " &
          &"{result}"

func len*[T](s: Seq2D[T]): int =
  s.elems.len

func rowAppend*[T](s: var Seq2D[T], elem: T, idx: int): void =
  ## Add element to `idx` row
  s.elems[idx].add elem

func newRow*[T](s: var Seq2D[T]): void =
  ## Add new row
  var tmp: seq[T]
  s.elems.add tmp

func addLast*[T](s: var Seq2D[T], elem: T): void =
  ## Add new element to last row
  s.elems[^1].add elem

func makeSeq2D*[T](s: seq[seq[T]]): Seq2d[T] =
  Seq2D[T](elems: s)

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

iterator itercols*[T](s: Seq2D[T], default: T): seq[T] =
  let rowlen = s.elems.mapIt(it.len).max()
  # for row in s.elems:
  #   assert row.len == rowlen, "Row lenths differ"

  for col in 0 ..< rowlen:
    var buf: seq[T]
    for row in s.elems:
      if col > row.len - 1:
        buf.add default
      else:
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

func `[]=`*[T](grid: Seq2d[T], cell: (int, int), val: T): T =
  grid.elems[cell[0]][cell[1]] = val

func concat*[T](inseq: Seq2d[T]): seq[T] = inseq.elems.concat()

template mapIt2d*[T](inseq: Seq2d[T], op: untyped): untyped =
  type ResT = typeof((
    block:
      var it {.inject.}: T
      op))

  var result: Seq2d[ResT]
  for row in inseq.elems:
    result.newRow
    for col in row:
      let it {.inject.} = col
      result.addLast op

  result

template maximizeColIt*[T](
  inseq: Seq2d[T], op: untyped, default: T): seq[int] =
  ## Iiterate over all columns in grid. Execute `op` for each cell in
  ## column and save max value from each column
  type ResType = typeof((var it {.inject.}: T; op))
  var res: seq[ResType]
  var idx = 0
  for col in inseq.itercols(default):
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
