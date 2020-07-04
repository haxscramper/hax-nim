type
  Size* = object
    width: int
    height: int

func width*(size: Size): int = size.width
func height*(size: Size): int = size.height
func makeSize*(w, h: int): Size = Size(width: w, height: h)

type
  StrSeq* = seq[string]

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

func rowAppend*[T](s: var Seq2D[T], elem: T, idx: int): void =
  ## Add element to `idx` row
  elems[idx].add elem

func newRow*[T](s: var Seq2D[T]): void =
  ## Add new row
  var tmp: seq[T]
  s.elems.add tmp

func addLast*[T](s: var Seq2D[T], elem: T): void =
  ## Add new element to last row
  s.elems[^1].add elem

converter toSeq2D*[T](s: seq[seq[T]]): Seq2d[T] =
  Seq2D[T](elems: s)

iterator items*[T](s: Seq2d[T]): seq[T] =
  for row in s.elems:
    yield row

iterator columns*[T](s: Seq2D[T], row: int): T =
  for item in s.elems[row]:
    yield item

func `[]`*[T](grid: Seq2d[T], row, col: int): T =
  grid.elems[row][col]

func `[]`*[T](grid: Seq2d[T], cell: (int, int)): T =
  grid.elems[cell[0]][cell[1]]

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

import tables

type
  SparseGrid[T] = object
    elems: Table[int, Table[int, T]]


func add*[T](s: var SparseGrid[T], row: seq[T]): void =
  s.elems[s.elems.keys().max() + 1] row

func prepend*[T](s: var SparseGrid[T], row: seq[T]): void =
  var newRow: Table[int, T]
  for idx, item in row:
    newRow[idx] = item

  s.elems[s.elems.keys().min() - 1] = newRow()

func rowlen*[T](s: SparseGrid[T], row: int): void =
  s.elems[row].len

func rowNum*[T](s: SparseGrid[T]): int =
  ## Get number or rows in 2d sequence
  s.elems.len

func rowAppend*[T](s: var SparseGrid[T], elem: T, idx: int): void =
  ## Add element to `idx` row
  if s.keys().min() <= idx and idx <= s.keys().max():
    let row = s.elems.mgetOrPut(idx, newTable[int, T]())
    row[row.keys().max() + 1] = elem
  else:
    raiseAssert("Sdfasdfasdf")

func newRow*[T](s: var SparseGrid[T]): void =
  ## Add new row
  var tmp: seq[T]
  s.elems.add tmp

func addLast*[T](s: var SparseGrid[T], elem: T): void =
  ## Add new element to last row
  s.elems[^1].add elem

converter toSparseGrid*[T](s: seq[seq[T]]): SparseGrid[T] =
  SparseGrid[T](elems: s)

iterator items*[T](s: SparseGrid[T]): seq[tuple[idx: int, row: T]] =
  for idx, row in s.elems:
    yield (idx: idx, row: row)

iterator columns*[T](s: SparseGrid[T], row: int
                    ): seq[tuple[idx: int, cell: T]] =
  for idx, cell in s.elems[row]:
    yield (idx: idx, cell: cell)

func `[]`*[T](grid: SparseGrid[T], row, col: int): T =
  grid.elems[row][col]

func `[]`*[T](grid: SparseGrid[T], cell: (int, int)): T =
  grid.elems[cell[0]][cell[1]]

template mapIt2d*[T](inseq: SparseGrid[T], op: untyped): untyped =
  type ResT = typeof((
    block:
      var it {.inject.}: tuple[row, col: int, val: T]
      op))

  var result: SparseGrid[ResT]
  for rowIdx, row in inseq.elems:
    result.newRow
    for colIdx, cell in row:
      let it {.inject.} = (row: rowIdx, col: colIdx, val: col)
      result.addLast op

  result
