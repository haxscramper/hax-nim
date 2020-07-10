import sugar, sequtils
import helpers
import sorta
export sorta

type
  Map*[K, V] = SortedTable[K, V]

type
  Size* = object
    width: int
    height: int

func initMap*[K, V](): Map[K, V] = initSortedTable[K, V]()
func `[]=`*[K, V](map: var Map[K, V], key: K, val: V): void =
  sorta.`[]=`(map, key, val)
# func `[]`*[K, V](map: Map[K, V], key: K): V =
#   map.getOrDefault(K)

func toMap*[K, V](its: seq[(K, V)]): Map[K, V] =
  result = initMap[K, V]()
  for (key, val) in its:
    result[key] = val

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
  s.elems[idx].add elem

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

func `[]`*[T](grid: Seq2d[T], row, col: int): T =
  grid.elems[row][col]

func `[]`*[T](grid: Seq2d[T], cell: (int, int)): T =
  grid.elems[cell[0]][cell[1]]

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

import tables

type
  SparseGrid*[T] = object
    elems: Map[int, Map[int, T]]

func maxRow*[T](s: SparseGrid[T]): int =
  toSeq(s.elems.keys()).max()

func minRow*[T](s: SparseGrid[T]): int =
  toSeq(s.elems.keys()).min()

func add*[T](s: var SparseGrid[T], row: seq[T]): void =
  s.elems[s.elems.keys().max() + 1] row

func prepend*[T](s: var SparseGrid[T], row: seq[T]): void =
  var newRow = initMap[int, T]()
  for idx, item in row:
    newRow[idx] = item

  s.elems[s.minRow() - 1] = newRow

  s.elems =
    block:
      var tmp = initMap[int, Map[int, T]]()
      for idx, v in s.elems:
        tmp[idx + 1] = v

      tmp

func rowAppend*[T](s: var SparseGrid[T], elem: T, idx: int): void =
  ## Add element to `idx` row
  if s.minRow() <= idx and idx <= s.maxRow():
    let row = s.elems.mgetOrPut(idx, newTable[int, T]())
    row[s.maxRow() + 1] = elem
  else:
    raiseAssert("Sdfasdfasdf")

converter toSparseGrid*[T](s: seq[seq[T]]): SparseGrid[T] =
  SparseGrid[T](
    elems:
      block:
        var elems = initMap[int, Map[int, T]]()
        for rowIdx, row in s:
          elems[rowIdx] =
            block:
              var cells = initMap[int, T]()
              for colIdx, cell in row:
                cells[colIdx] = cell
              cells

        elems
  )

iterator rows*[T](s: SparseGrid[T]): tuple[
  idx: int, row: Map[int, T]] =
  for key in toSeq(s.elems.keys()).sorted():
    yield (idx: key, row: s.elems[key])

iterator columns*[T](s: SparseGrid[T], row: int
                    ): tuple[idx: int, cell: T] =
  for idx in toSeq(s.elems[row].keys()).sorted():
    yield (idx: idx, cell: s.elems[row][idx])

func firstColumn*[T](grid: SparseGrid[T], row: int): int =
  toSeq(grid.elems[row].keys()).sorted()[0]

func `[]`*[T](grid: SparseGrid[T], row, col: int): T =
  grid.elems[row][col]

func `[]`*[T](grid: SparseGrid[T], cell: (int, int)): T =
  grid.elems[cell[0]][cell[1]]

func `[]=`*[T](grid: var SparseGrid[T], pos: (int, int), val: T): void =
  if pos[0] notin grid.elems:
    grid.elems[pos[0]] = initMap[int, T]()

  var gr = grid.elems[pos[0]]
  gr[pos[1]] = val

template mapIt2d*[T](inseq: SparseGrid[T], op: untyped): untyped =
  type ResT = typeof((
    block:
      var it {.inject.}: T
      var rowIdx {.inject.}: int
      var colIdx {.inject.}: int
      op))

  var result: SparseGrid[ResT]
  for rowId, row in inseq.elems:
    for colId, cell in row:
      let it {.inject.} = cell
      let rowIdx {.inject.}: int = rowId
      let colIdx {.inject.}: int = colId
      result[(rowIdx, colIdx)] = op

  result

template mapItRows*[T](inseq: SparseGrid[T], op: untyped): untyped =
  type ResT = typeof((
    block:
      var it {.inject.}: Table[int, T]
      var rowIdx {.inject.}: int
      op))

  var result: seq[tuple[idx: int, val: ResT]]
  for rowId, row in inseq.elems:
    let it {.inject.} = row
    let rowIdx {.inject.}: int = rowId
    result.add (idx: rowIdx, val: op)

  result
