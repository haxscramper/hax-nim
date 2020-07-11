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
