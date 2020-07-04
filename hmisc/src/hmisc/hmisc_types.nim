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

func rowNum*[T](s: Seq2D[T]): int =
  ## Get number or rows in 2d sequence
  s.elems.len

func rowAppend*[T](s: var Seq2D[T], elem: T, idx: int): void =
  ## Add element to `idx` row
  elems[idx].add elem

# template eachRow*[T](s: Seq2D[T], op: untyped): untyped =
#   type ResT = typeof((
#     block:
#       var it {.inject.}: seq[T]
#       op))

#   var result: seq[ResT]
#   for row in s.elems:
#     let it {.inject.} = row
#     result.add op

func newRow*[T](s: var Seq2D[T]): void =
  ## Add new row
  var tmp: seq[T]
  s.elems.add tmp

func addLast*[T](s: var Seq2D, elem: T): void =
  ## Add new element to last row
  s.elems[^1].add elem

converter toSeq2D*[T](s: seq[seq[T]]): Seq2d[T] =
  Seq2D[T](elems: s)

iterator items*[T](s: Seq2d[T]): seq[T] =
  for row in s.elems:
    yield row

func `[]`*[T](grid: Seq2d[T], row, col: int): T =
  grid.elems[row][col]

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
