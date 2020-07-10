import sequtils, strutils, math, lenientops, helpers, strformat, tables
import sugar
import hmisc_types
import halgorithm
import unicode

func sumjoin*(a: openarray[int], sep: int): int =
  a.sum() + (a.len > 0).tern((a.len() - 1) * sep, 0)

func cumsumjoin*(
  a: openarray[int], sep: int, addFirst: bool = false): seq[int] =
  var curr: int = 0
  if addFirst:
    result.add curr

  for val in a:
    result.add(val + sep + curr)
    curr += val + sep

type
  Shape* = ref object of RootObj
  RuneSeq = seq[Rune]
  TermBuf* = object
    buf: seq[seq[Rune]]
    xDiff: int
    yDiff: int

  Point*[Num] = object
    x: Num
    y: Num

  Radian* = float

converter toRune*(c: char): Rune = toRunes($c)[0]

const whitespaceRune: Rune = toRunes(" ")[0]

func setAtPoint(buf: var TermBuf, row, col: int, rune: Rune): void =
  for _ in buf.buf.len .. row:
    buf.buf.add @[]

  for r in 0 .. row:
    let rowLen = buf.buf[r].len
    if rowLen - 1 < col:
      buf.buf[r] &= whitespaceRune.repeat(col - rowLen + 1).toRunes()

  buf.buf[row][col] = rune

func `[]=`*(buf: var TermBuf, x, y: int, rune: Rune): void =
  let y = y + buf.yDiff
  let x = x + buf.xDiff
  buf.setAtPoint(y, x, rune)

func `[]=`*(buf: var TermBuf, x, y: int, c: char): void =
  buf[x, y] = toRunes($c)[0]

func `[]=`*(buf: var TermBuf, pos: Point[int], c: Rune): void =
  buf[pos.x, pos.y] = c

func newBuf*(offset: (int, int) = (0, 0)): TermBuf =
  TermBuf(xDiff: offset[0], yDiff: offset[1])

func `$`*(buf: TermBuf): string = buf.buf.join("\n")


method render*(s: Shape, buf: var TermBuf): void {.base.} =
  raiseAssert("Cannot draw base shape implementation")

type
  SPoint*[T, Num] = ref object of Shape
    point: Point[Num]
    config: T

  SLine*[T, Num] = ref object of Shape
    start: Point[Num]
    length: Num
    angle: Radian
    config: T

  SText*[Num] = ref object of Shape
    start: Point[Num]
    width: Num
    height: Num
    case reflow: bool
      of true:
        text: string
      of false:
        lines: seq[RuneSeq]

  Multishape* = ref object of Shape
    shapes: seq[Shape]

  RectPoint* = enum
    rpoLeftEdge
    rpoRightEdge
    rpoBottomEdge
    rpoTopEdge
    rpoTopLeft
    rpoTopRight
    rpoBottomLeft
    rpoBottomRight

  SRect*[T, Num] = ref object of Shape
    upLeft: Point[Num]
    height: Num
    width: Num
    config: T

  TermRectConf* = Table[RectPoint, Rune]
  TermRect* = SRect[TermRectConf, int]

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

  SGrid*[T, Num] = ref object of Shape
    start: Point[Num]
    cellWidths: seq[Num]
    cellHeights: seq[Num]
    config: T

  TermGridConf* = Table[GridPoint, Rune]
  TermGrid* = SGrid[TermGridConf, int]

func makePoint*[Num](x, y: Num): auto = Point[Num](x: x, y: y)
func makePoint*[Num](pos: (Num, Num)): auto = Point[Num](x: pos[0], y: pos[1])
func shiftX*[Num](p: Point[Num], dx: Num): Point[Num] =
  makePoint[Num](p.x + dx, p.y)

func shiftY*[Num](p: Point[Num], dy: Num): Point[Num] =
  makePoint[Num](p.x, p.y + dy)


func shiftXY*[Num](p: Point[Num], dx: Num, dy: Num): Point[Num] =
  makePoint[Num](p.x + dx, p.y + dy)

func shiftXY*[Num](p: (Num, Num), dx: Num, dy: Num): (Num, Num) =
  (p[0] + dx, p[1] + dy)

func x*[T, Num](p: SPoint[T, Num]): Num = p.point.x
func y*[T, Num](p: SPoint[T, Num]): Num = p.point.y


iterator `..+`*(start: int, offset: int): int =
  for i in start ..< start + offset:
    yield i

func order[Num](a, b: Num): (Num, Num) =
  if a > b: (b, a)
  else: (a, b)

func renderLine(x0, y0, x1, y1: int, buf: var TermBuf, rune: Rune): void =
  # de x0, y0, x1, y1
  let
    dx = x1 - x0
    dy = y1 - y0
    steps = if abs(dx) > abs(dy): abs(dx) else: abs(dy)
    xInc = dx / float(steps)
    yInc = dy / float(steps)

  var
    x = x0
    y = y0

  for i in 0 .. steps:
    buf[x, y] = rune
    x += xInc.int
    y += yInc.int

func renderLine*(p: Point[int], w, h: int, buf: var TermBuf, c: Rune): void =
  renderLine(
    p.x,
    p.y,
    p.x + w,
    p.y + h,
    buf,
    c
  )

method render*(line: SLine[char, int], buf: var TermBuf): void =
  renderLine(
    x0 = line.start.x,
    y0 = line.start.y,
    x1 = int(line.start.x + cos(line.angle) * line.length),
    y1 = int(line.start.y + sin(line.angle) * line.length),
    buf = buf,
    rune = line.config
  )



method render*(rect: SRect[char, int], buf: var TermBuf): void =
  renderLine(
    rect.upLeft, rect.width, 0, buf, rect.config)
  renderLine(
    rect.upLeft, 0, rect.height - 1, buf, rect.config)

  renderLine(
    rect.upLeft.shiftY(rect.height - 1), rect.width, 0, buf, rect.config)
  renderLine(
    rect.upLeft.shiftX(rect.width), 0, rect.height - 1, buf, rect.config)


method render*(rect: TermRect, buf: var TermBuf): void =
  let h = rect.height
  let w = rect.width

  if rpoLeftEdge in rect.config:
    renderLine(
      rect.upLeft, 0, h - 1, buf, rect.config[rpoLeftEdge])

  if rpoRightEdge in rect.config:
    renderLine(
      rect.upLeft.shiftX(w - 1), 0, h - 1, buf, rect.config[rpoRightEdge])

  if rpoTopEdge in rect.config:
    renderLine(rect.upLeft, w - 1, 0, buf, rect.config[rpoTopEdge])

  if rpoBottomEdge in rect.config:
    renderLine(rect.upLeft.shiftY(h - 1), w - 1, h, buf, rect.config[rpoBottomEdge])

  if rpoTopLeft in rect.config:
    buf[rect.upLeft.shiftXY(0, 0)] = rect.config[rpoTopLeft]

  if rpoTopRight in rect.config:
    buf[rect.upLeft.shiftXY(w - 1, 0)] = rect.config[rpoTopRight]

  if rpoBottomLeft in rect.config:
    buf[rect.upLeft.shiftXY(0, h - 1)] = rect.config[rpoBottomLeft]

  if rpoBottomRight in rect.config:
    buf[rect.upLeft.shiftXY(w - 1, h - 1)] = rect.config[rpoBottomRight]

func gridDimensions*(grid: TermGrid): tuple[vSpacing, hSpacing, totalW, totalH: int] =
  let rc = grid.config
  result.vSpacing = (
    (gpoHorizontalGap in rc) or
    (gpoLeftIntersection in rc) or
    (gpoRightIntersection in rc)
  ).tern(1, 0)

  result.hSpacing = (
    (gpoVerticalGap in rc) or
    (gpoTopIntersection in rc) or
    (gpoBottomIntersection in rc)
  ).tern(1, 0)

  result.totalW = grid.cellWidths.sumjoin(result.vSpacing)
  result.totalH = grid.cellHeights.sumjoin(result.hSpacing)

method render*(rect: TermGrid, buf: var TermBuf): void =
  let gridX = rect.start.x
  let gridY = rect.start.y
  let rc = rect.config
  let (vSpacing, hSpacing, totalW, totalH) = gridDimensions(rect)

  block outerBorder:
    if gpoTopBorder in rc:
      renderLine(
        x0 = gridX, x1 = gridX + totalW,
        y0 = gridY, y1 = gridY,
        buf, rc[gpoTopBorder])

    if gpoBottomBorder in rc:
      renderLine(
        x0 = gridX, x1 = gridX + totalW,
        y0 = gridY + totalH + 1, y1 = gridY + totalH + 1,
        buf, rc[gpoBottomBorder])

    if gpoLeftBorder in rc:
      renderLine(
        x0 = gridX, x1 = gridX,
        y0 = gridY, y1 = gridY + totalH,
        buf, rc[gpoLeftBorder])

    if gpoRightBorder in rc:
      renderLine(
        x0 = gridX + totalW + 1, x1 = gridX + totalW + 1,
        y0 = gridY, y1 = gridY + totalH,
        buf, rc[gpoRightBorder])

    if gpoTopLeft in rc: buf[gridX, gridY] = rc[gpoTopLeft]
    if gpoBottomLeft in rc: buf[gridX, gridY + totalH + 1] = rc[gpoBottomLeft]
    if gpoTopRight in rc: buf[gridX + totalW + 1, gridY] = rc[gpoTopRight]
    if gpoBottomRight in rc: buf[gridX + totalW + 1, gridY + totalH + 1] = rc[gpoBottomRight]


  block inerGrid:
    if vSpacing == 1:
      for row in rect.cellHeights.cumsumjoin(vSpacing)[0..^2]:
        if gpoHorizontalGap in rc:
          renderLine(
            y0 = row + gridY, y1 = row + gridY,
            x0 = 0 + gridX, x1 = totalW + gridX,
            buf,
            rc[gpoHorizontalGap]
          )

        if gpoLeftIntersection in rc:
          buf[gridX, gridY + row] = rc[gpoLeftIntersection]

        if gpoRightIntersection in rc:
          buf[gridX + totalW + 1, gridY + row] = rc[gpoRightIntersection]


    if hSpacing == 1:
      for col in rect.cellWidths.cumsumjoin(vSpacing)[0..^2]:
        if gpoVerticalGap in rc:
          renderLine(
            y0 = gridY, y1 = gridY + totalH,
            x0 = gridX + col, x1 = gridX + col,
            buf,
            rc[gpoVerticalGap]
          )

        if gpoTopIntersection in rc:
          buf[gridX + col, gridY] = rc[gpoTopIntersection]

        if gpoBottomIntersection in rc:
          buf[gridX + col, gridY + totalH + 1] = rc[gpoBottomIntersection]

    if gpoIntersection in rc:
      for row in rect.cellHeights.cumsumjoin(vSpacing)[0..^2]:
        for col in rect.cellWidths.cumsumjoin(hSpacing)[0..^2]:
          buf[gridX + col, gridY + row] = rc[gpoIntersection]


method render*(multi: Multishape, buf: var TermBuf): void =
  for shape in multi.shapes:
    render(shape, buf)

method render*(text: SText[int], buf: var TermBuf): void =
  case text.reflow:
    of true:
      raiseAssert("reflow text is not implemented")
    of false:
      for rId, row in text.lines:
        for cId in 0 ..< min(row.len, text.width):
          # echo &"{text.start}, ({cId}, {rId}), {text.start.shiftXY(cId, rId)}"
          buf[text.start.shiftXY(cId, rId)] = row[cId]

method render*(point: SPoint[char, int], buf: var TermBuf): void =
  buf[point.point] = point.config

func newTermRect*(
  start: (int, int),
  width, height: int, border: char = '+'): SRect[char, int] =
  SRect[char, int](
    upLeft: makePoint(start[0], start[1]),
    config: border,
    width: width,
    height: height
  )

func newTermText*(start: (int, int), text: seq[RuneSeq]): SText[int] =
  SText[int](
    start: makePoint(start[0], start[1]),
    lines: text,
    reflow: false,
    width: toSeq(text.mapIt(it.len)).max(0),
    height: text.len
  )

func newTermVline*(
  start: (int, int), length: int, c: char = '|', isDown: bool = true): auto =
  SLine[char, int](
    angle: if isDown: (PI/2) else: 3 * (PI/2),
    config: c,
    start: start.makePoint(),
    length: length
  )

func newTermHline*(
  start: (int, int), length: int, c: char = '-', isRight: bool = true): auto =
  SLine[char, int](
    angle: if isRight: PI*0 else: PI,
    config: c,
    start: start.makePoint(),
    length: length)

func newTermPoint*(start: (int, int), c: char = '+'): SPoint[char, int] =
  SPoint[char, int](point: start.makePoint(), config: c)

func newBoxedTermText*(start: (int, int), text: seq[RuneSeq], boxc: char = '#'): Multishape =
  let inner = newTermText(start.shiftXY(1, 1), text)
  Multishape(shapes: @[
    cast[Shape](inner),
    newTermRect(
      start, width = inner.width + 2, height = inner.height + 2, border = boxc)
  ])

func makeTwoLineRectBorder*(): TermRectConf =
  {
    rpoLeftEdge : "║",
    rpoRightEdge : "║",
    rpoBottomEdge : "═",
    rpoTopEdge : "═",
    rpoTopLeft : "╔",
    rpoTopRight : "╗",
    rpoBottomLeft : "╚",
    rpoBottomRight : "╝",
  }.mapPairs((lhs, rhs.toRunes()[0])).toTable()

func newTermRect*(
  start: (int, int), width, height: int, conf: TermRectConf): TermRect =
    TermRect(
      upLeft: start.makePoint(),
      height: height,
      width: width,
      config: conf
    )

func newBoxedTermText*(
  start: (int, int),
  text: seq[RuneSeq],
  conf: TermRectConf,
  size: (int, int) = (-1, -1)): Multishape =
  let inner = newTermText(start.shiftXY(1, 1), text)
  Multishape(shapes: @[
    cast[Shape](inner),
    newTermRect(
      start,
      width = (size == (-1, -1)).tern(inner.width + 2, size[0]),
      height = (size == (-1, -1)).tern(inner.height + 2, size[1]),
      conf
    )
  ])

#================================  grid  =================================#
func makeThinLineGridBorders*(): TermGridConf =
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
  }.mapPairs((lhs, rhs.toRunes()[0])).toTable()


func makeAsciiGridBorders*(): TermGridConf =
  {
    gpoIntersection : "+",
    gpoTopLeft : "+",
    gpoTopRight : "+",
    gpoBottomLeft : "+",
    gpoBottomRight : "+",
    gpoLeftBorder : "|",
    gpoLeftIntersection : "+",
    gpoRightBorder : "|",
    gpoRightIntersection : "+",
    gpoTopBorder : "-",
    gpoTopIntersection : "+",
    gpoBottomBorder : "-",
    gpoBottomIntersection : "+",
    gpoHorizontalGap : "-",
    gpoVerticalGap : "|",
  }.mapPairs((lhs, rhs.toRunes()[0])).toTable()


func makeEmptyGridBorders*(): TermGridConf = discard

func newTermGrid*(
  start: (int, int),
  cellws: seq[int],
  cellhs: seq[int],
  conf: TermGridConf): TermGrid =
  TermGrid(
    start: start.makePoint(),
    cellWidths: cellws,
    cellHeights: cellhs,
    config: conf
  )

func newMultishape(shapes: seq[Shape]): Multishape = Multishape(shapes: shapes)


func newTermGrid*(
  start: (int, int), cells: seq[seq[RuneSeq]], conf: TermGridConf): Multishape =
  let cells: Seq2d[seq[RuneSeq]] = cells.mapIt(
    it.mapIt(($it).split('\n').mapIt(it.toRunes()))
  ).toSeq2d()
  let cellws: seq[int] = collect(newSeq):
    for col in cells.itercols(@["".toRunes()]):
      col.mapIt(it.mapIt(it.len).max(0)).max(0)

  let cellhs: seq[int] = collect(newSeq):
    for row in cells.iterrows():
      row.mapIt(it.len).max(0)

  let grid = newTermGrid(start, cellws, cellhs, conf)
  let (vSpacing, hSpacing, totalW, totalH) = gridDimensions(grid)
  let absColPos: seq[int] = grid.cellWidths.cumsumjoin(vSpacing, true)
  let absRowPos: seq[int] = grid.cellHeights.cumsumjoin(hSpacing, true)

  let cellShapes: seq[Shape] = collect(newSeq):
    for (pos, cell) in cells.itercells():
      Shape(newTermText(
        start = (
          start[0] + absColPos[pos[1]] + 1,
          start[1] + absRowPos[pos[0]] + 1
        ), cell))

  newMultishape(@[Shape(grid)] & cellShapes)


func toString*(shape: Shape): string =
  var buf = newBuf()
  shape.render(buf)
  return $buf

converter toRunes*(s: string): RuneSeq = unicode.toRunes(s)
converter toRunes*(s: seq[string]): seq[RuneSeq] = s.mapIt(unicode.toRunes(it))
converter toRunes*(s: seq[seq[string]]): seq[seq[RuneSeq]] =
  s.mapIt(it.mapIt(unicode.toRunes(it)))
