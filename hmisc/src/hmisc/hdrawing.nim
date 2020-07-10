import sequtils, strutils, math, lenientops, helpers, strformat, tables
import halgorithm
import unicode



type
  Shape* = ref object of RootObj
  TermBuf* = object
    buf: seq[seq[Rune]]
    xDiff: int
    yDiff: int

  Point*[Num] = object
    x: Num
    y: Num

  Radian* = float

converter toRune*(c: char): Rune = toRunes($c)[0]

func `[]=`*(buf: var TermBuf, x, y: int, rune: Rune): void =
  let y = y + buf.yDiff
  let x = x + buf.xDiff
  if (y < buf.buf.len) and (x < buf.buf[y].len):
    buf.buf[y][x] = rune

func `[]=`*(buf: var TermBuf, x, y: int, c: char): void =
  buf[x, y] = toRunes($c)[0]

func `[]=`*(buf: var TermBuf, pos: Point[int], c: Rune): void =
  buf[pos.x, pos.y] = c

func makeBuf*(
  xSize, ySize: int, offset: (int, int) = (0, 0)): TermBuf =
  TermBuf(
    buf: newSeqWith(ySize, toRunes(" ".repeat(xSize))),
    xDiff: offset[0], yDiff: offset[1]
  )

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
        lines: seq[string]

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

func makeTermRect*(
  start: (int, int),
  width, height: int, border: char = '+'): SRect[char, int] =
  SRect[char, int](
    upLeft: makePoint(start[0], start[1]),
    config: border,
    width: width,
    height: height
  )

func makeTermText*(start: (int, int), text: seq[string]): SText[int] =
  SText[int](
    start: makePoint(start[0], start[1]),
    lines: text,
    reflow: false,
    width: toSeq(text.mapIt(it.len)).max(0),
    height: text.len
  )

func makeTermVline*(
  start: (int, int), length: int, c: char = '|', isDown: bool = true): auto =
  SLine[char, int](
    angle: if isDown: (PI/2) else: 3 * (PI/2),
    config: c,
    start: start.makePoint(),
    length: length
  )

func makeTermHline*(
  start: (int, int), length: int, c: char = '-', isRight: bool = true): auto =
  SLine[char, int](
    angle: if isRight: PI*0 else: PI,
    config: c,
    start: start.makePoint(),
    length: length)

func makeTermPoint*(start: (int, int), c: char = '+'): SPoint[char, int] =
  SPoint[char, int](point: start.makePoint(), config: c)

func makeBoxedTermText*(start: (int, int), text: seq[string], boxc: char = '#'): Multishape =
  let inner = makeTermText(start.shiftXY(1, 1), text)
  Multishape(shapes: @[
    cast[Shape](inner),
    makeTermRect(
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

func makeTermRect*(
  start: (int, int), width, height: int, conf: TermRectConf): TermRect =
    TermRect(
      upLeft: start.makePoint(),
      height: height,
      width: width,
      config: conf
    )

func makeBoxedTermText*(start: (int, int), text: seq[string], conf: TermRectConf): Multishape =
  let inner = makeTermText(start.shiftXY(1, 1), text)
  Multishape(shapes: @[
    cast[Shape](inner),
    makeTermRect(
      start, width = inner.width + 2, height = inner.height + 2, conf)
  ])
