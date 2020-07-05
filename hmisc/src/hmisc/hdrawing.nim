import sequtils, strutils, math, lenientops, helpers, strformat, tables

type
  Shape* = ref object of RootObj

  TermBuf* = object
    buf: seq[string]
    xDiff: int
    yDiff: int

func `[]=`*(buf: var TermBuf, x, y: int, c: char): void =
  # d &"Seting {c} to [{x}, {y}]"
  buf.buf[y + buf.yDiff][x + buf.xDiff] = c


func makeBuf*(rows, cols: int, offset: (int, int) = (0, 0)): TermBuf =
  TermBuf(
    buf: newSeqWith(rows, " ".repeat(cols)),
    xDiff: offset[0], yDiff: offset[1]
  )

func `$`*(buf: TermBuf): string = buf.buf.join("\n")


method render*(s: Shape, buf: var TermBuf): void {.base.} =
  raiseAssert("Cannot draw base shape implementation")

type
  Point*[Num] = object
    x: Num
    y: Num

  Radian* = float

  SPoint*[T, Num] = ref object of Shape
    point: Point[Num]
    config: T

  SLine*[T, Num] = ref object of Shape
    start: Point[Num]
    length: Num
    angle: Radian
    config: T

type
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

  TermRectConf* = Table[RectPoint, char]
  TermRect* = SRect[TermRectConf, int]

func makePoint*[Num](x, y: Num): auto = Point[Num](x: x, y: y)
func shiftX*[Num](p: Point[Num], dx: Num): Point[Num] =
  makePoint[Num](p.x + dx, p.y)

func shiftY*[Num](p: Point[Num], dy: Num): Point[Num] =
  makePoint[Num](p.x, p.y + dy)


func x*[T, Num](p: SPoint[T, Num]): Num = p.point.x
func y*[T, Num](p: SPoint[T, Num]): Num = p.point.y


iterator `..+`(start: int, offset: int): int =
  for i in start ..< start + offset:
    yield i

func order[Num](a, b: Num): (Num, Num) =
  if a > b: (b, a)
  else: (a, b)

func renderLine(x0, y0, x1, y1: int, buf: var TermBuf, c: char): void =
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
    buf[x, y] = c
    x += xInc.int
    y += yInc.int

func renderLine*(p: Point[int], w, h: int, buf: var TermBuf, c: char): void =
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
    c = line.config
  )



method render*(rect: SRect[char, int], buf: var TermBuf): void =
  renderLine(
    rect.upLeft, rect.width, 0, buf, rect.config)
  renderLine(
    rect.upLeft, 0, rect.height, buf, rect.config)

  renderLine(
    rect.upLeft.shiftY(rect.height), rect.width, 0, buf, rect.config)
  renderLine(
    rect.upLeft.shiftX(rect.width), 0, rect.height, buf, rect.config)


method render*(rect: SRect[Table[RectPoint, char], int], buf: var TermBuf): void =
  echo "rendering rect"
  # renderLine(
  #   rect.upLeft, rect.width, 0, buf, rect.config)
  # renderLine(
  #   rect.upLeft, 0, rect.height, buf, rect.config)

  # renderLine(
  #   rect.upLeft.shiftY(rect.height), rect.width, 0, buf, rect.config)
  # renderLine(
  #   rect.upLeft.shiftX(rect.width), 0, rect.height, buf, rect.config)

func makeTermRect*(
  start: (int, int),
  width, height: int, border: char = '+'): SRect[char, int] =
  SRect[char, int](
    upLeft: makePoint(start[0], start[1]),
    config: border,
    width: width,
    height: height
  )

func makeTermVline*(
  x, y: int, length: int, c: char = '|', isDown: bool = true): auto =
  SLine[char, int](
    angle: if isDown: 3 * (PI/2) else: PI/2,
    config: c,
    start: makePoint(x, y),
    length: length
  )

func makeUnicodeTermRect(): TermRect =
  discard
