import unicode

#=========================================================================#
#=======================  Geometrical primitives  ========================#
#=========================================================================#

#================================  Size  =================================#

type
  Size* = object
    width: int
    height: int


func width*(size: Size): int = size.width
func height*(size: Size): int = size.height
func makeSize*(w, h: int): Size = Size(width: w, height: h)

#================================  Point  ================================#

type
  Point*[Num] = object
    x: Num
    y: Num

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

#===============================  radian  ================================#

type
  Radian* = distinct float

const whitespaceRune*: Rune = toRunes(" ")[0]
var emptyRune*: Rune

#=========================================================================#
#===========================  String helpers  ============================#
#=========================================================================#

#============================  string block  =============================#

type
  StrSeq* = seq[string]

#============================  rune sequence  ============================#

type
  RuneSeq* = seq[Rune]
  RuneBlock* = seq[RuneSeq]

converter toRune*(c: char): Rune = toRunes($c)[0]

#=========================================================================#
#==========================  Sequence-related  ===========================#
#=========================================================================#

#================================  Range  ================================#

type
  Range* = object
    a*: int
    b*: int

import hashes
func hash*(r: Range): Hash = hash(r.a) !& hash(r.b)

#==============================  Position  ===============================#
# TODO is it possible to define custom unpacker for object? to write
#      `let (row, col) = Pos()`

type
  Pos* = object
    row*: int
    col*: int

func middles*(r: Range): int =
  ## Number of gaps in between range points
  (r.b - r.a - 1)

func isPoint*(r: Range): bool =
  ## If range starts is equal to end
  (r.a == r.b)

func point*(r: Range): int =
  assert r.isPoint()
  r.a
