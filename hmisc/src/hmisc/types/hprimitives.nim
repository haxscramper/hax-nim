import unicode, strutils, sequtils, strformat


#=========================================================================#
#===========================  String helpers  ============================#
#=========================================================================#

#============================  string block  =============================#

type
  StrBlock* = seq[string]

func toBlock*(s: string): StrBlock = s.split('\n')
func height*(s: StrBlock): int = s.len
func width*(s: StrBlock): int =
  if s.len == 0: 0
  else: s.mapIt(it.runeLen).max()


#============================  rune sequence  ============================#

type
  RuneSeq* = seq[Rune]
  RuneBlock* = seq[RuneSeq]

func toRunes*(s: StrBlock): RuneBlock =
  for line in s:
    result.add toRunes(line)

converter toRune*(c: char): Rune = toRunes($c)[0]

const whitespaceRune*: Rune = toRunes(" ")[0]
var emptyRune*: Rune


#=========================================================================#
#==========================  Sequence-related  ===========================#
#=========================================================================#

#================================  Size  =================================#

type
  Size* = object
    width: int
    height: int

const size1x1* = Size(width: 1, height: 1)
func width*(size: Size): int = size.width
func height*(size: Size): int = size.height
func makeSize*(w, h: int): Size = Size(width: w, height: h)


#================================  Range  ================================#

type
  Range* = object
    a*: int
    b*: int

import hashes
func makeRange*(a, b: int): Range = Range(a: a, b: b)
func hash*(r: Range): Hash = hash(r.a) !& hash(r.b)
func decRight*(r: Range, diff: int = 1): Range =
  ## Shift right side of range by one.
  assert r.b - diff >= r.a,
     &"Cannot shift right range side past left side: [{r.a}, {r.b}]" &
       &" -> [{r.a}, {r.b - diff}]"
  makeRange(r.a, r.b - diff)

func contains*(r: Range, item: int): bool =
  r.a <= item and item <= r.b

func middles*(r: Range): int =
  ## Number of gaps in between range points
  (r.b - r.a - 1)

func isPoint*(r: Range): bool =
  ## If range starts is equal to end
  (r.a == r.b)

func point*(r: Range): int =
  assert r.isPoint()
  r.a

func len*(r: Range): int = r.b - r.a + 1

iterator items*(r: Range): int =
  for it in r.a .. r.b:
    yield it

iterator items*(r: (Range, Range)): (int, int) =
  for x in r[0].a .. r[0].b:
    for y in r[1].a .. r[1].b:
      yield (x, y)

#==============================  Position  ===============================#
# TODO is it possible to define custom unpacker for object? to write
#      `let (row, col) = Pos()`

type
  Pos* = object
    row*: int
    col*: int

func makePos*(row, col: int): Pos = Pos(row: row, col: col)
func makePos*(pos: (int, int)): Pos = Pos(row: pos[0], col: pos[1])

#==============================  compound  ===============================#

func rowRange*(pos: Pos, size: Size): Range =
  makeRange(pos.row, size.height)

func colRange*(pos: Pos, size: Size): Range =
  makeRange(pos.col, pos.col + size.width)



#*************************************************************************#
#****************************  Tree-related  *****************************#
#*************************************************************************#

type
  TreePath* = seq[int]
