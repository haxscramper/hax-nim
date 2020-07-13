import unicode, strutils, sequtils, strformat


#=========================================================================#
#===========================  String helpers  ============================#
#=========================================================================#

#============================  string block  =============================#

type
  StrBlock* = seq[string]

func toBlock*(s: string): StrBlock = s.split('\n')
func toBlock*(s: seq[string]): StrBlock = StrBlock(s)
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
  ArrSize* = object
    width: int
    height: int

const size1x1* = ArrSize(width: 1, height: 1)
func width*(size: ArrSize): int = size.width
func height*(size: ArrSize): int = size.height
func makeArrSize*(w, h: int): ArrSize = ArrSize(width: w, height: h)
func makeArrSize*(widthHeight: (int, int)): ArrSize =
  ArrSize(width: widthHeight[0], height: widthHeight[1])


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


func incLeft*(r: Range, diff: int = 1): Range =
  ## Shift left side of range by one.
  assert r.a + diff <= r.b,
     &"Cannot shift right range side past left side: [{r.a}, {r.b}]" &
       &" -> [{r.a + diff}, {r.b}]"
  makeRange(r.a + diff, r.b)

func contains*(r: Range, item: int): bool =
  r.a <= item and item <= r.b

func middles*(r: Range): int =
  ## Number of gaps in between range points
  (r.b - r.a - 1)

func isPoint*(r: Range): bool =
  ## If range starts is equal to end
  (r.a == r.b)

func unpack*(r: Range): (int, int) = (r.a, r.b)

func point*(r: Range): int =
  assert r.isPoint()
  r.a

func isValid*(r: Range): bool = r.b >= r.a
func overlap*(r1, r2: Range): Range =
  result = makeRange(max(r1.a, r2.a), min(r1.b, r2.b))
func `$`*(r: Range): string = &"[{r.a}, {r.b}]"
func len*(r: Range): int = r.b - r.a + 1

iterator items*(r: Range): int =
  for it in r.a .. r.b:
    yield it

iterator items*(r: (Range, Range)): (int, int) =
  for x in r[0].a .. r[0].b:
    for y in r[1].a .. r[1].b:
      yield (x, y)

iterator `[]`*[T](s: seq[T], r: Range): T =
  for it in s[r.a .. r.b]:
    yield it

iterator inrange*(s: seq[int], r: Range, lDiff, rDiff: int = 0): int =
  ## Iterate over all values between `s[r.a]` to `s[r.b]`. Shift
  ## left/right edge of the range by `l/rDiff` respectively.
  for v in (s[r.a] + lDiff) .. (s[r.b] + rDiff):
    yield v

#==============================  Position  ===============================#
# TODO is it possible to define custom unpacker for object? to write
#      `let (row, col) = Pos()`

type
  ArrPos* = object
    row*: int
    col*: int

func makeArrPos*(row, col: int): ArrPos = ArrPos(row: row, col: col)
func makeArrPos*(pos: (int, int)): ArrPos = ArrPos(row: pos[0], col: pos[1])
func isValid*(pos: ArrPos): bool = (pos.row >= 0) and (pos.col >= 0)
func expandSize*(pos: ArrPos, size: ArrSize): ArrSize =
  makeArrSize(
    h = pos.row + size.height,
    w = pos.col + size.width
  )


func shiftRC*(pos: ArrPos, dRow: int = 1, dCol: int = 1): ArrPos =
  makeArrPos(pos.row + dRow, pos.col + dCol)

func shiftRc*(pos: ArrPos, dRC: (int, int) = (1, 1)): ArrPos =
  makeArrPos(pos.row + dRC[0], pos.col + dRC[1])

func shiftC*(pos: ArrPos, dCol: int = 1): ArrPos = shiftRC(pos, 0, dCol)
func shiftR*(pos: ArrPos, dRow: int = 1): ArrPos = shiftRC(pos, dRow, 0)
converter toArrPos*(pos: (int, int)): ArrPos = makeArrPos(pos)
func unpack*(pos: ArrPos): tuple[row, col: int] = (pos.row, pos.col)
func `==`(a, b: ArrPos): bool = (a.row == b.row) and (a.col == b.col)

type
  RelPos* = enum
    rpLeft
    rpRight
    rpBottom
    rpTop

func toDiffRC*(rp: RelPos): (int, int) =
  case rp:
    of rpLeft: (0, -1)
    of rpRight: (0, 1)
    of rpBottom: (1, 0)
    of rpTop: (-1, 0)

#==============================  compound  ===============================#

func rowRange*(pos: ArrPos, size: ArrSize): Range =
  ## Get *indices* of rows that multicell of `size` at `pos` would
  ## occupy
  makeRange(pos.row, pos.row + size.height - 1)

func colRange*(pos: ArrPos, size: ArrSize): Range =
  ## Get *indices* of columns that multicell of `size` at `pos` would
  ## occupy
  makeRange(pos.col, pos.col + size.width - 1)



#*************************************************************************#
#****************************  Tree-related  *****************************#
#*************************************************************************#

type
  TreePath* = seq[int]
