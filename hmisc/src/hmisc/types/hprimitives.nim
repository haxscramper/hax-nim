import unicode, strutils, sequtils


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
  else: s.mapIt(it.len).max()

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

#================================  Range  ================================#

type
  Range* = object
    a*: int
    b*: int

import hashes
func makeRange*(a, b: int): Range = Range(a: a, b: b)
func hash*(r: Range): Hash = hash(r.a) !& hash(r.b)
func contains*(r: Range, item: int): bool =
  r.a <= item and item <= r.b

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

func middles*(r: Range): int =
  ## Number of gaps in between range points
  (r.b - r.a - 1)

func isPoint*(r: Range): bool =
  ## If range starts is equal to end
  (r.a == r.b)

func point*(r: Range): int =
  assert r.isPoint()
  r.a


#*************************************************************************#
#****************************  Tree-related  *****************************#
#*************************************************************************#

type
  TreePath* = seq[int]
