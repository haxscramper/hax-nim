import unicode


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
