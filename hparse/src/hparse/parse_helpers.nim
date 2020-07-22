#==============================  token set  ==============================#

type
  EofTok* = object
  TkindSet*[Tk] = object
    ## Set of tokens + EOF token (end of inptu sequence)
    vals: set[Tk]
    hasEof: bool

const eofTok*: EofTok = EofTok()

func contains*[Tk](s: TKindSet[Tk], tk: Tk): bool = tk in s.vals
func contains*[Tk](s: TKindSet[Tk], tk: EofTok): bool = s.hasEof
func incl*[Tk](s: var TKindSet[Tk], tk: Tk): void = s.vals.incl tk
func incl*[Tk](s: var TKindSet[Tk], tk: EofTok): void = (s.hasEof = true)
func incl*[Tk](s: var TKindSet[Tk], other: TKindSet[Tk]): void =
  s.vals.incl other.vals

func `$`*[Tk](s: TKindSet[Tk]): string =
  (s.vals.mapIt($it) & s.hasEof.tern(@[ "$" ], @[])).join(", ").wrap("{}")

func toTkind*[Tk](s: set[Tk]): TKindSet[Tk] = (result.vals = s)
func makeTKindSet*[Tk](): TkindSet[Tk] = discard
func makeTKindSet*[Tk](tok: Tk): TkindSet[Tk] = result.vals.incl tok
func makeTKindSet*[Tk](eof: EofTok): TKindSet[Tk] = (result.hasEof = true)
iterator items*[Tk](s: TKindSet[Tk]): Tk =
  for it in s.vals:
    yield it

func union*[Tk](s: seq[TKindSet[Tk]]): TKindSet[Tk] =
  result.hasEof = s.anyOfIt(it.hasEof)
  for it in s:
    result.vals.incl it.vals

func containsOrIncl*[Tk](s: var TKindSet[Tk], other: TKindSet[Tk]): bool =
  if (s.hasEof == other.hasEof) and ((s.vals - other.vals).len == 0):
    result = false

  s.hasEof = s.hasEof or other.hasEof
  s.vals.incl other.vals

func hash*[Tk](s: TKindSet[Tk]): Hash =
  var h: Hash = 0
  h = h !& hash(s.vals) !& hash(s.hasEof)
  result = !$h
