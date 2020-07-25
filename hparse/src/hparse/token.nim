import sets, sequtils, hashes
import hmisc/helpers

type
  Token*[Category, Lexeme, Info] = object
    ## Actual value of input token
    cat*: Category ## Token category. It is REQUIRED to be correct as
                   ## it is always used in parsing
    lex*: Lexeme ## Lexeme information. It OPTIONAL and might be used
                 ## in parsing.
    info*: Info ## Additional information in token. It is OPTIONAL and
                ## never used in parsing.

  ExpectedToken*[C, L] = object
    ## Description of token to expect during parsing. In order for
    ## input token to match category MUST be identical (equality
    ## comparison). In case `hasLex` is true lexeme values also MUST
    ## match.
    # NOTE token comparison is done using simple equality - more
    # complex relations are not supported - they should be encoded in
    cat*: C ## Expected token category
    case hasLex: bool ## Whether or not lexeme value should be considered
      of true:
        lex*: L ## Expected lexeme value
      of false:
        nil


#==============================  token set  ==============================#

type
  EofTok* = object
  TkindSet*[C, L, I] = object
    ## Set of tokens + EOF token (end of inptu sequence)
    categories: HashSet[C]
    lexemes: Hashset[L]
    hasEof: bool

const eofTok*: EofTok = EofTok()

func contains*[C, L, I](s: TKindSet[C, L, I], tk: Token[C, L, I]): bool =
  tk in s.vals

func contains*[C, L, I](s: TKindSet[C, L, I], tk: EofTok): bool =
  s.hasEof

func incl*[C, L, I](s: var TKindSet[C, L, I], tk: Token[C, L, I]): void =
  s.vals.incl tk

func incl*[C, L, I](s: var TKindSet[C, L, I], tk: EofTok): void =
  (s.hasEof = true)

func incl*[C, L, I](
  s: var TKindSet[C, L, I], other: TKindSet[C, L, I]): void =
  s.vals.incl other.vals

func `$`*[C, L, I](s: TKindSet[C, L, I]): string =
  (s.vals.mapIt($it) & s.hasEof.tern(@[ "$" ], @[])).join(", ").wrap("{}")

func toTkind*[C, L, I](s: set[C]): TKindSet[C, L, I] =
  (result.vals = s)

func makeTKindSet*[C, L, I](): TkindSet[C, L, I] =
  discard

func makeTKindSet*[C, L, I](tok: Token[C, L, I]): TkindSet[C, L, I] =
  result.vals.incl tok

func makeTKindSet*[C, L, I](eof: EofTok): TKindSet[C, L, I] =
  (result.hasEof = true)

iterator items*[C, L, I](s: TKindSet[C, L, I]): Token[C, L, I] =
  for it in s.vals:
    yield it

func union*[C, L, I](s: seq[TKindSet[C, L, I]]): TKindSet[C, L, I] =
  result.hasEof = s.anyOfIt(it.hasEof)
  for it in s:
    result.vals.incl it.vals

func containsOrIncl*[C, L, I](
  s: var TKindSet[C, L, I], other: TKindSet[C, L, I]): bool =
  if (s.hasEof == other.hasEof) and ((s.vals - other.vals).len == 0):
    result = false

  s.hasEof = s.hasEof or other.hasEof
  s.vals.incl other.vals

func hash*[C, L, I](s: TKindSet[C, L, I]): Hash =
  var h: Hash = 0
  h = h !& hash(s.vals) !& hash(s.hasEof)
  result = !$h
