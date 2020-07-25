import sets, sequtils, hashes
import hmisc/helpers

## Parse tree contains actual token /values/ - concrete lexemes and
## additional information (whatever you deem necessary adding).
## `Token` represents lexical unit found in *input data*.
## `ExpectedToken` describes required token category and OPTIONAL
## lexeme value.

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
  TkindSet*[C, L] = object
    ## Set of expected tokens + EOF token (end of inptu sequence)
    categories: HashSet[C]
    lexemes: Hashset[L]
    hasEof: bool

const eofTok*: EofTok = EofTok()

func contains*[C, L, I](s: TKindSet[C, L], tk: Token[C, L, I]): bool =
  tk in s.vals

func contains*[C, L](s: TKindSet[C, L], tk: EofTok): bool =
  s.hasEof

func incl*[C, L, I](s: var TKindSet[C, L], tk: Token[C, L, I]): void =
  s.vals.incl tk

func incl*[C, L](s: var TKindSet[C, L], tk: EofTok): void =
  (s.hasEof = true)

func incl*[C, L](
  s: var TKindSet[C, L], other: TKindSet[C, L]): void =
  s.vals.incl other.vals

func `$`*[C, L](s: TKindSet[C, L]): string =
  (s.vals.mapIt($it) & s.hasEof.tern(@[ "$" ], @[])).join(", ").wrap("{}")

func toTkind*[C, L](s: set[C]): TKindSet[C, L] =
  (result.vals = s)

func makeTKindSet*[C, L](): TkindSet[C, L] =
  discard

func makeTKindSet*[C, L, I](tok: Token[C, L, I]): TkindSet[C, L] =
  result.vals.incl tok

func makeTKindSet*[C, L](eof: EofTok): TKindSet[C, L] =
  (result.hasEof = true)

iterator items*[C, L](s: TKindSet[C, L]): ExpectedToken[C, L] =
  for it in s.vals:
    yield it

func union*[C, L](s: seq[TKindSet[C, L]]): TKindSet[C, L] =
  result.hasEof = s.anyOfIt(it.hasEof)
  for it in s:
    result.vals.incl it.vals

func containsOrIncl*[C, L](
  s: var TKindSet[C, L], other: TKindSet[C, L]): bool =
  if (s.hasEof == other.hasEof) and ((s.vals - other.vals).len == 0):
    result = false

  s.hasEof = s.hasEof or other.hasEof
  s.vals.incl other.vals

func hash*[C, L](s: TKindSet[C, L]): Hash =
  var h: Hash = 0
  h = h !& hash(s.vals) !& hash(s.hasEof)
  result = !$h
