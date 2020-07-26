import sets, sequtils, hashes, tables, strutils, strformat, macros
import hmisc/helpers
import initcalls

## Parse tree contains actual token /values/ - concrete lexemes and
## additional information (whatever you deem necessary adding).
## `Token` represents lexical unit found in *input data*.
## `ExpectedToken` describes *required* token category and OPTIONAL
## lexeme value.

#*************************************************************************#
#********************************  Token  ********************************#
#*************************************************************************#

#===========================  Type definition  ===========================#

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
    case hasLex*: bool ## Whether or not lexeme value should be considered
      of true:
        lex*: L ## Expected lexeme value
      of false:
        nil

#============================  Constructors  =============================#

func makeExpToken*[C, L](category: C, lexeme: L): ExpectedToken[C, L] =
  ExpectedToken[C, L](cat: category, lex: lexeme, hasLex: true)

func makeExpToken*[C, L](category: C): ExpectedToken[C, L] =
  ExpectedToken[C, L](cat: category, hasLex: false)

func matches*[C, L, I](exp: ExpectedToken[C, L], tok: Token[C, L, I]): bool =
  ## Return true if token `tok` matches with expected token `exp`
  # TODO IMPLEMENT
  if tok.cat != exp.cat:
    false
  else:
    if exp.hasLex:
      exp.lex == tok.lex
    else:
      true


func makeToken*[C, L, I](cat: C, lex: L): Token[C, L, I] =
  Token[C, L, void](cat: cat, lex: lex)

func makeTokenNoInfo*[C, L](cat: C, lex: L): Token[C, L, void] =
  Token[C, L, void](cat: cat, lex: lex)

#*************************************************************************#
#******************************  Token set  ******************************#
#*************************************************************************#

#===========================  Type definition  ===========================#

type
  EofTok* = object

  LexSet*[L] = object
    ## Set of lexemes
    hasAll: bool ## Whether or not all lexeme values are in set
    lexemes: HashSet[L]

  TokSet*[C, L] = object
    ## Set of expected tokens + EOF token (end of inptu sequence)
    tokens: Table[C, LexSet[L]] ## Map from
    ## token category to list of expected lexemes + whether or not
    ## token can be accepted if lexeme does not match.
    # `kwd."if"` matches _if_ `kwd -> (lex: {"if"})` - token with
    # category `kwd` is paired with lexeme "if" _or_ `kwd ->
    # (hasEmpty: true)` - token of kind `kwd` is accepted without
    # checking lexeme.

    hasEof: bool

#=============================  Lexeme set  ==============================#

func incl*[L](s: var LexSet[L], lex: L): void = s.lexemes.incl(lex)

func incl*[L](s: var LexSet[L], other: LexSet[L]): void =
  s.hasAll = s.hasAll or other.hasAll
  s.lexemes.incl(other.lexemes)

func contains*[L](lset: LexSet[L], lex: L): bool =
  lex in lset.lexemes

func makeLexSet*[L](): LexSet[L] =
  LexSet[L](lexemes: initHashSet[L](2))

func initLexSet*[L](hasAll: bool, lexemes: HashSet[L]): LexSet[L] =
  LexSet[L](hasAll: hasAll, lexemes: lexemes)

func getHasAll*[L](lset: LexSet[L]): bool = lset.hasAll
func getLexemes*[L](lset: LexSet[L]): HashSet[L] = lset.lexemes

iterator items*[L](lset: LexSet[L]): L =
  for lex in lset.lexemes:
    yield lex

#==============================  Token set  ==============================#

func initTokSet*[C, L](
  tokens: Table[C, LexSet[L]], hasEof: bool): TokSet[C, L] =
  TokSet[C, L](tokens: tokens, hasEof: hasEof)

func getTokens*[C, L](tset: TokSet[C, L]): Table[C, LexSet[L]] =
  tset.tokens

func getHasEof*[C, L](tset: TokSet[C, L]): bool =
  tset.hasEof

const eofTok*: EofTok = EofTok()

func contains*[C, L, I](s: TokSet[C, L], tk: Token[C, L, I]): bool =
  if tk.cat in s.tokens:
    (s.tokens[tk.cat].hasAll) or (tk.lex in s.tokens[tk.cat])
  else:
    false

func contains*[C, L](s: TokSet[C, L], tk: EofTok): bool =
  s.hasEof

func incl*[C, L, I](s: var TokSet[C, L], tk: Token[C, L, I]): void =
  s.vals.incl tk

func incl*[C, L](s: var TokSet[C, L], etk: ExpectedToken[C, L]): void =
  if etk.cat notin s.tokens:
    s.tokens[etk.cat] = makeLexSet[L]()

  if etk.hasLex:
    s.tokens[etk.cat].incl etk.lex
  else:
    s.tokens[etk.cat].hasAll = true

func incl*[C, L](s: var TokSet[C, L], tk: EofTok): void =
  (s.hasEof = true)

func incl*[C, L](
  s: var TokSet[C, L], other: TokSet[C, L]): void =
  for cat, lex in other.tokens:
    if cat notin s.tokens:
      s.tokens[cat] = lex
    else:
      s.tokens[cat].incl lex

func exprRepr*[L](lset: LexSet[L]): string =
  (
    lset.lexemes.mapIt($it) &
      lset.hasAll.tern(@["_"], @[])
  ).join(", ").wrap("{}")

func exprRepr*[C, L](s: TokSet[C, L]): string =
  s.tokens.mapPairs(fmt("{lhs} -> {rhs.exprRepr()}")
  ).join(", ").wrap("{}")

iterator pairs*[C, L](s: TokSet[C, L]): (C, LexSet[L]) =
  for cat, lset in s.tokens:
    yield (cat, lset)

func makeTokSet*[C, L](): TokSet[C, L] =
  TokSet[C, L](tokens: initTable[C, LexSet[L]](2))

func toTkind*[C, L](s: set[C]): TokSet[C, L] =
  result = makeTokSet[C, L]()
  (result.vals = s)

func makeTokSet*[C, L, I](tok: Token[C, L, I]): TokSet[C, L] =
  result = makeTokSet[C, L]()
  result.vals.incl tok

func makeTokSet*[C, L](eof: EofTok): TokSet[C, L] =
  result = makeTokSet[C, L]()
  (result.hasEof = true)

iterator items*[C, L](s: TokSet[C, L]): ExpectedToken[C, L] =
  for it in s.vals:
    yield it

func union*[C, L](s: seq[TokSet[C, L]]): TokSet[C, L] =
  result.hasEof = s.anyOfIt(it.hasEof)
  for it in s:
    result.vals.incl it.vals

func containsOrIncl*[C, L](
  s: var TokSet[C, L], other: TokSet[C, L]): bool =
  if (s.hasEof == other.hasEof) and ((s.vals - other.vals).len == 0):
    result = false

  s.hasEof = s.hasEof or other.hasEof
  s.vals.incl other.vals

func hash*[C, L](s: TokSet[C, L]): Hash =
  var h: Hash = 0
  h = h !& hash(s.vals) !& hash(s.hasEof)
  result = !$h


#*************************************************************************#
#****************************  Token lookup  *****************************#
#*************************************************************************#
#===========================  Type definition  ===========================#

type
  LexLookup*[L] = object
    table: Table[L, seq[int]]
    hasAll: seq[int]

  TokLookup*[C, L] = object
    table: Table[C, LexLookup[L]]

#=============================  Contructors  =============================#

func `[]`*[C, L](tl: var TokLookup[C, L], cat: C): var LexLookup[L] =
  tl.table[cat]

func `[]`*[L](tl: var LexLookup, lex: L): var seq[int] =
  tl.table[lex]

func `[]`*[C, L](tl: TokLookup[C, L], cat: C): LexLookup[L] =
  tl.table[cat]

func `[]`*[L](tl: LexLookup, lex: L): seq[int] =
  tl.table[lex]

func contains*[C, L](tl: TokLookup[C, L], cat: C): bool = cat in tl.table
func contains*[L](ll: LexLookup[L], lex: L): bool = lex in ll.table

#===================  Predicates/accessors/iterators  ====================#

func initTokLookup*[C, L](table: Table[C, LexLookup[L]]): TokLookup[C, L] =
  TokLookup[C, L](table: table)

func initLexLookup*[L](
  table: Table[L, seq[int]], hasAll: seq[int]): LexLookup[L] =
  LexLookup[L](table: table, hasAll: hasAll)

func makeInitCalls*[C, L](lookup: TokLookup[C, L]): NimNode =
  mixin makeInitCalls
  result = newCall(
    "initTokLookup",
    nnkExprEqExpr.newTree(ident "table", lookup.table.makeInitCalls()))

func makeInitCalls*[L](lookup: LexLookup[L]): NimNode =
  mixin makeInitCalls
  result = newCall(
    "initLexLookup",
    nnkExprEqExpr.newTree(ident "table", lookup.table.makeInitCalls()),
    nnkExprEqExpr.newTree(ident "hasAll", lookup.hasAll.makeInitCalls())
  )

func makeTokLookup*[C, L](): TokLookup[C, L] =
  TokLookup[C, L](table: initTable[C, LexLookup[L]](2))

func makeLexLookup*[L](): LexLookup[L] =
  LexLookup[L](table: initTable[L, seq[int]](2))

func makeTokLookup*[C, L](
  altSets: seq[TokSet[C, L]], canConflict: bool = false): TokLookup[C, L] =
  ## Create token lookup from sequence of alternatives
  # TODO detect ambiguity
  # TODO IMPLEMENT
  result = makeTokLookup[C, L]()
  for idx, alt in altSets:
    for cat, lset in pairs(alt):
      if cat notin result.table:
        result.table[cat] = makeLexLookup[L]()

      if lset.hasAll:
        if canConflict and result[cat].hasAll.len > 0:
          raiseAssert("Conflict") # TODO better error msg
        else:
          result[cat].hasAll.add idx

      for lex in items(lset):
        if lex notin result.table[cat].table:
          result[cat].table[lex] = @[]

        if canConflict and result[cat][lex].len > 0:
          raiseAssert("Conflict") # TODO better error msg
        else:
          result[cat][lex].add idx




#==============================  Accessors  ==============================#

func getAlt*[C, L, I](
  lookup: TokLookup[C, L], token: Token[C, L, I]): int =
  ## Get select alternative set based on token category and lexeme values.
  # TODO raise exception if token is not found
  # TODO IMPLEMENT
  if token.cat in lookup:
    if token.lex in lookup[token.cat]:
      let alts = lookup[token.cat][token.lex]
      if alts.len > 1:
        raiseAssert("#[ IMPLEMENT more than one alternative ]#")
      else:
        return alts[0]
    elif lookup[token.cat].hasAll.len > 0:
      let alts = lookup[token.cat].hasAll
      if alts.len > 1:
        raiseAssert("#[ IMPLEMENT more than one alternative ]#")
      else:
        return alts[0]
    else:
      raiseAssert("#[ IMPLEMENT token lexeme not found ]#")
  else:
    raiseAssert("#[ IMPLEMENT token category not found ]#")

#*************************************************************************#
#*********************  Unexpected token exceptions  *********************#
#*************************************************************************#

type
  LinePosInfo* = object
    line*: int
    column*: int
    filename*: string

  # ParserErrorKind* = enum

  #   pekUnexpectedToken

  # ParserError = ref object of CatchableError
  #   linepos*: Option[LinePosInfo]
  #   case kind*: ParserErrorKind
  #     of pekUnexpectedToken:
  #       expected*: string
  #       intoken*: string
  #       inputpos*: int

func exprRepr*[C, L, I](tok: Token[C, L, I]): string =
  # TODO remove prefix from category enum
  fmt("({tok.cat} '{tok.lex}')")

func exprRepr*[C, L](exp: ExpectedToken[C, L]): string =
  if exp.hasLex:
    fmt("({exp.cat} '{exp.lex}')")
  else:
    fmt("({exp.cat} _)")

func getLinePos*[C, L, I](tok: Token[C, L, I]): LinePosInfo =
  getLinePos(tok.info)

func hasLinePosInfo*[C, L, I](tok: Token[C, L, I]): bool =
  const res = compiles(getLinePos(tok.info)) and
    (getLinePos(tok.info) is LinePosInfo)

template assertToken*[C, L, I](
  exp: ExpectedToken[C, L], tok: Token[C, L, I]): untyped =
  if not matches(exp, tok):
    raiseAssert(
      "Unexpected token _" & exprRepr(tok) & "_, expected _" &
        exprRepr(exp) & "_")
