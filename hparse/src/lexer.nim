# To support lexing in both compiled and interpreted environments as
# well as at compile-time
import regex
import streams

type
  Matcher[Tok] = object
    ## Token matcher. `Tok` is a type of returned Token.
    case cbMatch: bool
      of true:
        matchImpl: proc(inStream: Stream): tuple[
          ok: bool, tok: Tok, pos: int] ## Callback proc to match
        ## token. Starting position and other parts of lexer state are
        ## stored in the `inStream`. On failure proc must return
        ## starting position in `pos` and `ok = false`.
      of false:
        patt: Regex
        classify: proc(match: string, ruleId: int): Tok ## Classify
        ## matched token and convert it to string.


  Lexer[Tok] = object
    ## Data stream lexer. `Tok` is a type of returned tokens.
    stream*: Stream ## Input data stream
    # Lexer state is implicitly stored in the stream (position and
    # buffer)
    matchers*: seq[Matcher[Tok]]


  TokStream*[Tok] = object
    ## Buffer for tokens. Modeled after `std/streams` implementation
    curPos: int ## Current position in buffer
    buffer: seq[Tok] ## Token buffer
    newTok: proc(): tuple[stop: bool, tok: Tok] ## Callback to get new
    ## tokens. To indicate final token return `stop = true`
    atEnd: bool

proc next*[Tok](ts: var TokStream[Tok]): Tok =
  ## Create single token by either parsing new data or returning from
  ## buffer
  if ts.curPos < ts.buffer.len - 1:
    inc ts.curPos
    return ts.buffer[ts.curPos]
  else:
    if ts.atEnd:
      raiseAssert("Cannot read from finished token stream. " &
      & "Current position: {ts.curPos}, buffer size: {ts.buffer.len}")

    else:
      # Assuming _if_ token stream not atEnd _then_ it can read at
      # least one more token.
      let (stop, tok) = ts.newTok()
      if stop:
        ts.atEnd = true

      ts.buffer.add tok
      inc ts.curPos

func makeStream*[Tok](tokens: seq[Tok]): TokStream[Tok] =
  TokStream[Tok](
    buffer: tokens,
    newTok: proc(): auto = (stop: true, tok: Tok()),
    atEnd: true,
    curPos: -1
  )

func finished*[Tok](toks: TokStream[Tok]): bool =
  toks.atEnd and toks.curPos == toks.buffer.len - 1

proc move*[Tok](ts: var TokStream[Tok], shift: int = -1): void =
  ts.curPos = ts.curPos + shift

proc peek*[Tok](ts: var TokStream[Tok]): Tok =
  ## Get next token from token stream without changing position
  let next = ts.next()
  ts.move(-1)
  return next


iterator items*[Tok](ts: var TokStream[Tok]): Tok =
  ## Iterate over tokems in tokens stream. New parsing is done only
  ## when buffer is reached.
  while not ts.finished():
    yield ts.next()

proc reset*[Tok](ts: var TokStream[Tok]): Tok =
  ## Reset token stream internal state - clear buffer, set position to
  ## 0 etc.
  ts.buffer = @[]
  ts.atEnd = false
  ts.curPos = 0
