# To support lexing in both compiled and interpreted environments as
# well as at compile-time
when nimvm or defined(nimscript):
  import regex
else:
  import re

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


  TokStream[Tok] = object
    ## Buffer for tokens. Modeled after `std/streams` implementation
    curPos: int ## Current position in buffer
    buffer: seq[Tok] ## Token buffer
    newTok: proc(): tuple[stop: bool, tok: Tok] ## Callback to get new
    ## tokens. To indicate final token return `stop = true`
    atEnd: bool

proc next[Tok](ts: var TokStream): Tok =
  ## Create single token by either parsing new data or returning from
  ## buffer
  if ts.curPos < ts.buffer.len - 1:
    inc ts.curPos
    return ts.buffer[ts.curPos]
  else:
    if ts.atEnd:
      raise newException(AssertionError,
        "Cannot read from finished token stream")

    else:
      # Assuming _if_ token stream not atEnd _then_ it can read at
      # least one more token.
      let (stop, tok) = ts.newTok()
      if stop:
        ts.atEnd = true

      buffer.add tok
      inc ts.curPos

iterator items[Tok](ts: var TokStream[Tok]): Tok =
  ## Iterate over tokems in tokens stream. New parsing is done only
  ## when buffer is reached.
  while not ts.atEnd:
    yield ts.next()

proc reset[Tok](ts: var TokStream[Tok]): Tok =
  ## Reset token stream internal state - clear buffer, set position to
  ## 0 etc.
  ts.buffer = @[]
  ts.atEnd = false
  ts.curPos = 0
