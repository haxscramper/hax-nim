## Parser combinator librariy

import options
import regex

type
  ParseResult*[Val] = object
    case success*: bool
      of true:
        value*: Val
        endpos*: int
      of false:
        error*: string

  Callbacks* = object
    entry*: proc(pos: int)
    success*: proc(pos: int)
    failure*: proc(pos: int)

const noCallbacks* = Callbacks(
  entry: proc(p: int) = discard,
  success: proc(p: int) = discard,
  failure: proc(p: int) = discard
)

type
  Parser*[Val, Buf] = proc(buffer: Buf, position: int = 0): ParseResult[Val]

proc parseString*(str: string, cb: Callbacks = noCallbacks): Parser[string, string] =
  return proc(buffer: string, position: int = 0): ParseResult[string] =
    if position + str.len > buffer.len:
      ParseResult[string](
        success: false,
        error: "End position reached"
      )
    elif buffer[position ..< position + str.len] == str:
      ParseResult[string](
        success: true,
        value: str,
        endpos: position + str.len
      )
    else:
      ParseResult[string](
        success: false,
        error: "Cannot find string"
      )

proc parseRx*(rx: Regex, cb: Callbacks = noCallbacks): Parser[string, string] =
  return proc(buffer: string, startpos: int = 0): ParseResult[string] =
    if not cb.entry.isNil(): cb.entry(startpos)

    var m: RegexMatch
    if find(buffer, rx, m, startpos) and m.boundaries.a == startpos:
      ParseResult[string](
        success: true,
        endpos: m.boundaries.b,
        value: buffer[m.boundaries.a ..< m.boundaries.b]
      )
    else:
      ParseResult[string](
        success: false
      )


proc parseOr*[Val, Buf](args: seq[Parser[Val, Buf]], cb: Callbacks = noCallbacks): Parser[Val, Buf] =
  return proc(buffer: string, position: int = 0): ParseResult[Val] =
    for parser in args:
      let res = parser(buffer, position)
      if res.success:
        return res

    return ParseResult[Val](
      success: false,
      error: "None of the parsers match"
    )

proc parseAnd*[Val, Buf](args: seq[Parser[Val, Buf]], cb: Callbacks = noCallbacks): Parser[seq[Val], Buf] =
  return proc(buffer: string, position: int = 0): ParseResult[seq[Val]] =
    var posNow = position
    var resVals: seq[Val]
    for parser in args:
      let res = parser(buffer, posNow)
      if res.success:
        resVals.add res.value
        posNow = res.endpos
      else:
        return ParseResult[seq[Val]](
          success: false,
          error: res.error
        )

    return ParseResult[seq[Val]](
      success: true,
      value: resVals,
      endpos: posNow
    )

proc parseOpt*[Val, Buf](parser: Parser[Val, Buf], cb: Callbacks = noCallbacks): Parser[Option[Val], Buf] =
  return proc(buffer: string, position: int = 0): ParseResult[Option[Val]] =
    let res = parser(buffer, position)
    if res.success:
      return ParseResult(
        success: true,
        value: some(res.value),
        endpos: res.endpos
      )
    else:
      return ParseResult(
        success: true,
        value: none(Val),
        endpos: position
      )

proc parseNTimes*[Val, Buf](
  parser: Parser[Val, Buf],
  mintimes: int = 0,
  maxtimes: int = high(int),
  cb: Callbacks = noCallbacks): Parser[seq[Val], Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[seq[Val]] =
    var resSeq: seq[Val]
    var posNow = position
    for i in 0 .. maxtimes:
      let res = parser(buffer, posNow)
      if res.success:
        echo res
        resSeq.add res.value
        posNow = res.endpos
      else:
        if i < mintimes:
          return ParseResult[seq[Val]](success: false, error: res.error)
        elif mintimes <= i:
          return ParseResult[seq[Val]](success: true, value: resSeq, endpos: posNow)

proc parseOneOrMore*[Val, Buf](parser: Parser[Val, Buf], cb: Callbacks = noCallbacks): Parser[seq[Val], Buf] =
  return parseNTimes(parser, mintimes = 1, cb = cb)

proc parseZeroOrMore*[Val, Buf](parser: Parser[Val, Buf], cb: Callbacks = noCallbacks): Parser[seq[Val], Buf] =
  return parseNTimes(parser, cb = cb)

proc parseSkipUntil*[Val, Buf](value: Val, cb: Callbacks = noCallbacks): Parser[Buf, Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[Buf] =
    var i = position
    while true:
      if (i == buffer.len) or (buffer[i] == value):
        if i == position:
          return ParseResult[Buf](success: false, error: "No tokens matched")
        else:
          return ParseResult[Buf](
            success: true,
            endpos: i,
            value: buffer[position ..< i]
          )

      inc i
