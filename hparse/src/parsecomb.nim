## Parser combinator librariy

import options
import regex

type
  ParseResult*[T] = object
    case success*: bool
      of true:
        value*: T
        endpos*: int
      of false:
        error*: string

  Parser*[Val, Buf] = proc(buffer: Buf, position: int = 0): ParseResult[Val]

proc parseString*(str: string): Parser[string, string] =
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

proc parseRx*(rx: Regex): Parser[string, string] =
  return proc(buffer: string, startpos: int = 0): ParseResult[string] =
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


proc parseOr*[V, T](args: seq[Parser[V, T]]): Parser[V, T] =
  return proc(buffer: string, position: int = 0): ParseResult[T] =
    for parser in args:
      let res = parser(buffer, position)
      if res.success:
        return res

    return ParseResult[T](
      success: false,
      error: "None of the parsers match"
    )

proc parseAnd*[V, T](args: seq[Parser[V, T]]): Parser[seq[V], T] =
  return proc(buffer: string, position: int = 0): ParseResult[seq[V]] =
    var posNow = position
    var resVals: seq[V]
    for parser in args:
      let res = parser(buffer, posNow)
      if res.success:
        resVals.add res.value
        posNow = res.endpos
      else:
        return ParseResult[seq[V]](
          success: false,
          error: res.error
        )

    return ParseResult[seq[V]](
      success: true,
      value: resVals,
      endpos: posNow
    )

proc parseOpt*[Val, Tok](parser: Parser[Val, Tok]): Parser[Option[Val], Tok] =
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
