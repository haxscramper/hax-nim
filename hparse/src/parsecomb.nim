## Parser combinator librariy

import options

type
  ParseResult*[T] = object
    case success*: bool
      of true:
        value*: T
        endpos*: int
      of false:
        error*: string

  Parser*[T] = proc(buffer: string, position: int): ParseResult[T]

proc parseString*(str: string): Parser[string] =
  return proc(buffer: string, position: int): ParseResult[string] =
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

proc parseOr*[T](args: seq[Parser[T]]): Parser[T] =
  return proc(buffer: string, position: int): ParseResult[T] =
    for parser in args:
      let res = parser(buffer, position)
      if res.success:
        return res

    return ParseResult[T](
      success: false,
      error: "None of the parsers match"
    )

proc parseAnd*[T](args: seq[Parser[T]]): Parser[seq[T]] =
  return proc(buffer: string, position: int): ParseResult[seq[T]] =
    var posNow = position
    var resVals: seq[T]
    for parser in args:
      let res = parser(buffer, posNow)
      if res.success:
        resVals.add res.value
        posNow = res.endpos
      else:
        return ParseResult[seq[T]](
          success: false,
          error: res.error
        )

    return ParseResult[seq[T]](
      success: true,
      value: resVals,
      endpos: posNow
    )
