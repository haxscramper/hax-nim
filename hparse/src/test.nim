type
  Parser[Val, Buf] = proc(buf: Buf): Val

proc parseStr(str: string): Parser[string, char] = discard
var parser: Parser[string, char]
