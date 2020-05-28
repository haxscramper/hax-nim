import macros
import options

type
  Parser[Val, Buf] = proc(buf: Buf): Val

proc parseStr(str: string): Parser[string, char] = discard
var parser: Parser[string, char]


proc genericImpl[T](arg: T) =
  echo "using generic"

proc genericImpl[T](arg: seq[T]) =
  echo "Using less generic"

proc genericImpl(arg: string) =
  echo "using speciialiation"

genericImpl("hello")
genericImpl(@["Hello"])
genericImpl(12)

# macro makeBufType(t: untyped): untyped =
#   echo t.toStrLit()
#   # let res =
#   #   if t is string:
#   #     typeof(string)
#   #   else:
#   #     typeof(seq[t])

# echo typeof(makeBufType(string))

import options

proc t[T](arg: Option[tuple[field1: T, field2: int]]) = discard

t(some((1, 2)))
