import times

type
  Test = object
    case kind: bool
      of true:
        f1: int
      of false:
        f2: string

    case kind2: bool
      of true:
        f12: float
      of false:
        case nestedKind: bool
          of true:
            f22: seq[float]
          of false:
            f23: int

for fld, val in Test(kind: true).fieldPairs():
  echo fld

proc toPStr(a: int): string = $a
proc toPStr(a: float): string = $a


when declared(toPStr) and (toPStr is proc(a: int): string {.nimcall.}):
    echo "Declared proc with necessary signature"
else:
  echo "signature does not match"
  echo "Type is: ", typeof(toPStr)
  echo "Expected: ", typeof(proc(a: int): string {.nimcall.})

proc f(a: int) = echo a
# proc f(a: char) = echo a # invalid type: 'None' for let
# proc f[T](a: T) = echo "G", a # invalid type: 'None' for let


static:
  for i in 0 .. 100:
    discard compiles(matcher(0, toPStr))

when compiles(matcher(0, toPStr)):
  echo "compiles explicitly"
  let str = matcher(0, toPStr)
  echo "result: ", str

let foo = f
