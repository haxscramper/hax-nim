import sequtils, strformat, terminal
import macros, strutils
import options

var userConf*: tuple[
  maxCol: Option[int],
  minCol: int
] = (
  maxCol: none(int),
  minCol: 0
)

var conf: tuple[
  maxCol: int
]

proc updatePrintConf(): void =
  conf.maxCol = userConf.maxCol.get(terminalWidth())

proc ind(lv: int): string = " ".repeat(lv * 2)

proc pformat*(x: SomeInteger|SomeFloat|string|bool, lv: int): string =
  ind(lv) & $x

proc pformat*(x: enum, lv: int): string =
  ind(lv) & $x

proc pformat*(x: string, lv: int): string =
  result = "\""
  for c in x:
    case c
    of '\0':
      result &= "\\0"
    of '\n':
      result &= "\\n"
    of '\r':
      result &= "\\r"
    of '\t':
      result &= "\\t"
    of '\1'..'\8', '\11'..'\12', '\14'..'\31', '\127'..'\255':
      result &= "\\x"
      const HexChars = "0123456789ABCDEF"
      let n = ord(c)
      result &= HexChars[int((n and 0xF0) shr 4)]
      result &= HexChars[int(n and 0xF)]
    of '\\': result &= "\\\\"
    of '\'': result &= "\\'"
    of '\"': result &= "\\\""
    else: result &= c
  result &= "\""

  result = ind(lv) & result

proc pformat*[T](x: seq[T], lv: int): string =
  if x.len > 0:
    result =
      ind(lv) &
        "@[\n" &
        x.mapIt(pformat(it, lv + 1)).join("\n") &
        ind(lv) & "\n" & ind(lv) & "]\n"
  else:
    result = ind(lv) & "@[]"

proc pformat*(x: tuple, lv: int): string =
  result = ind(lv) & "(\n"
  for name, value in x.fieldPairs():
    result &= ind(lv + 1) & name & ":\n" & pformat(value, lv + 1)

  result &= ind(lv) & ")\n"

proc prettyObj(x: object, lv: int): string =
  result &= ind(lv) & "(\n"
  for name, value in x.fieldPairs:
    result &= ind(lv) & name & ":\n" & pformat(value, lv + 1) & "\n"
  result &= ind(lv) & ")\n"

proc pformat*(x: object, lv: int): string =
  ind(lv) & $type(x) & prettyObj(x, lv + 1)

proc pformat*(x: ref object, lv: int): string =
  if x.isNil:
    ind(lv) & "nil"
  else:
    ind(lv) & ($typeof(x[])).split(":")[0] & prettyObj(x[], lv + 1)

when isMainModule:
  type
    Test1 = object
      arrField: seq[int]
      arr2Field: seq[bool]
      strField: string

    Test2 = ref object
      recurse: seq[Test2]
      sub: tuple[first: Test1, second: float]

  let t = Test2(
    recurse: @[Test2()],
    sub: (first: Test1(strField: "sdfsdf"), second: 0.99999)
  )


  echo pformat(t, 0)
