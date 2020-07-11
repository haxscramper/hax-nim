import macros, strutils

import algo/[halgorithm, hseq_mapping]
export halgorithm
export hseq_mapping
import sequtils


proc colorPrint*(node: NimNode): void =
  # TODO convert nim ast into adequately readable form without using
  # `pygmentize`. Maybe even color macros/templates/procs differently.
  let file = "/tmp/nimast.tmp.nim"
  file.writeFile($node.toStrLit())
  discard staticExec("nimpretty " & file)
  echo staticExec("pygmentize -f terminal " & file)


proc echoi*(indent: int, message: varargs[string, `$`]): void =
  ## Echo with indentation. `message` is joined using spaces
  echo "  ".repeat(indent), message.join(" ")

proc echoi*(message: varargs[string, `$`]): void =
  ## Echo with message joined by spaces
  echo message.join(" ")

func d*(text: varargs[string, `$`]): void =
  debugecho text.join(" ")

template de*(expr: untyped, text: varargs[string, `$`]): void =
  debugecho astToStr(expr), ": ", expr, " ", text.join(" ")

template subnodesEq*(lhs, rhs, field: untyped): untyped =
  ## Check if two objects `lhs` and `rhs` has identical field `field`
  ## by comparing all items in the field. Check if two object's fields
  ## have identical lengths too.
  lhs.field.len() == rhs.field.len() and
  zip(lhs.field, rhs.field).allOfIt(it[0] == it[1])
