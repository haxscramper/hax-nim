import macros, strutils

import algo/[halgorithm, hseq_mapping]
export halgorithm
export hseq_mapping
import sequtils
include hdebug_misc

proc colorPrint*(node: NimNode): void =
  # TODO convert nim ast into adequately readable form without using
  # `pygmentize`. Maybe even color macros/templates/procs differently.
  let file = "/tmp/nimast.tmp.nim"
  file.writeFile($node.toStrLit())
  discard staticExec("nimpretty " & file)
  echo staticExec("pygmentize -f terminal " & file)

template expectType*(op, t: untyped): untyped =
  static: assert op is t

proc echoi*(indent: int, message: varargs[string, `$`]): void =
  ## Echo with indentation. `message` is joined using spaces
  echo "  ".repeat(indent), message.join(" ")

proc echoi*(message: varargs[string, `$`]): void =
  ## Echo with message joined by spaces
  echo message.join(" ")

template subnodesEq*(lhs, rhs, field: untyped): untyped =
  ## Check if two objects `lhs` and `rhs` has identical field `field`
  ## by comparing all items in the field. Check if two object's fields
  ## have identical lengths too.
  lhs.field.len() == rhs.field.len() and
  zip(lhs.field, rhs.field).allOfIt(it[0] == it[1])

template fail*(msg: string): untyped =
  raiseAssert(msg)

template nnil*(): untyped =
  defer:
    let iinfo = instantiationInfo()
    when result is seq:
      for idx, val in result:
        when val is ref:
          assert (val != nil)
        else:
          for name, fld in val.fieldPairs():
            when fld is ref:
              if fld.isNil:
                raiseAssert("Non-nil return assert on line " &
                  $iinfo.line & ". Error idx: " & $idx & " fld name: " &
                  name & ". Item type is " & $typeof(val)
                )
    else:
      assert (result != nil)
