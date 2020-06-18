echo 1

import options
import macros


proc retvalMatcher[T1, Res](cb: proc(a: T1): Res): Res = discard

proc generic[T, Res](
  arg: (proc(a: T): Res {.nimcall.})): Res =

  discard

echo typeof generic(
  arg = (proc(a: int): int = a + 1)
)

echo typeof generic(
  arg = (proc(a: int): Option[int] = some(a + 1))
)




# for fld, val in pr.fieldPairs():
#   echo fld
