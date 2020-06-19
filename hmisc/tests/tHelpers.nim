import unittest

import hmisc/helpers

func empty[T](): seq[T] = discard

suite "Misc helper functions":
  test "Split list":
    expect AssertionError:
      discard empty[int]().splitList()

    assert @[1].splitList() == (1, empty[int]())
    assert @[1, 2].splitList() == (1, @[2])

import hmisc/iflet

suite "If let":
  test "{iflet} Simple :macro:":
    var ok: bool = false
    iflet (val = none(int)):
      echo "none is something and it has value of ", val
      fail()
    else:
     ok = true

    assert ok

  test "{iflet} Else-if brances :macro:":
    var final: int = 0
    iflet (val = none(int)):
      final = 3
    elif 2 == 3:
      final = 5
    else:
      final = 1

    assert final == 1

  test "{iflet} Return value from body using block :macro:":
    let final = block:
      iflet (val = none(int)):
        3
      elif 2 == 3:
        5
      else:
        1

    assert final == 1

  test "{iflet} Iflet in generic function :macro:generic:":
    proc g[T](arg: T): T =
      var res = some(arg)
      iflet (resVal = res):
        assert resVal == arg
        return resVal
      else:
        fail()

    assert g(12) == 12

  test "{iflet} inside of template :macro:template:":
    template whileLet(expr, body: untyped): untyped =
      var ok: bool = true
      while ok:
        iflet (val = expr):
          body
        else:
          ok = false

    var cnt: int = 0
    whileLet(none(int)):
      inc cnt

    assert cnt == 0


