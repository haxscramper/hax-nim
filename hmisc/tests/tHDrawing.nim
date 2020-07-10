import sugar, strutils, sequtils, strformat
import hmisc/hdrawing

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Drawing":
  test "test":
    var buf = makeBuf(40, 20)
    makeTermText((0,0), @["* (0, 0)"]).render(buf)
    makeTermText((5, 5), @["* (5, 5)"]).render(buf)
    makeTermPoint((39, 19)).render(buf)
    makeTermVLine((0, 0), 18).render(buf)
    makeTermHLine((0, 0), 38).render(buf)
    makeTermPoint((0, 0), '#').render(buf)
    makeBoxedTermText(
      (0, 0), @["Hello world", "Some text", "to render"]
    ).render(buf)

    makeTermRect((8, 6), 3, 3, makeTwoLineBorder()).render(buf)

    echo $buf
