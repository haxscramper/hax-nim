import sugar, strutils, sequtils, strformat
import hmisc/hdrawing

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Drawing":
  test "test":
    var buf = makeBuf(16, 16, (8, 8))
    makeTermVline(4, 4, 4).render(buf)
    makeTermRect((-6, -6), 12, 8, '*').render(buf)

    echo $buf
