import sugar, strutils, sequtils, strformat
import hmisc/hdrawing

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Drawing":
  test "test":
    var buf = makeBuf()
    makeTermText((0,0), @["* (0, 0)"]).render(buf)
    makeTermText((8, 5), @["* (5, 5)"]).render(buf)
    makeTermPoint((39, 19)).render(buf)
    makeTermVLine((0, 0), 18).render(buf)
    makeTermHLine((0, 0), 38).render(buf)
    makeTermPoint((0, 0), '#').render(buf)
    makeBoxedTermText(
      (0, 0), @["Hello world", "Some text", "to render"]
    ).render(buf)

    let w = 10
    makeTermRect((8, 6), w, 5, makeTwoLineRectBorder()).render(buf)

    for p in 6 ..+ 5:
      makeTermText((8 + w, p), @[&"* (5, {p})"]).render(buf)

    makeTermText((8, 11), @["12345"]).render(buf)

    makeBoxedTermText(
      (15, 15), @["Text inside", "of unicode box"],
      makeTwoLineRectBorder()
    ).render(buf)

    makeTermGrid(
      (30, 20),
      @[
        @["HEllo", "world"],
        @["wer", "2333"],
        @["1222", "@@@"]
      ],
      makeThinLineGridBorders()
    ).render(buf)

    echo $buf
