import sugar, strutils, sequtils, strformat, options
import hmisc/types/[hdrawing, hgeometry_primitives]
import hmisc/algo/halgorithm

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Drawing":
  test "test":
    block:
      var buf = newBuf()
      newTermText((0,0), @["* (0, 0)".toRunes()]).render(buf)
      newTermText((8, 5), @["* (5, 5)"]).render(buf)
      newTermPoint((39, 19)).render(buf)
      newTermVLine((0, 0), 18).render(buf)
      newTermHLine((0, 0), 38).render(buf)
      newTermPoint((0, 0), '#').render(buf)
      newBoxedTermText(
        (0, 0), @["Hello world", "Some text", "to render"]
      ).render(buf)

      let w = 10
      newTermRect((8, 6), w, 5, makeTwoLineRectBorder()).render(buf)

      for p in 6 ..+ 5:
        newTermText((8 + w, p), @[&"* (5, {p})"]).render(buf)

      newTermText((8, 11), @["12345"]).render(buf)

      newBoxedTermText(
        (15, 15), @["Text inside", "of unicode box"],
        makeTwoLineRectBorder()
      ).render(buf)
      # echo $buf


    block:
      var buf = newBuf()
      newTermGrid(
        (0, 0),
        @[
          @["HEllo", "world"],
          @["wer", "2333"],
          @[
            "1222",
            newTermGrid(
              (0, 0),
              @[
                @["HEllo", "world"],
                @["wer", "2333"],
                @["1222", "Hello"]
              ],
              makeThinLineGridBorders()
            ).toString(),
            newTermGrid(
              (0, 0),
              @[
                @["HEllo", "world"],
                @["wer", "2333"],
                @["1222", "Hello"]
              ],
              makeEmptyGridBorders()
            ).toString()
          ]
        ],
        makeThinLineGridBorders()
      ).render(buf)
      # echo $buf

  test "Multicell grid":
    let size1x1 = makeSize(1, 1)
    echo newTermMultiGrid(
      (0, 0),
      @[
        @[some(size1x1), some(size1x1),   some(size1x1)],
        @[some(size1x1), some(makeSize(2, 2)),           none(Size)],
        @[some(size1x1), none(Size),      none(Size)],
        @[some(size1x1), some(size1x1),   some(size1x1)],
        @[some(size1x1), some(makeSize(2, 2)),           none(Size)],
        @[some(size1x1), none(Size),      none(Size)],
        @[some(size1x1), none(Size),      none(Size)],
      ],
      @[5, 5, 5],
      @[3, 3, 3, 3, 3, 3, 3],
      makeThinLineGridBorders(),
    ).toString()
