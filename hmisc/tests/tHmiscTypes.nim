import sugar, strutils, sequtils, strformat

import hmisc/hmisc_types

import unittest

suite "Seq2D":
  test "se":
    let grid = toSeq2D(@[@["HEllo"]])
    let grid2 = grid.mapIt2d(it.len)
    assert grid2[0, 0] == 5
