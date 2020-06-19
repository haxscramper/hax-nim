import unittest

import hmisc/helpers

func empty[T](): seq[T] = discard

suite "Misc helper functions":
  test "Split list":
    expect AssertionError:
      discard empty[int]().splitList()

    assert @[1].splitList() == (1, empty[int]())
    assert @[1, 2].splitList() == (1, @[2])
