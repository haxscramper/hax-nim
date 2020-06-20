import sugar, strutils, sequtils, strformat, algorithm
import hmisc/[htrie, helpers, halgorithm, hpprint]

#================================  tests  ================================#

import unittest

suite "Main":
  test "Add path to trie :object:":
    var tr: Trie[int, int]
    tr[[1, 2, 3]] = 190
    assertEq tr.paths, @[@[1, 2, 3]]

  test "Multiple trie paths :object:":
    var tr: Trie[int, char]
    tr[[2, 4, 90, 9]] = '9'
    tr[[8, 2, 3, 3]] = '%'
    tr[[1, 2, 5, 6]] = '&'
    assert tr.paths.len() == 3

  test "Has value assertions :object:":
    var tr: Trie[int, int]
    tr[[2, 3, 4]] = 9
    assert tr.prefixHasValue([2, 3, 4])
    assert tr.prefixHasValue([2, 3, 4, 5])
    assert not tr.prefixHasValue([1, 2, 3])
    assert not tr.prefixHasValue([2, 3])
