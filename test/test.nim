import macros, tables, strformat, strutils, sequtils, strutils
import options
import re

import macros, times, strutils, typetraits

import deques

# template mapIt*(optSeq: )
#[ IMPLEMENT call option only if it is not none
template callIt*[T](it: Option[T], op: untyped): untyped =
  type outType = type((
    block:
      var it{.inject.}: type(items(s)); op))

  var result: Option[outType] = none[outType]
  if opt.isSome:
    result = op(it.get)
]#

dumpTree:
  loop -> seq[(int, float)]:
    lfor x in @[1,2,2]
    lfor e in @[0.01]
    linitially:
      echo "Start-test"
    ldo:
      lcollect (x * e) into mults
      lcollect x into nums
    lfinally:
      lreturn zip(nums, mults)
