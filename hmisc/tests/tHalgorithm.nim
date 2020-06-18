import unittest

import hmisc/halgorithm

suite "Tree mapping":
  ## Unit test suite documentation?
  type
    InTest = object
      val: int
      sub: seq[InTest]

    OutTest = object
      val: string
      sub: seq[OutTest]

  let inval = InTest(val: 12, sub: @[
    InTest(val: 22, sub: @[InTest(val: 900)]),
    InTest(val: 90, sub: @[InTest(val: 0), InTest(val: 888)])]
  )

  let outval = OutTest(
      val: "12--2",
      sub: @[
        OutTest(val: "22--1", sub: @[OutTest(val: "900--0")]),
        OutTest(val: "90--2", sub: @[
          OutTest(val: "0--0"),
          OutTest(val: "888--0")])])


  test "{mapDFSpost} value assetions :proc:value:example:":
    assert inval.mapDFSpost(
      map =
        proc(it: InTest, path: seq[int], subt: seq[OutTest]): OutTest =
          OutTest(val: $it.val & "--" & $(subt.len()), sub: subt),
      getSubnodes =
        proc(it: InTest): seq[InTest] = it.sub
    ) == outval


  test "{mapItTreeDFS} type assertions :macro:type:example:":
    let res = InTest().mapItTreeDFS(
      it.sub, OutTest,
      OutTest(val: $it.val & "+"))

    assert res is OutTest

  test "{mapItTreeDFS} value assertions :macro:value:":
    assert inval.mapItTreeDFS(
      it.sub, OutTest,
      block:
        OutTest(
          val: $it.val & "--" & $(subt.len()),
          sub: subt
        )) == outval
