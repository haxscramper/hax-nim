import unittest
import hmisc/hpprint
import sugar

import hmisc/halgorithm

suite "Tree mapping":
  test "{mapItBFStoSeq} :template:value:":
    type
      Tree = ref object
        name: string
        subs: seq[Tree]

    var tree =
      Tree(
        name: "test",
        subs: @[
          Tree(name: "test11"),
          Tree(name: "test12")])

    assert tree.mapItBFStoSeq(
      it.subs,
      it.name & " on level " & $lv) == @[
        "test on level 0",
        "test11 on level 1",
        "test12 on level 1"]


  ## Unit test suite documentation?
  type
    InTest = object
      val: int
      sub: seq[InTest]

    OutTest = object
      val: string
      sub: seq[OutTest]

  let inval = InTest(val: 91, sub: @[
    InTest(val: 22, sub: @[InTest(val: 900)]),
    InTest(val: 90, sub: @[InTest(val: 0), InTest(val: 888)])]
  )

  let outval = OutTest(
      val: "91--2",
      sub: @[
        OutTest(val: "22--1", sub: @[OutTest(val: "900--0")]),
        OutTest(val: "90--2", sub: @[
          OutTest(val: "0--0"),
          OutTest(val: "888--0")])])


  test "{mapDFSpost} value assetions :proc:generic:value:example:":
    assert inval.mapDFSpost(
      map =
        proc(it: InTest, path: seq[int], subt: seq[OutTest]): OutTest =
          OutTest(val: $it.val & "--" & $(subt.len()), sub: subt),
      getSubnodes =
        proc(it: InTest): seq[InTest] = it.sub
    ) == outval

  test "{mapDFSpost} drop filter tree :proc:value:example":
    ## Iterate over nodes of the tree and leave only ones that have
    ## `val` greater or equal than 90. Discard their children nodes.
    let res = mapDFSpost[InTest, InTest](
      tree = inval,
      map = proc(it: InTest, subn: seq[InTest]): auto =
                if it.val >= 90:
                  some(InTest(val: it.val, sub: subn))
                else:
                  none(InTest),
      getSubnodes = proc(it: InTest): seq[InTest] = it.sub
    )

    assert res is Option[InTest]
    assert res.get().mapItBFStoSeq(it.sub, it.val) == @[91, 90, 888]

  type
    Ast = object
      name: string
      case isToken: bool
        of true:
          tok: string
        of false:
          subnodes: seq[Ast]

  let astInval = Ast(isToken: false, subnodes: @[
    Ast(isToken: false, subnodes: @[
      Ast(isToken: true, tok: "opBrace"),
      Ast(isToken: true, tok: "ident"),
      Ast(isToken: true, tok: "closeBrace"),
    ]),
    Ast(isToken: false, subnodes: @[
      Ast(isToken: true, tok: "opBrace"),
      Ast(isToken: true, tok: "ident"),
      Ast(isToken: true, tok: "closeBrace"),
    ])
  ])


  test "{mapDFSpost} missing subnodes :proc:generic:example":
    ## Check if tree instance can have subnodes before trying to
    ## iterate over it's children

    let res = astInval.mapDFSpost(
      map = proc(it: Ast, subn: seq[Ast]): Option[Ast] =
                if not it.isToken:
                  some(Ast(isToken: false, subnodes: subn))
                elif it.isToken and it.tok == "ident":
                  some(Ast(isToken: true, tok: "ident"))
                else:
                  none(Ast)
      ,
      getSubnodes = proc(it: Ast): seq[Ast] = it.subnodes
      ,
      hasSubnodes = proc(it: Ast): bool = not it.isToken
    )

    pprint res

  test "{mapDFSpost} map to linear structure :proc:macro:example:":
     ## Convert ast to linear structure (graphviz document)
     echo "ok"

  # TODO macro type assertions for option

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
