import unittest
import hmisc/hpprint
import sugar, json, sequtils, tables

import hmisc/halgorithm

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

suite "Tree mapping":
  ## Test tempates/procs/macros for working on trees.
  # Tests are a little verbose
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

  test "{mapItBFStoSeq} check missing subnodes :template:example:":
    ## Get subnodes only from nodes that have subnodes
    # echo astInval.mapItBFStoSeq(
    #   it.subnodes,
    #   it.val,
    # )

  test "{mapItBFStoSeq} filter json node :template:example:":
    ## Map json tree to sequence. 
    let jsonNode = parseJson """
      {"menu": {
        "id": "file",
        "value": "File",
        "popup": {
          "menuitem": [
            {"value": "New", "onclick": "CreateNewDoc()"},
            {"value": "Open", "onclick": "OpenDoc()"},
            {"value": "Close", "onclick": "CloseDoc()"}
          ]
        }
      }}
      """

    let values = jsonNode.mapItBFStoSeq(
      # Get only values from JObject
      if it.kind == JObject:
        collect(newSeq):
          for key, val in it.getFields():
            val
      else:
        it.getElems()
      ,
      # Get `onclick` fields from all objects
      if it.kind == JObject and "onclick" in it:
        some(it["onclick"].getStr())
      else:
        none(string)
      ,
      # Only try to get values from objects and arrays
      it.kind in {JObject, JArray}
    )

    assert values == @["CreateNewDoc()", "OpenDoc()", "CloseDoc()"]

  test "{mapDFSpost} check missing subnodes :proc:generic:example":
    ## Check if tree instance can have subnodes before trying to
    ## iterate over it's children

    let res: Option[Ast] = astInval.mapDFSpost(
      map = proc(it: Ast, subn: seq[Ast]): Option[Ast] =
                if not it.isToken:
                  some(Ast(isToken: false, subnodes: subn))
                elif it.isToken and it.tok == "ident":
                  some(Ast(isToken: true, tok: "ident"))
                else:
                  none(Ast)
      ,
      # Field is accessible only for non-token ast nodes
      getSubnodes = proc(it: Ast): seq[Ast] = it.subnodes
      ,
      hasSubnodes = proc(it: Ast): bool = not it.isToken
    )

    assert res.get().mapItBFStoSeq(
      it.subnodes,
      if it.isToken: some(it.tok) else: none(string),
      not it.isToken) == @["ident", "ident"]

  test "{mapDFSpost} map to linear structure :proc:macro:example:":
    ## Convert ast to linear structure (graphviz document)
    discard

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

suite "Simple sequence templates":
  test "{allOfIt} empty sequence :template:":
    var empty: seq[int]
    assert empty.allOfIt(false)

  test "{allOfIt} use example :template:example:":
    assert @[1, 2, 3].allOfIt(it > 0)

  test "{mapPairs} type assertion :template:type:":
    assert {1: "hello", 2: "222"}.mapPairs(
      $lhs & "--" & rhs
    ) is seq[string]

  test "{mapPairs} return value :template:value:":
    assert {1: 2, 3: 4}.mapPairs(
      lhs + rhs
    ) == @[3, 7]

  test "{mapPairs} map table values :template:example:":
    assert {1: 3, 4: 5}.toTable().mapPairs(
      max(lhs, rhs)
    ) ==  @[3, 5]

  test "{mapPairs} iterate indexed :template:example:":
    let res = @["a", "b"].mapPairs(
      block:
        assert lhs is int
        assert rhs is string
        $lhs & ":" & rhs
    )

    assert res is seq[string]
    assert res == @["0:a", "1:b"]

  test "{mapPairs} custom `pairs` :template:":
    type U = object

    iterator pairs(u: U): (float, string) =
      yield (1.2, "1222")

    # ignored by `mapPairs`
    iterator items(u: U): string =
      yield "222"

    let res = U().mapPairs($lhs & " () " & rhs)
    assert res is seq[string]
    assert res == @["1.2 () 1222"]

  test "{mapPairs} custom `items` :template:":
    type U = object

    # No pairs declared - using `items` with index as `lhs`
    iterator items(u: U): string =
      yield "222"
      yield "aaa"

    let res = U().mapPairs($lhs & " () " & rhs)
    assert res is seq[string]
    assert res == @["0 () 222", "1 () aaa"]

  test "{subnodesEq} :template:":
    type
      U = object
        s: seq[U]

    assert subnodesEq(U(), U(), s)
    assert subnodesEq(U(s: @[U(), U()]), U(s: @[U(), U()]), s)
    assert not subnodesEq(
      U(s: @[U(), U(), U()]), U(s: @[U(), U()]), s)

  test "{findItFirst} :template:example:":
    assert @["A", "B", "D"].findItFirst(it == "A") == "A"

    expect AssertionError:
      discard @["A", "B"].findItFirst(it == "D")

  test "{findItFirstOpt} :template:example:":
    assert @["A"].findItFirstOpt(it == "A").isSome()
    assert @["A"].findItFirstOpt(it == "D").isNone()

  test "{findIt} :template:":
    assert @[1].findIt(it == 1) == 0
    assert @[2].findIt(it == 1) == -1

  test "{max} :proc:generic:":
    assert @[1, 2, 3].max(90) == 3
    var tmp: seq[int]
    assert tmp.max(80) == 80

  test "{anyOfIt} :template:":
    assert [1, 2, 3].anyOfIt(it > 1)
    assert not [3, 4, 5].anyOfIt(it < 1)
    var tmp: seq[int]
    assert not tmp.anyOfIt(it > 9)


