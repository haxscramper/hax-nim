template mapItBFStoSeq*(
  topNode: typed,
  subNode: untyped,
  op: untyped): untyped =
    # IDEA maybe add better static type checking? Something like
    # boost's concepts: "the top node type must satisfy requirement
    # is-tree-on-$topNode". And I will check that `subNode` is indeed
    # contains objects of the Node type.
    ## Perform BFS (`Breadth First Search
    ## <https://en.wikipedia.org/wiki/Breadth-first_search>`_)
    ## iteration of recursive data type. `topNode` is a top-level item
    ## in tree, `subNode` is name of the field that contains child
    ## nodes, `op` is an expression that will be evaluated to get
    ## results. Varables `it` and `lv` are injected into scope. `it`
    ## is a value of current node in tree, `lv` is a level of the tree
    ## we are currenty in (might be useful for checking for root node
    ## or something like that). `lv` starts at 0 and is incremented
    ## each on each iteration.
    runnableExamples:
      import deques
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
        subs,
        it.name & " on level " & $lv) == @[
          "test on level 0",
          "test11 on level 1",
          "test12 on level 1"]


    type OutType = type((
      block:
        var it {.inject.}: type(topNode)
        var lv {.inject.}: int
        op))

    type VertType = type((topNode))

    var result: seq[OutType] = @[]

    var q = initDeque[(VertType, int)]()
    q.addLast((topNode, 0))
    while q.len != 0:
      var tmp = q.popFirst()
      let it {.inject.} = tmp[0]
      let lv {.inject.} = tmp[1]

      result.add(op)
      for sub in it.subNode:
        q.addLast((sub, lv + 1))

    result

template mergeUniqByIt*(sequence, operation: untyped): untyped =
  let s = sequence
  var prev =
    block:
      let it {.inject.} = s[0]
      operation

  var equal: seq[type(s[0])] = @[s[0]]
  var result: seq[type(equal)]

  for i in 1..<s.len:
    let it {.inject.} = s[i]
    let new = operation

    if new == prev:
      equal.add it
    else:
      result.add equal
      equal = @[it]

    prev = new

  result.add equal
  result
