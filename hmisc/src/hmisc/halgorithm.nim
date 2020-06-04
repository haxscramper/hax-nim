import sequtils

import algorithm
export algorithm

import deques
export deques

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
  ## For each element in sequence apply `operation` and compare
  ## results. Consequent items from `sequence` with equal results will
  ## be added into single subsequence
  runnableExamples:
    assert @[1,2,3,4,4,5].mergeUniqByIt(it) ==
           @[@[1], @[2], @[3], @[4, 4], @[5]]

    assert @[(1,2), (1,2), (1,4), (2,3)].mergeUniqByIt(it[0]) ==
           @[@[(1, 2), (1, 2), (1, 4)], @[(2, 3)]]

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

template twoPassSortByIt*(
  sequence, operation1, operation2: untyped
         ): untyped =
  ## Sort input sequence using firt `operation1`, then group into
  ## 2d-sequence based on result of `operation1` and sort each
  ## subsequence using `operation2`
  runnableExamples:
    # Sort by first field and then by second
    assert @[(1,2), (1,9), (4,32), (1,3)].twoPassSortByIt(it[0], it[1]) ==
           @[@[(1, 2), (1, 3), (1, 9)], @[(4, 32)]]


  let s = sequence
  let firstSorted = sortedByIt(sequence, operation1)

  var secondSorted: seq[type(@[s[0]])]
  for equal in firstSorted.mergeUniqByIt(operation1):
    secondSorted.add(equal.sortedByIt(operation2))

  secondSorted

template allOfIt*(s: untyped, op: untyped): bool =
  var res = true
  for it {.inject.} in s:
    if not op:
      res = false
      break

  res


when isMainModule:
  echo @[(1,2), (1,9), (4,32), (1,3)].twoPassSortByIt(it[0], it[1])


# TODO implement
# template maxIt(sequence, operation: untyped): untyped  =
#   assert sequence.len > 0, "cannot find max of empty sequence"
#   var result: type(sequence[0]) = sequence[0]
#   for i in 1..<sequence.len:
#     let
#       lhs {.inject.} = result
#       rhs {.inject.} = sequence[i]

#     if operation: # lhs is 'smaller' than rhs
#       result = lhs
#     else:
#       result = rhs

#   return result
