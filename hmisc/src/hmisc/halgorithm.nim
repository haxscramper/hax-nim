import sequtils
import strutils, strformat
import macros
import sugar

import algorithm
export algorithm

import deques
export deques

##[

.. raw:: html
  <style>

  table th.docinfo-name {
      text-align: left;
  }

  </style>

]##

# IDEA `itemsBFS` and `itemsDFS` to iterate over nodes in tree
# IDEA `pairsBFS` and `pairsDFS` to iterate over paths
#      (similar to index?) + nodes in tree

template mapItBFStoSeq*(
  topNode: typed,
  subNode: untyped,
  op: untyped,
  hasSubnodes: untyped = true,
  filterOptions: static[bool] = true): untyped =
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
    # TODO assert correct types for `topNode...` and `subNodes...`
    type OutType = type((
      block:
        var it {.inject.}: type(topNode)
        var lv {.inject.}: int
        op))

    const doFiltering = (OutType is Option) and filterOptions
    when doFiltering:
      type SeqType = type((
        block:
          var res: OutType
          get(res)))

    else:
      type SeqType = OutType

    type VertType = type((topNode))
    var result: seq[SeqType] = @[]

    var q = initDeque[(VertType, int)]()
    q.addLast((topNode, 0))
    while q.len != 0:
      var tmp = q.popFirst()
      let it {.inject.} = tmp[0]
      let lv {.inject.} = tmp[1]

      when doFiltering:
        let tmpRes: OutType = op
        if tmpRes.isSome():
          result.add tmpRes.get()
      else:
        result.add(op)

      if hasSubnodes:
        for sub in subNode:
          static:
            assert sub is VertType,
              "Mismatch between type of the subnodes and root tree - `subNode` is " &
              $typeof(subNode) & "while `topNode` is " & $typeof(topNode)

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

template anyOfIt*(sequence: typed, predicate: untyped): bool =
  ## Return `true` if for any of the items in sequence `predicate`
  ## evaluates as `true`. Otherwise return false.
  var result = false
  for it {.inject.} in sequence:
    if predicate:
      result = true
      break

  result

template allOfIt*(s: untyped, op: untyped): bool =
  ## True if for all items in `s` predicate `op` returns true.
  not s.anyOfIt(not op)


proc nthType1*[T1, T2](a: (T1, T2)): T1 =
  ## Helper proc to get first type from tuple. Used as workaround for
  ## `pairs` iterator
  discard

proc nthType2*[T1, T2](a: (T1, T2)): T2 =
  ## Helper proc to get second type from tuple. Used as workaround for
  ## `pairs` iterator
  discard

macro mapPairs*(
  inseq: untyped, op: untyped,
  injectNames: untyped): untyped =
  ## `mapIt` for object with `pairs`. `lhs`, `rhs` and `idx` are
  ## injected into scope
  assert injectNames.kind == nnkPar
  var inj: tuple[lhs, rhs, idx: string] = ("lhs", "rhs", "idx")
  for pair in injectNames:
    case $pair[0]:
      of "lhs": inj.lhs = $pair[1]
      of "rhs": inj.rhs = $pair[1]
      of "idx": inj.idx = $pair[1]

  let
    lhsId = ident(inj.lhs)
    rhsId = ident(inj.rhs)
    idxId = ident(inj.idx)

  quote do:
    block:
      const openarrPairs = ((`inseq` is array) or (`inseq` is seq) or (`inseq` is openarray))

      when openarrPairs:
        when `inseq`[0] is tuple:
          type TLhs = type((`inseq`[0][0]))
          type TRhs = type((`inseq`[0][1]))
        else:
          type TLhs = int
          type TRhs = type((`inseq`[0]))
      else:
        when compiles(for k, v in pairs(`inseq`): discard):
          type TLhs = type((pairs(`inseq`).nthType1))
          type TRhs = type((pairs(`inseq`).nthType2))
        else:
          type TLhs = int
          type TRhs = type((items(`inseq`)))

      var `idxId` {.inject.}: int = 0
      type TRes = type((
        block:
          var `lhsId` {.inject.}: TLhs
          var `rhsId` {.inject.}: TRhs
          `op`))

      var res: seq[TRes]

      when openarrPairs:
        when `inseq`[0] is tuple:
          for (`lhsId`, `rhsId`) in `inseq`:
            res.add `op`
            inc `idxId`

        else:
          for `lhsId`, `rhsId` in `inseq`:
            res.add `op`
            inc `idxId`

      else:
        when compiles(for k, v in pairs(`inseq`): discard):
          for `lhsId`, `rhsId` in `inseq`:
            res.add `op`
            inc `idxId`
        else:
          var lhs {.inject.}: int = 0
          for `rhsId` in `inseq`:
            res.add `op`
            inc `lhsId`
            inc `idxId`

      res


template mapPairs*(inseq: untyped, op: untyped): untyped =
  mapPairs(inseq, op, (lhs: lhs, rhs: rhs, idx: idx))

proc max*[T](x: openArray[T], default: T): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  ## use `default` if array is empty
  if x.len == 0:
    result = default
  else:
    for i in x:
      if result < i: result = i



template findIt*(s: typed, op: untyped): int =
  ##[ Find first element of the sequence for which `op` evaluates as
  true and return it's index. If no such element is found return -1
  ]##

  var result = -1
  for idx, it {.inject.} in s:
    if op: result = idx; break

  result

template findItFirst*(s: typed, op: untyped): untyped =
  var res: typeof(s[0])
  var found: bool = false
  for it {.inject.} in s:
    if op:
      res = it
      found = true
      break

  assert found, "Item not found in sequence"

  res


template findItFirstOpt*(s: typed, op: untyped): untyped =
  var res: Option[typeof(s[0])]
  for it {.inject.} in s:
    if op:
      res = some(it)
      break

  res

template subnodesEq*(lhs, rhs, field: untyped): untyped =
  ## Check if two objects `lhs` and `rhs` has identical field `field`
  ## by comparing all items in the field. Check if two object's fields
  ## have identical lengths too.
  lhs.field.len() == rhs.field.len() and
  zip(lhs.field, rhs.field).allOfIt(it[0] == it[1])

import options
export options

# proc mapDFSpost*[InTree, OutTree](
#   tree: InTree,
#   map: proc(n: InTree, path: seq[int], subn: seq[OutTree]): OutTree,
#   getSubnodes: proc(tree: InTree): seq[InTree],
#   hasSubnodes: proc(it: InTree): bool = (proc(it: InTree): bool = true),
#   path: seq[int] = @[0]): OutTree =
#   ## Convert one tree type into another using post order DFS traversal
#   let nodeRes: seq[OutTree] = collect(newSeq):
#     for idx, node in getSubnodes(tree):
#       mapDFSpost(node, map, getSubnodes, hasSubnodes, path & @[idx])

#   return map(tree, path, nodeRes)

proc mapDFSpost*[InTree, OutTree, CbRes](
  tree: InTree,
  map: proc(
    n: InTree,
    path: seq[int],
    subn: seq[OutTree],
    inSubn: seq[InTree]
  ): CbRes,
  getSubnodes: proc(tree: InTree): seq[InTree],
  hasSubnodes: proc(it: InTree): bool = (proc(it: InTree): bool = true),
  path: seq[int] = @[0]): CbRes =
  ##[

Map one tree into another

Convert one tree type into another using post-order DFS traversal.
Tree is iterated in bottom-up manner. After leaving each node `map` is
called to generate new result. `subn` is a result of previous.

For tree like this `map` will first be executed for `IN1.1` through
`IN1.3`. Results of the map will be passed as `subn`.

.. code::
    +-------+     +-------+     +-------+
    | IN1.3 | <-- |  IN1  | --> | IN1.1 |
    +-------+     +-------+     +-------+
                    |
                    |
                    v
                  +-------+
                  | IN1.2 |
                  +-------+


## Parameters

:tree: Input tree
:map: Procedure for converting one tree into another.

  `CbRes` can be either `OutTree` or `Option[OutTree]`. In latter case
  `none()` results will be filtered out from the `subn`

  :n: Input subtree
  :path: Path of the current subtree in original node
  :subn: Evaluation results from child nodes.
  :inSubn: List of child nodes for original node.

  Elements in `subn` and `inSubn` are ordered identically: `subn[i] ==
  map(inSubn[i], ...)`

:getSubnodes: Get subnodes from current tree
:hasSubnodes: Check if input node can have subnodes

  ]##

  # IDEA if `OutTree` is a sequence perform recursive concatenation of
  # the items instead of joining them in tree.
  static:
    assert (CbRes is OutTree) or (CbRes is Option[OutTree])

  var subnodes: seq[InTree]
  let nodeRes: seq[OutTree] =
    if hasSubnodes(tree):
      var tmp: seq[OutTree]
      subnodes = getSubnodes(tree)
      for idx, node in subnodes:
        var res = mapDFSpost(
          node, map, getSubnodes, hasSubnodes,  path & @[idx])

        when CbRes is Option[OutTree]:
          if res.isSome():
            tmp.add res.get()
        else:
          tmp.add res

      tmp
    else:
      @[]

  return map(tree, path, nodeRes, subnodes)


proc mapDFSpost*[InTree, OutTree](
  tree: InTree,
  map: proc(n: InTree, subn: seq[OutTree]): OutTree,
  getSubnodes: proc(tree: InTree): seq[InTree],
  hasSubnodes: proc(it: InTree): bool = (proc(it: InTree): bool = true),
  path: seq[int] = @[0]): OutTree =
  ## Overload without `path` for `map`
  # TODO DOC
  mapDFSpost(
    tree,
    proc(
      n: InTree,
      path: seq[int],
      subn: seq[OutTree],
      inSubn: seq[InTree]
    ): OutTree = map(n, subn),
    getSubnodes,
    hasSubnodes,
    path
  )



proc mapDFSpost*[InTree, OutTree](
  tree: InTree,
  map: proc(n: InTree, subn: seq[OutTree]): Option[OutTree],
  getSubnodes: proc(tree: InTree): seq[InTree],
  hasSubnodes: proc(it: InTree): bool = (proc(it: InTree): bool = true),
  path: seq[int] = @[0]): Option[OutTree] =
  ## Overload without `path` for `map`
  # TODO DOC
  return mapDFSpost(
    tree = tree,
    map = proc(
      n: InTree,
      path: seq[int],
      subn: seq[OutTree],
      inSubn: seq[InTree]
    ): Option[OutTree] = map(n, subn),
    getSubnodes = getSubnodes,
    hasSubnodes = hasSubnodes,
    path = path
  )


type
  DfsFrame[T, R] = object
    idx: int
    inSubt: seq[T]
    path: seq[int]
    subt: seq[R]

  DfsStack[T, R] = seq[DfsFrame[T, R]]

template last[T](stack: var seq[T]): var T = stack[^1]
template last[T](stack: seq[T]): T = stack[^1]


func makeDfsFrame[T, R](elems: seq[T], path: seq[int]): DfsFrame[T, R] =
  DfsFrame[T, R](idx: 0, inSubt: elems, path: path, subt: @[])

template mapItDFSImpl*[InTree, OutTree](
  inTree: InTree,
  subnodeCall: untyped,
  op: untyped,
  hasSubnodes: untyped = true): untyped =
  # NOTE Too lazy to check for already implemented features. I guess
  # most oft he things are implemented? #idea #software##emacs write
  # helper to jump to closest todo in the code. #todo parse todo items
  # in comments.

  # TODO add proc for checking if futher recursion is not needed (trim
  # down arbitrary branches from tree)

  # TODO return `Option[OutTree]` from map function. Support both
  # versions: return-all and return-option (NOTE: can be determined
  # using typeof `op`)

  # NOTE `subnodeCall` does not feel intuitive - injecting current
  # node into scope will be better.
  # TEST - either write unit tests or chec if existing ones cover this
  #      explicitly

  # IDEA TEST write example/test for mapping tree to DFS sequence

  # TODO predicate to check if item has subnodes or not.
  # TEST predicated for subnodes checking
  # REVIEW TODO STYLE rename `outType` into `intermediateType` ?

  var stack: seq[DfsFrame[InTree, OutTree]]
  var res: OutTree
  stack.add makeDfsFrame[InTree, OutTree](@[intree], @[])

  var cnt: int = 0
  block dfsLoop:
    while cnt < 20:
      if stack.last.idx == stack.last.inSubt.len:
        # Current toplevel frame reached the end
        let top = stack.pop
        let foldRes = (
          block:
            let
              it {.inject.} = stack.last.inSubt[stack.last.idx]
              path {.inject.} = top.path
              subt {.inject.} = top.subt
              inSubt {.inject.} = top.inSubt

            op
        )

        if not (foldRes is OutTree) or (foldREs is Option[OutTree]):
          let pos = instantiationInfo().line
          raiseAssert(
            "Invalid type for expression result: expected either `" & $typeof(OutTree) &
             "` or " & "`Option[" & $typeof(OutTree) &
             "]` in `mapItTreeDfs` on line " & $`pos` & ", but `op` is " &
            $typeof(foldRes)
          )

        if stack.len == 1:
          when foldRes is Option[OutTree]:
            res = foldRes.get() # NOTE
          else:
            res = foldRes

          break dfsLoop
        else:
          inc stack.last.idx
          when foldRes is Option[OutTree]:
            if foldRes.isSome():
              stack.last.subt.add foldRes.get()
          else:
            stack.last.subt.add foldRes

      else:
        stack.add makeDfsFrame[InTree, OutTree](
          block:
            let it {.inject.} = stack.last.inSubt[stack.last.idx]
            if hasSubnodes:
              subnodeCall
            else:
              var tmp: seq[InTree]
              tmp
          ,
          stack.last.path & @[stack.last.idx]
        )

  res

macro mapItDFS*(
  inTree: untyped,
  subnodeCall: untyped,
  outType: untyped,
  op: untyped,
  hasSubnodes: untyped = true): untyped =

  ##[
Convert one tree type into another. Conversion is perfomed in
bottom-up manner - first child nodes are evaluated, then results are
supplied to parent nodes and so on.

## Parameters

:subnodeCall: Expression to get child nodes
:outType: Result type
:inTree: Tree to convert
:op: Expression for converting objects.

## Injected variables

:it: current tree node
:path: path of the current node in original tree
:subt: already converted subnodes
:inSubt: current input subnodes

## Example

For examples of use look into `tests/tHalgorithm.nim`, 'Tree mapping
suite'.

  ]##


  result = quote do:
    mapItDFSImpl[typeof(`inTree`), `outType`](
      `inTree`,
      `subnodeCall`,
      `op`,
      `hasSubnodes`
    )

  # echo result.toStrLit()

import tables, strutils

proc longestCommonSubsequence*[T](x, y: seq[T]): seq[T] =
  # TODO Multiple subsequences
  # TODO Weighted subsequences
  var mem: CountTable[(int, int)]
  proc lcs(i, j: int): int =
    if (i, j) notin mem:
      mem[(i, j)] =
        if i == -1 or j == -1:
          0
        elif x[i] == y[j]:
          lcs(i - 1, j - 1) + 1
        else:
          max(
            lcs(i, j - 1),
            lcs(i - 1, j)
          )

    mem[(i, j)]

  let
    m = x.len - 1
    n = y.len - 1

  proc backtrack(i, j: int): seq[T] =
    if i == 0:
      @[x[i]]
    elif j == 0:
      @[y[j]]
    elif x[i] == y[j]:
      backtrack(i - 1, j - 1) & @[x[i]]
    elif lcs(i, j - 1) > lcs(i - 1, j):
      backtrack(i, j - 1)
    else:
      backtrack(i - 1, j)

  result = backtrack(m, n)


proc longestCommonSubsequence*[T](x, y: openarray[T]): seq[T] =
  longestCommonSubsequence(toSeq(x), toSeq(y))


template echov*(other: varargs[string, `$`]): untyped =
  let pref = "| ".repeat(getStackTraceEntries().len)
  echo pref, other.join(" ")

proc fuzzyMatchRecursive[Seq, Item](
  patt, other: Seq,
  pattIdx, otherIdx: int8,
  recLevel, maxRec: int,
  succStart: var int, matches: var seq[int8],
  cmpEq: proc(lhs, rhs: Item): bool,
  scoreFunc: proc(patt, other: Seq, matches: seq[int]): int
    ): tuple[ok: bool, score: int] =


  var otherIdx = otherIdx
  var pattIdx = pattIdx

  if (recLevel > maxRec) or
     (pattIdx == patt.len) or
     (otherIdx == other.len):
    result.ok = false
    return result

  var hadRecursiveMatch: bool = false
  var bestRecursiveScore: int = 0
  var bestRecursiveMatches: seq[int8]

  var succMatchBuf: seq[int8] = matches
  while (pattIdx < patt.len) and (otherIdx < other.len):
    if cmpEq(patt[pattIdx], other[otherIdx]):
      let recRes = fuzzyMatchRecursive(
        patt,
        other,
        pattIdx,
        otherIdx + 1,
        recLevel + 1,
        maxRec,
        succStart,
        succMatchBuf,
        cmpEq,
        scoreFunc
      )

      # echov &"Recursive test score: {recRes.score}, current: {bestRecursiveScore}"
      if (not hadRecursiveMatch) or (recRes.score > bestRecursiveScore):
        # echov &"Updated best recursive score, sub buf: {succMatchBuf}"
        bestRecursiveScore = recRes.score
        bestRecursiveMatches = succMatchBuf

      matches[pattIdx] = otherIdx
      succMatchBuf[pattIdx] = otherIdx

      hadRecursiveMatch = true

      # echov &"Has match on idx: {otherIdx}, patt: {pattIdx}, matches: {matches}"
      inc pattIdx

    inc otherIdx


  let fullMatch: bool = (pattIdx == patt.len)
  let currentScore = scoreFunc(patt, other, matches.mapIt(it.int))
  # echov &"Score: {currentScore}, matches: {matches}, best rec: {bestRecursiveScore} {bestRecursiveMatches}"

  # echov &"Full match: {fullMatch}, {pattIdx} == {patt.len}"
  if fullMatch:
    result.score = currentScore

  if hadRecursiveMatch and (not fullMatch or (bestRecursiveScore > currentScore)):
    # echov &"Recursive had better results: {bestRecursiveScore} > {currentScore}"
    result.score = bestRecursiveScore
    result.ok = true
    matches = bestRecursiveMatches
    # echov &"Assign to matches: {matches}"
    # echo &"Recursive match has better results: {bestRecursiveMatches}"
  elif fullMatch:
    # echov "Full match completed"
    # echov &"Full match results: {matches}"
    result.ok = true
  else:
    # echov &"Else"
    result.ok = false



proc fuzzyMatchImpl[Seq, Item](
  patt, other: Seq,
  matchScore: proc(patt, other: Seq, matches: seq[int]): int
                  ): tuple[ok: bool, score: int, matches: seq[int]] =
  ## Perform fuzzy matching of `other` agains `patt`. Return `score` -
  ## how similar two sequences are and `matches` - indices for which
  ## other matches pattern.
  var matchBuf: seq[int8] = newSeqWith(patt.len, 0.int8)
  var succStart = 0
  # echov &"Calling recursive implementation: input buffer {matchBuf}"
  let recMatch = fuzzyMatchRecursive[Seq, Item](
    patt = patt,
    other = other,
    pattIdx = 0,
    otherIdx = 0,
    recLevel = 0,
    maxRec = 10,
    succStart = succStart,
    matches = matchBuf,
    cmpEq = (proc(lhs, rhs: Item): bool = (lhs == rhs)),
    scoreFunc = matchScore
  )

  # echov &"Finished recursive implementation, buffer: {matchBuf}"

  return (
    ok: recMatch.ok,
    score: recMatch.score,
    matches: matchBuf.mapIt(it.int)
  )

proc fuzzyMatch*[T](
  patt, other: openarray[T],
  matchScore: proc(patt, other: openarray[T], matches: seq[int]): int
                 ): tuple[ok: bool, score: int, matches: seq[int]] =
  fuzzyMatchImpl[openarray[T], T](patt, other, matchScore)


proc fuzzyMatch*(
  patt, other: string,
  matchScore: proc(patt, other: string, matches: seq[int]): int
                 ): tuple[ok: bool, score: int, matches: seq[int]] =
  fuzzyMatchImpl[string, char](patt, other, matchScore)
