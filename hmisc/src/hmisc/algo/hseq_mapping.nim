import sequtils, strutils, macros

import ../types/seq2d

#========================  sorting and filtering  ========================#

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

#===========================  transformation  ============================#

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

#==============================  searching  ==============================#

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
