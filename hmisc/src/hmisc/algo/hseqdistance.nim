## Sequence distance metrics

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
