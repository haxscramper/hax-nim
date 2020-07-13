import math, strutils, sequtils, random, macros, options
import std/wordwrap
import ../types/hprimitives

#=======================  small helper templates  ========================#

template expectEqualTypes(a, b: untyped): untyped =
  assert (a is typeof(b)), "Mismatch between types: first is `" &
    $typeof(a) & "` and second is `" & $typeof(b) & "`"

template tern*(
  predicate: bool,
  tBranch: untyped,
  fBranch: untyped): untyped =
    ## Shorthand for inline if/else.
    runnableExamples:
      let a = (1 == 2).tern("99", "0-")
      assert a == "0-"

    block:
      # static: expectEqualTypes(tBranch, fBranch)
      if predicate: tBranch
      else: fBranch

template orElse*(
  value: untyped, predicate: bool, fallback: untyped): untyped =
  if predicate: value
  else: fallback

template setIf*(lhs: untyped, predicate: bool, value: untyped): untyped =
  if predicate: lhs = value


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

macro disjointIterImpl(x: typed): untyped =
  var values: seq[NimNode]
  for value in x.getTypeImpl[1..^1]:
    values.add newIdentNode($value.tostrlit)

  result = nnkStmtList.newTree(
    nnkPrefix.newTree(
      newIdentNode("@"),
      nnkBracket.newTree(values)))

macro disjointIter*(x: typed): untyped =
  nnkBracket.newTree(x.getType[1][1..^1])


#========================  sequence operations  ==========================#

func emptySeq*[T](): seq[T] = discard
proc enumerate*[T](s: openArray[T]): seq[(int, T)] =
  ## Return enumerated sequence of items
  for idx, item in s:
    result.add((idx, item))

func splitList*[T](s: openarray[T]): (T, seq[T]) =
  ## Return head and tail of the list
  assert s.len > 0, "Cannot split empty list"
  (s[0], s[1..^1])

func sumjoin*(a: openarray[int], sep: int): int =
  a.sum() + (a.len > 0).tern((a.len() - 1) * sep, 0)


func cumsumjoin*(
  a: openarray[int], sep: int, addFirst: bool = false): seq[int] =
  var curr: int = 0
  if addFirst:
    result.add curr

  for val in a:
    result.add(val + sep + curr)
    curr += val + sep

func sumjoin*(a: openarray[int], r: ArrRange, sep: int): int =
  sumjoin(a[r.a .. r.b], sep)

iterator `..+`*(start: int, offset: int): int =
  for i in start ..< start + offset:
    yield i

func order*[Num](a, b: Num): (Num, Num) =
  if a > b: (b, a)
  else: (a, b)

template last*[T](stack: var seq[T]): var T = stack[^1]
template last*[T](stack: seq[T]): T = stack[^1]

func modiv*(a, b: int): tuple[val, rem: int] =
  result.val = a div b
  result.rem = a mod b

proc max*[T](x: openArray[T], default: T): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  ## use `default` if array is empty
  if x.len == 0:
    result = default
  else:
    for i in x:
      if result < i: result = i

# TODO use static hashtable instead of searching whole list each time.
proc matchWith*[K, V](
  val: K,
  tbl: seq[tuple[k: seq[K], v: V]]): Option[V] =
  ## Search `seq[seq[Key], Val]` for entry that has matching `Key` and
  ## return corresponding `Val`. If nothing found return `none(V)`
  runnableExamples:
    let lookup = @[
      (@["one", "two", "three"], "number"),
      (@["cat", "dog", "mole"], "animal")
    ]

    assert "one".matchWith(lookup) == some("number")
    assert "dog".matchWith(lookup) == some("animal")
    assert "number".matchWith(lookup).isNone()


  for tupl in tbl:
    if val in tupl.k:
      return some(tupl.v)

    result = none(V)

#=======================  single item operations  ========================#

func setMax*[T](a: var T, b: T): void =
  if a < b:
    a = b


proc nthType1*[T1, T2](a: (T1, T2)): T1 =
  ## Helper proc to get first type from tuple. Used as workaround for
  ## `pairs` iterator
  discard

proc nthType2*[T1, T2](a: (T1, T2)): T2 =
  ## Helper proc to get second type from tuple. Used as workaround for
  ## `pairs` iterator
  discard

#=========================  string operations  ===========================#

func posString*(node: NimNode): string =
  let info = node.lineInfoObj()
  return "on line " & $info.line

proc mismatchStart*(str1, str2: string): int =
  ## Find position where two strings mismatch first
  # TODO implement mismatch with support for multiple
  # matching/mismatching sections - use larges common subsequence to
  # determine differences

  # NOTE can use annotation highlighter from code error reporting
  # `hmisc/defensive`

  # TODO support multiline strings (as sequence of strigns and as
  # single multiline strings)
  for i in 0 ..< min(str1.len(), str2.len()):
    if str1[i] != str2[i]:
      return i

  if str1.len() != str2.len():
    # Have common prefix but second one is longer
    return min(str1.len(), str2.len()) + 1
  else:
    # No mismatch found
    return -1

proc joinl*(inseq: openarray[string]): string =
  ## Join items using newlines
  runnableExamples:
    assert @["as", "bn"].joinl == "as\nbn"
  inseq.join("\n")

proc joinw*(inseq: openarray[string]): string =
  ## Join items using spaces
  runnableExamples:
    assert @["as", ";;"].joinw == "as ;;"
  inseq.join(" ")

proc joinq*(inseq: openarray[string], sep: string = " ", wrap: string = "\""): string =
  ## Join items using spaces and quote each item
  runnableExamples:
    assert @["as", "qq"].joinq == "\"as\" \"qq\""

  inseq.mapIt(wrap & it & wrap).join(sep)

proc replaceN*(str: string, n: int, subst: char = ' '): string =
  ## Replace first `n` characters in string with `subst`
  runnableExamples:
    assert "123".replaceN(1) == " 23"
    assert "0--".replaceN(3, '-') == "---"

  result = str
  for i in 0..<min(str.len, n):
    result[i] = subst

proc wrapTwoColumns*(
  text: seq[(string, string)],
  padding: (int, int) = (0,0),
  widthColLimits: (int, int) = (30, -1),
  maxWidthTotal: int = 80): seq[(string, string)] =

  var wrapped: seq[(seq[string], seq[string])] =
    text.mapIt(
      (it[0].wrapWords(widthColLimits[0]).split("\n"), @[it[1]]))

  let maxWidth1: int =
    wrapped
    .mapIt(
      it[0]
      .mapIt(it.len)
      .max()
    )
    .max()

  let maxWidth2: int =
    if widthColLimits[1] > 0:
      widthColLimits[1]
    else:
      maxWidthTotal - min(maxWidth1, widthColLimits[0])


  wrapped = wrapped.mapIt(
    (it[0],
     it[1][0].wrapWords(maxWidth2).split("\n")))

  for entry in wrapped:
    let lines1 = entry[0]
    let lines2 = entry[1]

    let lineCount = max(lines1.len, lines2.len)
    for idx in 0 ..< lineCount:
      # echo " $# $#" % [
        result.add (
          (idx < lines1.len)
          .tern(
            lines1[idx].alignLeft(maxWidth1 + 1),
            " ".repeat(maxWidth1 + 1)
          ),
                  (idx < lines2.len).tern(lines2[idx], "")
        )
      # ]


proc printTwoColumns*(
  text: seq[(string, string)],
  padding: (int, int) = (0,0),
  widthColLimits: (int, int) = (30, -1),
  maxWidthTotal: int = 80): void =
  ## Print two columns of text side by side
  ##
  ## :params:
  ##   :padding: amount of spaces from left and right
  ##   :maxWidthNotal: max width of two columns plus padding
  ##   :widthColLimits: limit of each column width
  ##   :text: sequence of string pairs. Each pair will be printed on new
  ##          row
  ##
  ## .. code-block::
  ##     @[
  ##       ("=first line=", "=second="),
  ##       ("=aaaaaaaaaaaaaaaaaa=", "=sd d fd fd ="),
  ##       ("=a d d d aaaaaaaaaaaaaaaaa=", "=sd d fd fd ="),
  ##       ("q", "=sd d fd fd =")
  ##     ].printTwoColumns()
  ##
  ## .. code-block:: text
  ##     =first line=                 =second=
  ##     =aaaaaaaaaaaaaaaaaa=         =sd d fd fd =
  ##     =a d d d aaaaaaaaaaaaaaaaa=  =sd d fd fd =
  ##     q                            =sd d fd fd =

  for (lhs, rhs) in wrapTwoColumns(text, padding, widthColLimits, maxWidthTotal):
    echo " $# $#" % [lhs, rhs]

proc join*(text: openarray[(string, string)], sep: string = " "): string =
  text.mapIt(it[0] & it[1]).join(sep)

func join*(text: openarray[string], sep: char = ' '): string =
  text.join($sep)

func enclosedIn*(
  str: string,
  delim: tuple[left, right: string]): bool =
  ## Check if string starts and ends with strings.
  return str.startsWith(delim.left) and
    str.endsWith(delim.right)


func wrap*(
  str: string,
  delim: tuple[left, right: string]): string =
  ## Check if string starts and ends with strings.
  return delim.left & str & delim.right


func escapeHTML*(input: string): string =
  input.multiReplace([
    (">", "&gt;"),
    ("<", "&lt;"),
    ("&", "&amp;"),
    ("\"", "&quot;")
  ])

func enclosedIn*(s: string, delim: string): bool =
  s.enclosedIn((delim, delim))

proc getRandomBase64*(length: int): string =
  ## Return random base 64 string with `length` characters
  newSeqWith(
    length,
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".
    sample()).join("")

proc dedent*(multiline: string): string =
  ## Uniformly deindent multiline string
  let seplines = multiline.split('\n')
  var indent = 0
  for c in seplines[0]:
    if c == ' ': inc indent
    else: break

  seplines.mapIt(
    if it.len == 0:
      it
    else:
      assert it[0..<indent].allOfIt(it == ' '),
        "Cannot unindent non-whitespace character"

      it[indent..^1]
  ).join("\n")

#===============================  options  ===============================#
proc `==`*[T](opt: Option[T],val: T): bool =
  ## Compare option with value for equatilty
  if opt.isNone: false
  else: opt.get() == val

proc `==`*[A, B](tpl: (Option[A], Option[B]), tpl1: (A, B)): bool =
  ## Compare tuple of optional values for equality
  tpl[0] == tpl1[0] and tpl[1] == tpl1[1]

template ifSomeIt*[T](opt: Option[T], predicate: untyped): bool =
  opt.isSome() and ((let it {.inject.} = opt.get(); predicate))

#================================  tests  ================================#

import unittest
proc testEq*[A, B](lhs: A, rhs: B) =
  static:
    assert compiles(lhs == rhs),
     "Cannot directly compare objects of type" & $typeof(lhs) &
       " and " & $typeof(rhs)

  if lhs != rhs:
    let lhsStr = $lhs
    let rhsStr = $rhs

    testEnded(
      ConsoleOutputFormatter(colorOutput: true, isInSuite: true),
      TestResult(testName: "Equality comparison", status: FAILED)
    )

    let diffPos = mismatchStart(lhsStr, rhsStr)
    if '\n' in lhsStr or '\n' in rhsStr:
      let
        linesA = lhsStr.split('\n')
        linesB = rhsStr.split('\n')

      for idx, line in zip(linesA, linesB):
        if line[0] != line[1]:
          echo &"LHS #{idx}: '{line[0]}'"
          echo &"RHS #{idx}: '{line[1]}'"
          break
        # else:
        #   echo &"#{idx}: '{line[0]}' == '{line[1]}'"

    else:
      echo "LHS: ", lhsStr
      echo "RHS: ", rhsStr
      echo "    ", " ".repeat(diffPos), "^".repeat(rhsStr.len() - diffPos + 1)

    echo ""

template assertEq*(lhs, rhs: untyped): untyped =
  let lhsVal = lhs
  let rhsVal = rhs
  testEq(lhsVal, rhsVal)
  let lInfo = instantiationInfo()
  if not (lhsVal == rhsVal):
    raiseAssert("Comparison failed on line " & $lInfo.line)