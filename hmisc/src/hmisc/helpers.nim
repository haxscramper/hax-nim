import options, sequtils
import random
import std/wordwrap
import strutils
import macros
import deques
import posix
import os


export options

#~#=== Optional type
type Opt*[T] = Option[T]

proc `==`*[T](opt: Opt[T],val: T): bool =
  ## Compare option with value for equatilty
  if opt.isNone: false
  else: opt.get() == val

proc `==`*[A, B](tpl: (Opt[A],Opt[B]), tpl1: (A,B)): bool =
  ## Compare tuple of optional values for equality
  tpl[0] == tpl1[0] and tpl[1] == tpl1[1]

converter toOption*[T](t: T): Option[T] =
  ## Implicit convertion of values to option types
  when T is ref:
    if t.isNil:
      return none(T)
    return some(t)
  else:
    return some(t)

template tern*(
  predicate: bool,
  tBranch: untyped,
  fBranch: untyped): untyped =
    ## Shorthand for inline if/else.
    runnableExamples:
      let a = (1 == 2).tern("99", "0-")
      assert a == "0-"

    if predicate: tBranch
    else: fBranch

macro quoteDoInterpolStmt*(body: untyped): untyped =
  ## Allows to interpolate function call into `quote do` body. Literal
  ## strings surrounded by backticks will be replaced by function calls.
  runnableExamples:
    import macros
    macro test(): untyped = quote do: 123

    # With interpol stmt
    macro macrotest(): untyped =
      result = quoteDoInterpolStmt:
        `"test()"`

    macro macrotest2(): untyped =
      let sub = test()
      result = quote do:
        `sub`

    assert macrotest() == 123
    assert macrotest2() == macrotest()

  # TODO add documentaion
  # TODO refactor to two simpler functions
  var stmts: seq[tuple[i: int, node: NimNode]]
  var newBody: seq[NimNode]

  for idx, stmt in body:
    case stmt.kind
    of nnkAccQuoted:
      let parsed: NimNode = stmt[0].toStrLit.strVal.parseExpr
      if parsed.kind == nnkCall:
        stmts.add((i: idx, node: parsed))
        let newStmt = ident("tmp" & $idx)
        newBody.add(
          nnkAccQuoted.newTree(newStmt))
      else:
        newBody.add(stmt)

    of nnkLetSection, nnkVarSection:
      if stmt[0][2].kind == nnkAccQuoted:
        let parsed = stmt[0][2][0].toStrLit.strVal.parseExpr
        stmts.add((idx, parsed))

        let lhs = stmt[0][0]
        let typ = stmt[0][1]
        let rhs = nnkAccQuoted.newTree(ident("tmp" & $idx))
        let newAssgn = newTree(
          stmt.kind,
          nnkIdentDefs.newTree(lhs, typ, rhs))
        newBody.add(newAssgn)
      else:
        newBody.add(stmt)

    else:
      newBody.add(stmt)


  let lets: NimNode = nnkStmtList.newTree(
    stmts.mapIt(
      nnkLetSection.newTree nnkIdentDefs.newTree(
        newIdentNode("tmp" & $it.i),
        newIdentNode("NimNode"),
        it.node)))

  let newBodyStmts = newBody.newStmtList()

  let resBlock = quote do:
    block:
      `lets`
      quote do:
        `newBodyStmts`

  # echo toStrLit resBlock
  result = resBlock



proc testEq*[A, B](lhs: A, rhs: B) =
  if lhs != rhs:
    echo "Test failed"
    echo "lhs: ", lhs
    echo "rhs: ", rhs
    echo ""


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

proc echoi*(indent: int, message: varargs[string, `$`]): void =
  ## Echo with indentation. `message` is joined using spaces
  echo "  ".repeat(indent), message.join(" ")

proc echoi*(message: varargs[string, `$`]): void =
  ## Echo with message joined by spaces
  echo message.join(" ")

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

proc joinq*(inseq: openarray[string]): string =
  ## Join items using spaces and quote each item
  runnableExamples:
    assert @["as", "qq"].joinq == "\"as\" \"qq\""

  inseq.mapIt("\"" & it & "\"").join(" ")

proc replaceN*(str: string, n: int, subst: char = ' '): string =
  ## Replace first `n` characters in string with `subst`
  runnableExamples:
    assert "123".replaceN(1) == " 23"
    assert "0--".replaceN(3, '-') == "---"

  result = str
  for i in 0..<min(str.len, n):
    result[i] = subst


proc enumerate*[T](s: openArray[T]): seq[(int, T)] =
  ## Return enumerated sequence of items
  runnableExamples:
    assert @["cat", "dog"].enumerate() == @[(0, "cat"), (1, "dog")]

  for idx, item in s:
    result.add((idx, item))

proc printTwoColumns*(
  text: seq[(string, string)],
  padding: (int, int) = (0,0),
  widthColLimits: (int, int) = (30, -1),
  maxWidthTotal: int = 80): void =
  ##[
  Print two columns of text side by side

  :padding: amount of spaces from left and right

  :maxWidthNotal: max width of two columns plus padding

  :widthColLimits: limit of each column width

  :text: sequence of string pairs. Each pair will be printed on new
  row

  .. code-block::
      @[
        ("=first line=", "=second="),
        ("=aaaaaaaaaaaaaaaaaa=", "=sd d fd fd ="),
        ("=a d d d aaaaaaaaaaaaaaaaa=", "=sd d fd fd ="),
        ("q", "=sd d fd fd =")
      ].printTwoColumns()

  .. code-block:: text
      =first line=                 =second=
      =aaaaaaaaaaaaaaaaaa=         =sd d fd fd =
      =a d d d aaaaaaaaaaaaaaaaa=  =sd d fd fd =
      q                            =sd d fd fd =
  ]##

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
      echo " $# $#" % [
        (idx < lines1.len)
        .tern(
          lines1[idx].alignLeft(maxWidth1 + 1),
          " ".repeat(maxWidth1 + 1)
        ),
        (idx < lines2.len).tern(lines2[idx], "")]




proc enclosedIn*(
  str: string,
  delim: tuple[left, right: string]): bool =
  ## Check if string starts and ends with strings.
  runnableExamples:
    assert "--hello--".enclosedIn(("--", "--"))
  return str.startsWith(delim.left) and
    str.endsWith(delim.right)

proc getRandomBase64*(length: int): string =
  ## Return random base 64 string with `length` characters
  newSeqWith(
    length,
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".
    sample()).join("")

macro ifLet*(head: untyped, bodies: varargs[untyped]): untyped =
  ##[ Unwrap optional value into variable if it contains something.

  .. code-block::
      let final = block:
        iflet (val = none(int)):
          3
        elif 2 == 3:
          5
        else:
          1

      assert final == 1
  ]##

  assert head.kind == nnkPar
  assert head[0].kind == nnkAsgn

  # defer:
  #   echo result.toStrLit()

  let varSymbol = head[0][0]
  let varValue = head[0][1]

  let ifBody = bodies[0]

  var condBranch = newIfStmt(
    ((quote do: optValue.isSome()),
     quote do:
       let `varSymbol` = optValue.get()
       `ifBody`))

  for body in bodies[1..^1]:
    condBranch.add body

  result = quote do:
    block:
      let optValue {.inject.} = `varValue`
      `condBranch`

func splitList*[T](s: openarray[T]): (T, seq[T]) =
  ## Return head and tail of the list
  assert s.len > 0
  (s[0], s[1..^1])

when isMainModule:


  block:
    let (head, tail) = @[1].splitList()
    doAssert head == 1
    doAssert tail.len == 0

  block:
    let (head, tail) = @[1,2].splitList()
    doAssert head == 1
    doAssert tail == @[2]

  iflet (val = none(int)):
    echo "none is something and it has value of ", val

  @[
    ("=first line=", "=second="),
    ("=aaaaaaaaaaaaaaaaaa=", "=sd d fd fd ="),
    ("=a d d d aaaaaaaaaaaaaaaaa=", "=sd d fd fd ="),
    ("q", "=sd d fd fd =")
  ].printTwoColumns()

template anyofIt*(sequence: typed, predicate: untyped): bool =
  ## Return `true` if for any of the items in sequence `predicate`
  ## evaluates as `true`. Otherwise return false.
  var result = false
  for it {.inject.} in sequence:
    if predicate:
      result = true

  result

proc max*[T](x: openArray[T], default: T): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  ## use `default` as starting value for comparison.
  result = default
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
