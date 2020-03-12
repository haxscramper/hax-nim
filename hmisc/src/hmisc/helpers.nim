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
  if opt.isNone: false
  else: opt.get() == val

proc `==`*[A, B](tpl: (Opt[A],Opt[B]), tpl1: (A,B)): bool =
  tpl[0] == tpl1[0] and tpl[1] == tpl1[1]

converter toOption*[T](t: T): Option[T] =
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
    if predicate: tBranch
    else: fBranch

macro quoteDoInterpolStmt*(body: untyped): untyped =
  ## Allows to interpolate function call into `quote do` body.
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
  tbl: seq[tuple[k: seq[K], v: V]]):
    Option[V] =
  for tupl in tbl:
    if val in tupl.k:
      return some(tupl.v)

    result = none(V)

proc echoi*(indent: int, message: varargs[string, `$`]): void =
  echo "  ".repeat(indent), message.join(" ")

proc echoi*(message: varargs[string, `$`]): void =
  echo message.join(" ")

proc joinl*(inseq: openarray[string]): string = inseq.join("\n")
proc joinw*(inseq: openarray[string]): string = inseq.join(" ")
proc joinq*(inseq: openarray[string]): string = inseq.mapIt("\"" & it & "\"").join(" ")

proc replaceN*(str: string, n: int, subst: char = ' '): string =
  result = str
  for i in 0..<min(str.len, n):
    result[i] = subst


proc enumerate*[T](s: openArray[T]): seq[(int, T)] =
  for idx, item in s:
    result.add((idx, item))


proc printTwoColumns*(
  text: seq[(string, string)],
  padding: (int, int) = (0,0),
  widthColLimits: (int, int) = (30, -1),
  maxWidthTotal: int = 80): void =

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
          lines1[idx].alignLeft(2),
          " ".repeat(maxWidth1 + 1)
        ),
        (idx < lines2.len).tern(lines2[idx], "")]




proc enclosedIn*(
  str: string,
  delim: tuple[left, right: string]):
    bool =
  return str.startsWith(delim.left) and
    str.endsWith(delim.right)

proc getRandomBase64*(length: int): string =
  newSeqWith(
    length,
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".
    sample()).join("")

macro ifLet*(head: untyped, bodies: varargs[untyped]): untyped =
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

  for a in 1..3:
    echo a

  let final =
    block:
      iflet (val = none(int)):
        echo "none is something and it has value of ", val
        3
      elif 2 == 3:
        echo "pretty good at math?"
        5
      else:
        echo "not really"
        1


  echo "and final value is ", final
