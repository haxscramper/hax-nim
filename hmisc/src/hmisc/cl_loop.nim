import macroutils
import hmisc/helpers
import tables
import sequtils
import macros
import strformat
import sugar
import strformat
import strutils
import os

import colechopkg/types
export types

## Implementation of common lisp's loop macro

proc nthLine(file: string, line: int): string =
  readLines(file, line)[line - 1]

proc highlightNode*(info: LineInfo, length: int, message: string): void =
  let (dir, name, ext) = info.filename.splitFile()
  let position = &"{name}{ext} {info.line}:{info.column} "
  echo position, nthLine(info.filename, info.line)
  let padding = " ".repeat(position.len + info.column)
  echo padding, "^".repeat(length).toRed()
  echo padding, message

template iterValType*(arg: untyped): untyped =
  when compiles(arg[0]):
    typeof(arg[0])
  else:
    typeof(arg)

template quitAssert*(cond: untyped, body: untyped): untyped =
  if not cond:
    body
    quit(1)

proc makeClosureIteratorDef(iter: NimNode): NimNode =
  nnkIteratorDef.newTree(
    Empty(),
    Empty(),
    Empty(),
    nnkFormalParams.newTree(
      nnkCall.newTree(
        newIdentNode("iterValType"),
        iter
      )
    ),
    Empty(),
    Empty(),
    nnkStmtList.newTree(
      nnkForStmt.newTree(
      newIdentNode("val"),
      iter,
      nnkStmtList.newTree(
          nnkYieldStmt.newTree(
            newIdentNode("val")
          )
        )
      )
    )
  )

proc foldlInfix(nodes: seq[NimNode], inf: NimNode): NimNode =
  result = nodes.foldl(nnkInfix.newTree(
    inf, nnkPar.newTree(a), nnkPar.newTree(b)))

proc substituteEnv(expr: NimNode, env: seq[(NimNode, NimNode)]): NimNode =
  ## Recursively substitue all occurrenices of variables from `env` to
  ## corresponding expressions.
  let idx = env.findIt(it[0] == expr)
  if idx != -1:
    result = env[idx][1]
    result = quote do: (`result`)
  else:
    case expr.kind:
      of nnkIdent, nnkStrLit:
        return expr
      else:
        result = newTree(
          expr.kind,
          toSeq(expr.children()).mapIt(it.substituteEnv(env))
        )


macro loop*(body: untyped): untyped =
  defer: echo result.toStrLit()

  result = quote do: 1
  let stmts = toSeq(body.children()).filterIt(it.kind != nnkEmpty)

  proc iterNode(v: NimNode): NimNode =
    ident("iterFor__" & $v)

  let forStmts = stmts.filterIt(it.kind == nnkCommand and it[0].eqIdent("lfor"))
  let forIters = collect(newSeq):
    for fs in forStmts:
      let varn = fs[1][1]
      let iter = fs[1][2]

      let iterDecl = makeClosureIteratorDef(iter)
      (
        vsym: varn,
        iter: iterDecl,
        itbody: iter,
        itersym: iterNode(varn)
      )

  let decls = collect(newSeq):
    for decl in forIters:
      superQuote do:
        let `decl.itersym` = `decl.iter`


  let collectStmts = stmts.filterIt(it.kind == nnkCommand and it[0].eqIdent("lcollect"))

  let typeExprs = collect(newSeq):
    for coll in collectStmts:
      coll[1].substituteEnv(forIters.mapIt((
        it[0], Call(iterNode(it[0])))))

  let resType = superQuote do: typeof(`typeExprs[0]`)

  let resSeq = superQuote do:
    var ress {.inject.}: seq[`resType`]

  let breakCondition = forIters.mapIt(
    Call("finished", it.itersym)
  ).foldlInfix(ident "or")

  let varAssgns = collect(newSeq):
    for decl in forIters:
      superQuote do:
        let `decl.vsym` = `decl.itersym`()

  let endStmt =
    if collectStmts.len > 0:
      quote do: ress
    else:
      Empty()

  let collectCalls = collect(newSeq):
    for coll in collectStmts:
      superQuote do:
        ress.add(`coll[1]`)

  let typeAsserts = collect(newSeq):
    for idx, coll in collectStmts:
      let info = collectStmts[idx].lineInfoObj()
      let nodeLen = ($collectStmts[idx][1].toStrLit()).len().newIntLitNode()
      superQuote do:
        quitAssert (typeof(`typeExprs[idx]`)) is `resType`:
          echo "\nType mismatch on `lcollect` expression\n"
          highlightNode(
            info = LineInfo(
              filename: `info.filename.newStrLitNode()`,
              line: `info.line.newIntLitNode()`,
              column: `info.column.newIntLitNode()`
            ),
            length = `nodeLen`,
            message =
              "Has type `" & $typeof(`typeExprs[idx]`) &
                "` but expected `" & $typeof(`resType`) & "`"
          )

  result = superQuote do:
    block:
      `newStmtList(decls)`
      `resSeq`

      static:
        `typeAsserts.newStmtList()`

      while true:
        `varAssgns.newStmtList()`
        if `breakCondition`:
          break

        `collectCalls.newStmtList()`

      `endStmt`
