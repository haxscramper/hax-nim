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

type
  ErrorAnnotation = object
    errpos*: LineInfo
    expr*: string
    annotation*: string

  CodeError* = ref object of CatchableError
    annots: seq[ErrorAnnotation]

proc nthLine(file: string, line: int): string =
  readLines(file, line)[line - 1]

proc highlightErr*(err: CodeError): void =

  echo "\n", err.msg, "\n"

  for err in err.annots:
    let (dir, name, ext) = err.errpos.filename.splitFile()
    let position = &"{name}{ext} {err.errpos.line}:{err.errpos.column} "
    let padding = " ".repeat(position.len + err.errpos.column)

    echo position, nthLine(err.errpos.filename, err.errpos.line)
    echo padding, "^".repeat(err.expr.len()).toRed()
    echo padding, err.annotation
    echo ""

template iterValType*(arg: untyped): untyped =
  when compiles(arg[0]):
    typeof(arg[0])
  else:
    typeof(arg)

proc codeAssert*(cond: bool, msg: string, annots: varargs[ErrorAnnotation]): void =
  if not cond:
    raise CodeError(
      annots: toSeq(annots),
      # line: info.line,
      # column: info.column,
      # filename: info.filename,
      # expr: expr,
      # annotation: annotation,
      msg: msg
    )


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
  # defer: echo result.toStrLit()

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
      let annots = collect(newSeq):
        for id in @[idx, 0]:
          let info = collectStmts[id].lineInfoObj()
          let nodeStr = $collectStmts[id][1].toStrLit()
          let nodeLen = (nodeStr).len().newIntLitNode()
          let annotation =
            if id != 0:
              superQuote do: ("Has type `" & $typeof(`typeExprs[idx]`) &
                "` but expected `" & $typeof(`resType`) & "`")
            else:
              superQuote do: ("Inferred type is `" & $typeof(`resType`) & "` ")

          superQuote do:
            annots.add ErrorAnnotation(
              annotation: `annotation`,
              errpos: LineInfo(
                  filename: `info.filename.newStrLitNode()`,
                  line: `info.line.newIntLitNode()`,
                  column: `info.column.newIntLitNode()`
              ),
              expr: `newStrLitNode(nodeStr)`
            )


      superQuote do:
        block:
          var annots {.inject.}: seq[ErrorAnnotation]
          `annots.newStmtList()`
          codeAssert(
            cond = (typeof(`typeExprs[idx]`) is `resType`),
            msg = "Type mismatch on `lcollect` expression",
            annots = annots
          )

  result = superQuote do:
    block:
      `newStmtList(decls)`
      `resSeq`

      static:
        try:
          `typeAsserts.newStmtList()`
        except CodeError:
          highlightErr(CodeError(getCurrentException()))
          quit(1)

      while true:
        `varAssgns.newStmtList()`
        if `breakCondition`:
          break

        `collectCalls.newStmtList()`

      `endStmt`
