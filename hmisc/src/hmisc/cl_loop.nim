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

proc makeIterFor(v: NimNode): NimNode =
  ident("iterFor__" & $v)

type
  LoopStmtKind = enum
    lskCollect
    lskMax
    lskMin

    lskAllOf
    lskNoneOf
    lskAnyOf

  LoopStmts = object
    expression: seq[NimNode] ## Expressions of statement arguments
    kind: LoopStmtKind ## Kind of expression
    case genVal: bool ## Whether or not this expression adds value to
                      ## result type
    of true:
      tag: int
    of false:
      nil


  LoopGenConfig = object
    case retTuple: bool ## Single or multiple value return
      of true:
        flds: seq[tuple[name, ftype: string]] ## | Multiple
        ## value return. List of tuple field names along with return
        ## types.
        # List of name/ftype pairs is generated alter processing macro
        # body. In statement like `lcoll out, i.name` first argument
        # to lcoll corresponds to `name` in the pair. Generated type
        # will have name `Type<Name>`. In this example tuple will be:
        # `(name: "out", type: "TypeOut")`
      of false:
        rtype: NimNode ## |
        ## Single value return

    defType: Opt[NimNode] ## Default return type, if any

proc makeResType(stmts: seq[LoopStmtKind], conf: LoopGenConfig): tuple[
  typeDecl, typeName: NimNode] =
  ## Generate statement for declaring loop return type and expression to
  # IDEA Generate name of the return type based on loop init location
  if conf.defType.isSome():
    # TODO generate static type checking assertions
    result.typeDecl = superQuote do:
      type LoopResult = `conf.defType.get()`
  else:
    # Calcuate return type based on loop statements in body.
    discard

proc makeIterators(stmts: seq[NimNode]): tuple[
  iterDecl, typeDecl: NimNode] =
  ## Create declaration of iterators and type declarations

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
        itersym: makeIterFor(varn)
      )

  let decls = collect(newSeq):
    for decl in forIters:
      superQuote do:
        let `decl.itersym` = `decl.iter`


  let collectStmts = stmts.filterIt(it.kind == nnkCommand and it[0].eqIdent("lcoll"))
  let typeExprs = collect(newSeq):
    for coll in collectStmts:
      coll[1].substituteEnv(forIters.mapIt((
        it[0], Call(makeIterFor(it[0])))))

  return (iterDecl: decls.newStmtList(), typeDecl: typeExprs.newStmtList())

macro loop*(arg, body: untyped): untyped =
  let stmts = toSeq(body.children()).filterIt(it.kind != nnkEmpty)
  let (iterDecls, typeDecls) = makeIterators(stmts)
  let collectStmts = stmts.filterIt(it.kind == nnkCommand and it[0].eqIdent("lcoll"))
  let resType = superQuote do: typeof(`typeDecls[0]`)

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


macro loop1*(body: untyped): untyped =
  quote do:
    loop((), `body`)
