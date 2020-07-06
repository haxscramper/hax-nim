import hmisc/hterms_callback

# import hmisc/hterms_tree

import sequtils, strformat, strutils
import hmisc/halgorithm

import unittest

type
  AstKind = enum
    # Constant values
    akStrLit
    akIntLit
    akIdent

    # Functors
    akCall
    akCondition

  Ast = object
    case kind: AstKind
      of akStrLit, akIdent:
        strVal: string
      of akIntLit:
        intVal: int
      else:
        sons: seq[Ast]

proc `==`(lhs, rhs: Ast): bool =
  lhs.kind == rhs.kind and
  (
    case lhs.kind:
      of akStrLit, akIdent: lhs.strVal == rhs.strVal
      of akIntLit: lhs.intVal == rhs.intVal
      else: subnodesEq(lhs, rhs, sons)
  )

type AstTerm = Term[Ast, AstKind]
func nOp(op: AstKind, subt: seq[AstTerm]): AstTerm =
  case op:
    of akStrLit .. akIdent:
      assert false
    else:
      return makeFunctor[Ast, AstKind](op, subt)

func nVar(n: string): AstTerm =
  makeVariable[Ast, AstKind](n)

func mkOp(op: AstKind, sub: seq[Ast]): Ast =
  case op:
    of akCall, akCondition:
      Ast(kind: op, sons: sub)
    else:
      raiseAssert("12")

func mkVal(val: int): Ast = Ast(kind: akIntLit, intVal: val)
func mkIdent(val: string): Ast = Ast(kind: akIdent, strVal: val)
func mkStrLit(val: string): Ast = Ast(kind: akStrLit, strVal: val)

func nConst(n: Ast): AstTerm = makeConstant[Ast, AstKind](n)


suite "Hterms ast rewriting":
  test "Ast rewriting":
    let cb = TermImpl[Ast, AstKind](
      getFsym: (proc(n: Ast): AstKind = n.kind),
      isFunctor: (proc(n: Ast): bool = n.kind in {akCall .. akCondition}),
      makeFunctor: (proc(op: AstKind): Ast = Ast(kind: op)),
      getSubt: (proc(n: Ast): seq[Ast] = n.sons),
      setSubt: (proc(n: var Ast, sub: seq[Ast]) = n.sons = sub),
      valStrGen: (proc(n: Ast): string = "[[ TODO ]]"),
    )

    let rSystem = RedSystem[Ast, AstKind](rules: @[
      makeRulePair(
        nOp(akCall, @[
          mkIdent("someFunc").nConst(), mkVal(9000).nConst()
        ]).makePattern(),
        nConst(mkStrLit("Hello 9000")).makeGenerator()
    )])

    let obj =  mkOp(akCall, @[ mkIdent("someFunc"), mkVal(9000) ])
    let res = reduce(obj.toTerm(cb), rSystem)
    if res.ok:
      let resAst = res.term.fromTerm(cb)
      echo resAst
      assert resAst == Ast(kind: akStrLit, strVal: "Hello 9000")
    else:
      echo "res not ok"
