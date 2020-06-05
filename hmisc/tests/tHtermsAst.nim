import hmisc/hterms_callback

import hmisc/hterms_tree

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

proc makeFunctor(kind: AstKind, sons: seq[Ast]): Ast =
  case kind:
    of akStrLit .. akIdent:
      assert false
    else:
      return Ast(kind: kind, sons: sons)

test "Ast rewriting":
  defineTermSystemFor[Ast, AstKind](
      kindField = kind,
      sonsField = sons,
      treeMaker = makeFunctor,
      implName = astImpl,
      functorKinds = {akCall .. akCondition},
      constantKinds = {akStrLit .. akIdent}
  )

  type
    AstTerm = CaseTerm[Ast, AstKind]

  let rSystem = RedSystem[string, CaseTerm[Ast, AstKind]](rules: @[(
      makePattern[string, CaseTerm[Ast, AstKind]](
        AstTerm(tkind: tkFunctor, functor: akCall, sons: @[
          AstTerm(tkind: tkConstant, value: Ast(
            kind: akIdent, strVal: "someFunc")),
          AstTerm(tkind: tkConstant, value: Ast(
            kind: akIntLit, intVal: 9000))
      ]))
    ,
      makeGenerator[string, CaseTerm[Ast, AstKind]](
        AstTerm(tkind: tkConstant, value: Ast(
          kind: akStrLit, strVal: "Hello 9000")
      ))
  )])

  let obj = Ast(kind: akCall, sons: @[
    Ast(kind: akIdent, strVal: "someFunc"),
    Ast(kind: akIntLit, intVal: 9000)
  ])

  # echo obj.toTerm()

  let res = reduce(
    obj.toTerm(),
    rSystem,
    astImpl
  )

  if res.ok:
    let resAst = res.term.fromTerm()
    echo resAst
    assert resAst == Ast(kind: akStrLit, strVal: "Hello 9000")
  else:
    echo "res not ok"
