import hmisc/hterms_callback
import hmisc/hterms_tree

import hashes, sequtils, strformat, strutils
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

proc hash(a: Ast): Hash =
  var h: Hash = 0
  h = h !& hash(a.kind)

proc `==`(lhs, rhs: Ast): bool =
  lhs.kind == rhs.kind and
  (
    case lhs.kind:
      of akStrLit, akIdent: lhs.strVal == rhs.strVal
      of akIntLit: lhs.intVal == rhs.intVal
      else: subnodesEq(lhs, rhs, sons)
  )

test "Ast rewriting":
  defineTermSystemFor[Ast, AstKind](
      kindField = kind,
      sonsField = sons,
      implName = astImpl,
      functorKinds = {akCall .. akCondition},
      constantKinds = {akStrLit .. akIdent}
  )

  type
    AstTerm = CaseTerm[Ast, AstKind]

  let rSystem = RedSystem[CaseTerm[Ast, AstKind]](rules: @[(
      AstTerm(tkind: tkFunctor, functor: akCall, sons: @[
        AstTerm(tkind: tkConstant, value: Ast(
          kind: akIdent, strVal: "someFunc")),
        AstTerm(tkind: tkConstant, value: Ast(
          kind: akIntLit, intVal: 9000))
      ]).makePattern()
      ,
      AstTerm(tkind: tkConstant, value: Ast(
        kind: akStrLit, strVal: "Hello 9000")).makeGenerator()
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
  else:
    echo "res not ok"
