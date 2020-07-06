import unittest

import hmisc/hterms_callback

import hmisc/halgorithm

import strutils, sequtils, strformat, sugar, options

type
  ArithmOp = enum
    aopSucc
    aopAdd
    aopMult

  Arithm = object
    case operator: bool
    of true:
      tsym: ArithmOp
      tsubt: seq[Arithm]
    of false:
      tval: int

proc `==`(lhs, rhs: Arithm): bool =
  lhs.operator == rhs.operator and (
    case lhs.operator:
      of false: lhs.tval == rhs.tval
      of true:
        lhs.tsym == rhs.tsym and
        zip(lhs.tsubt, rhs.tsubt).allOfIt(it[0] == it[1])
  )

proc `$`*(term: Arithm): string =
  case term.operator:
    of false:
      "'" & $term.tval & "'"
    of true:
      let symName =
        case term.tsym:
          of aopSucc: "S"
          of aopAdd: "+"
          of aopMult: "*"

      if ($term.tsym).validIdentifier():
        symName & "(" & term.tsubt.mapIt($it).join(", ") & ")"
      else:
        case term.tsubt.len():
          of 1: &"{symName}({term.tsubt[0]})"
          of 2: &"{term.tsubt[0]} {symName} {term.tsubt[1]}"
          else:
            symName & "(" & term.tsubt.mapIt($it).join(", ") & ")"

type ATerm = Term[Arithm, ArithmOp]

func nOp(op: ArithmOp, subt: seq[ATerm]): ATerm =
  makeFunctor[Arithm, ArithmOp](op, subt)

func nVar(n: string): ATerm =
  makeVariable[Arithm, ArithmOp](n)

func nConst(n: int): ATerm =
  makeConstant[Arithm, ArithmOp](Arithm(operator: false, tval: n))

func mkOp(op: ArithmOp, sub: seq[Arithm]): Arithm =
  Arithm(operator: true, tsym: op, tsubt: sub)

func mkVal(val: int): Arithm =
  Arithm(operator: false, tval: val)

proc toTerm[V, F](val: V, cb: TermImpl[V, F]): Term[V, F] =
  if cb.isFunctor(val):
    return makeFunctor[V, F](cb.getFSym(val), cb.getSubt(val).mapIt(it.toTerm(cb)))
  else:
    return makeConstant[V, F](val)


suite "Hterms callback/arithmetic":
  test "Arithmetic addition":

    let cb = TermImpl[Arithm, ArithmOp](
      valStrGen: (proc(n: Arithm): string = "[[ TODO ]]")
    )

    assertCorrect(cb)

    let rSystem = RedSystem[Arithm, ArithmOp](
      # NOTE this madness is intended to be generated from some kind of
      # DSL, not written by hand.
      rules: @[
        # A + 0 -> A
        makeRulePair(
          nOp(aopAdd, @[nVar("A"), nConst(0)]).makePattern(),
          nVar("A").makeGenerator()
        ),

        # A + S(B) -> S(A + B)
        makeRulePair(
          nOp(aopAdd, @[ nVar("A"), nOp(aopSucc, @[ nVar("B") ]) ]).makePattern(),
          nOp(aopSucc, @[ nOp(aopAdd, @[ nVar("A"), nVar("B") ]) ]).makeGenerator()
        ),

        # A * 0 -> 0
        makeRulePair(
          nOp(aopMult, @[ nVar("A"), nConst(0) ]).makePattern(),
          nConst(0).makeGenerator()
        ),

        # A * S(B) -> A + (A * B)
        makeRulePair(
          nOp(aopMult, @[ nVar("A"), nOp(aopSucc, @[ nVar("B") ])]).makePattern(),
          nOp(aopAdd, @[ nVar("A"), nOp(aopMult, @[ nVar("A"), nVar("B") ])]).makeGenerator()
        )
      ]
    )

    let res = reduce(
      # S(0) + S(0)
      mkOp(aopAdd, @[
        mkOp(aopSucc, @[ mkOp(aopSucc, @[ mkVal(0) ]) ]),
        mkOp(aopSucc, @[ mkOp(aopSucc, @[ mkVal(0) ]) ])
      ]).toTerm(cb),
      rSystem,
      cb,
      reduceConstraints = rcNoConstraints
    )

    echo $res[0]
    assert "S(S(S(S('0'))))" == $res[0]
