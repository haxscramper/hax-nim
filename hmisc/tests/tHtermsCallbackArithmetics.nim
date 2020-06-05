import unittest

import hmisc/hterms_callback

import hmisc/halgorithm

import strutils, sequtils, strformat, sugar, options, hashes

type
  ArithmOp = enum
    aopSucc
    aopAdd
    aopMult

  Arithm = object
    case tkind: TermKind
    of tkConstant:
      tval: int
    of tkVariable:
      tname: string
    of tkFunctor:
      tsym: ArithmOp
      tsubt: seq[Arithm]
    of tkPlaceholder:
      nil

proc hash(a: Arithm): Hash =
  var h: Hash = 0
  h = h !& hash(a.tkind)
  case a.tkind:
    of tkVariable: h = h !& hash(a.tname)
    of tkConstant: h = h !& hash(a.tval)
    of tkFunctor: h = h !& hash(a.tsym)
    of tkPlaceholder: discard

proc `==`(lhs, rhs: Arithm): bool =
  lhs.tkind == rhs.tkind and (
    case lhs.tkind:
      of tkConstant: lhs.tval == rhs.tval
      of tkVariable: lhs.tname == rhs.tname
      of tkFunctor:
        lhs.tsym == rhs.tsym and
        zip(lhs.tsubt, rhs.tsubt).allOfIt(it[0] == it[1])
      of tkPlaceholder:
        true
  )



proc `$`*(term: Arithm): string =
  case term.tkind:
    of tkConstant:
      "'" & $term.tval & "'"
    of tkVariable:
      "_" & $term.tname
    of tkFunctor:
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
    of tkPlaceholder:
      "_"



test "Arithmetic addition":
  let s1 = Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
    Arithm(tkind: tkConstant, tval: 0)
  ])


  let s2 = Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
    Arithm(tkind: tkConstant, tval: 0)
  ])

  let cb = TermImpl[Arithm, string, ArithmOp, int](
    getVName: (t: Arithm) => t.tname,
    getKind: (t: Arithm) => t.tkind,
    setNth: (t: var Arithm, idx: int, val: Arithm) => (t.tsubt[idx] = val),
    getNth: (t: Arithm, idx: int) => (t.tsubt[idx]),
    getNthMod: proc(t: var Arithm, idx: int): var Arithm = t.tsubt[idx],
    getFsym: (t: Arithm) => t.tsym,
    getSubt: (t: Arithm) => t.tsubt,
    setSubt: (t: var Arithm, subt: seq[Arithm]) => (t.tsubt = subt),
    getValue: (t: Arithm) => t.tval,
    unifCheck: (t1: Arithm, t2: Arithm) => true,
    makePlaceholder: () => Arithm(tkind: tkPlaceholder),
    makeConstant: (v: int) => Arithm(tkind: tkConstant, tval: v),
    makeVariable: (n: string) => Arithm(tkind: tkVariable, tname: n),
    makeFunctor: (sym: ArithmOp, subt: seq[Arithm]) => Arithm(
      tkind: tkFunctor, tsym: sym, tsubt: subt
    )
  )

  assertCorrect(cb)

  let rSystem = RedSystem[Arithm](
    # NOTE this madness is intended to be generated from some kind of
    # DSL, not written by hand.
    rules: @[
      # A + 0 -> A
      (
        (
          Arithm(tkind: tkFunctor, tsym: aopAdd, tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkConstant, tval: 0)
          ]).makePattern()
        ) , (
          Arithm(tkind: tkVariable, tname: "A").makeGenerator()
        )
      ),

      (
        makeMatcher(
          proc(t: Arithm): Option[TermEnv[Arithm]] =
            echo "testing ", t
        ) , (
          proc(env: TermEnv[Arithm]): Arithm {.closure.} = discard
        )
      ),

      # A + S(B) -> S(A + B)
      (
        (
          Arithm(tkind: tkFunctor, tsym: aopAdd, tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
              Arithm(tkind: tkVariable, tname: "B")
            ])
          ]).makePattern()
        ) , (
          Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
            Arithm(tkind: tkFunctor, tsym: aopAdd, tsubt: @[
              Arithm(tkind: tkVariable, tname: "A"),
              Arithm(tkind: tkVariable, tname: "B")
            ])
          ]).makeGenerator()
        )
      ),

      # A * 0 -> 0
      (
        (
          Arithm(tkind: tkFunctor, tsym: aopMult, tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkConstant, tval: 0)
          ]).makePattern()
        ) , (
          Arithm(tkind: tkConstant, tval: 0).makeGenerator()
        )
      ),

      # A * S(B) -> A + (A * B)
      (
        (
          Arithm(tkind: tkFunctor, tsym: aopMult, tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
              Arithm(tkind: tkVariable, tname: "B")
            ])
          ]).makePattern()
        ) , (
          Arithm(tkind: tkFunctor, tsym: aopAdd, tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkFunctor, tsym: aopMult, tsubt: @[
              Arithm(tkind: tkVariable, tname: "A"),
              Arithm(tkind: tkVariable, tname: "B")
            ])
          ]).makeGenerator()
        )
      )
    ]
  )

  let res = reduce(
    # S(0) + S(0)
    Arithm(tkind: tkFunctor, tsym: aopAdd, tsubt: @[
      Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
        Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
          Arithm(tkind: tkConstant, tval: 0)
        ]),
      ]),
      Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
        Arithm(tkind: tkFunctor, tsym: aopSucc, tsubt: @[
          Arithm(tkind: tkConstant, tval: 0)
        ]),
      ])
    ]),
    rSystem,
    cb
  )

  assert "S(S(S(S('0'))))" == $res[0]
