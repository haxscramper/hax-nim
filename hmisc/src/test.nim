import macros
import strutils
import hmisc/halgorithm
import sugar
import strformat
import hashes
import sequtils
import tables


when false:
  template iterValType*(arg: untyped): untyped =
    when compiles(arg[0]):
      typeof(arg[0])
    else:
      typeof(arg)

  var ress {.inject.}: seq[iterValType((0 .. 2))]
  let iterFor_i = iterator (): iterValType(0 .. 2) =
                    for val in 0 .. 2:
                      yield val

  let iterFor_q = iterator (): iterValType(@[1, 2, 4]) =
    for val in @[1, 2, 4]:
      yield val

# dumpTree:
#   (not finished(iterFor_i)) and (not finished(iterFor_q))

# template t(d: string = "default"): untyped =
#   echo "h"

when false:
  macro loop(arg, body: untyped): untyped =
    echo arg.toStrLit()
    echo body.toStrLit()

  macro loop1(body: untyped): untyped =
    quote do:
      block:
        type ResType {.inject.} = int
        var r {.inject.}: ResType
        loop((), `body`)
        r


  loop([res = [all, tuple], nn = [11]], (lfor i in 0..2; lmax i; lcoll i))
  echo typeof(loop1((lfor i in 0..2; lmax i; lcoll i)))
  echo typeof(loop1((lfor i in 0..2; lmax i; lcoll i)))

  loop [res = all]:
    lfor 12

when false:
  import hmisc/hterms
  type
    Arithm = Term[string, int]
    ArithmEnv = TermEnv[string, int]
    ArithmSys = RedSystem[string, int]


  let mf = makeFunctor[string, int]
  let mc = makeConstant[string, int]
  let mp = makePlaceholder[string, int]
  let mv = makeVariable[string, int]

  let t1 = Arithm()
  let t2 = Arithm()
  var env = ArithmEnv()
  var sys = ArithmSys()

  # A + 0 -> A
  sys["+".mf(@[mv "A", mc 0])] = mv "A"

  # A + S(B) -> S(A + B)
  sys["+".mf(@[mv "A", "S".mf(@[mv "B"])])] =
    "S".mf(@["+".mf(@[mv "A", mv "B"])])

  # A * 0 -> 0
  sys["*".mf(@[mv "A", mc 0])] = mc 0

  # A * S(B) -> A + (A * B)
  sys["*".mf(@[mv "A", "S".mf(@[mv "B"])])] =
    "+".mf(@[mv "A", "+".mf(@[mv "A", mv "B"])])

  let sum = "+".mf(@[
      "S".mf(@["S".mf(@[mc 0])]),
      "S".mf(@["S".mf(@[mc 0])])
    ])

  echo reduce(sum, sys)

import hmisc/hterms_callback

type
  Arithm = object
    case tkind: TermKind
    of tkConstant:
      tval: int
    of tkVariable:
      tname: string
    of tkFunctor:
      tsym: string
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


proc `$`*(term: Arithm): string =
  case term.tkind:
    of tkConstant:
      "'" & $term.tval & "'"
    of tkVariable:
      "_" & $term.tname
    of tkFunctor:
      if ($term.tsym).validIdentifier():
        $term.tsym & "(" & term.tsubt.mapIt($it).join(", ") & ")"
      else:
        case term.tsubt.len():
          of 1: &"{term.tsym}({term.tsubt[0]})"
          of 2: &"{term.tsubt[0]} {term.tsym} {term.tsubt[1]}"
          else:
            $term.tsym & "(" & term.tsubt.mapIt($it).join(", ") & ")"
    of tkPlaceholder:
      "_"

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


if false:
  let s1 = Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
    Arithm(tkind: tkConstant, tval: 0)
  ])


  let s2 = Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
    Arithm(tkind: tkConstant, tval: 0)
  ])

  let cb = TermImpl[Arithm, string, int](
    getVName: (t: Arithm) => t.tname,
    getKind: (t: Arithm) => t.tkind,
    setNth: (t: var Arithm, idx: int, val: Arithm) => (t.tsubt[idx] = val),
    getNth: (t: Arithm, idx: int) => (t.tsubt[idx]),
    getNthMod: proc(t: var Arithm, idx: int): var Arithm = t.tsubt[idx],
    getTsym: (t: Arithm) => t.tsym,
    getSubt: (t: Arithm) => t.tsubt,
    setSubt: (t: var Arithm, subt: seq[Arithm]) => (t.tsubt = subt),
    getValue: (t: Arithm) => t.tval,
    unifCheck: (t1: Arithm, t2: Arithm) => true,
    makePlaceholder: () => Arithm(tkind: tkPlaceholder),
    makeConstant: (v: int) => Arithm(tkind: tkConstant, tval: v),
    makeVariable: (n: string) => Arithm(tkind: tkVariable, tname: n),
    makeFunctor: (sym: string, subt: seq[Arithm]) => Arithm(
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
          Arithm(tkind: tkFunctor, tsym: "+", tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkConstant, tval: 0)
          ]).makePattern()
        ) , (
          Arithm(tkind: tkVariable, tname: "A").makeGenerator()
        )
      ),

      (
        makeMatcher(
          proc(t: Arithm): TermEnv[Arithm] =
            echo "testing ", t
            raise UnifFailure(msg: "Test")
        ) , (
          proc(env: TermEnv[Arithm]): Arithm {.closure.} = discard
        )
      ),

      # A + S(B) -> S(A + B)
      (
        (
          Arithm(tkind: tkFunctor, tsym: "+", tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
              Arithm(tkind: tkVariable, tname: "B")
            ])
          ]).makePattern()
        ) , (
          Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
            Arithm(tkind: tkFunctor, tsym: "+", tsubt: @[
              Arithm(tkind: tkVariable, tname: "A"),
              Arithm(tkind: tkVariable, tname: "B")
            ])
          ]).makeGenerator()
        )
      ),

      # A * 0 -> 0
      (
        (
          Arithm(tkind: tkFunctor, tsym: "*", tsubt: @[
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
          Arithm(tkind: tkFunctor, tsym: "*", tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
              Arithm(tkind: tkVariable, tname: "B")
            ])
          ]).makePattern()
        ) , (
          Arithm(tkind: tkFunctor, tsym: "+", tsubt: @[
            Arithm(tkind: tkVariable, tname: "A"),
            Arithm(tkind: tkFunctor, tsym: "*", tsubt: @[
              Arithm(tkind: tkVariable, tname: "A"),
              Arithm(tkind: tkVariable, tname: "B")
            ])
          ]).makeGenerator()
        )
      )
    ]
  )

  echo reduce(
    # S(0) + S(0)
    Arithm(tkind: tkFunctor, tsym: "+", tsubt: @[
      Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
        Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
          Arithm(tkind: tkConstant, tval: 0)
        ]),
      ]),
      Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
        Arithm(tkind: tkFunctor, tsym: "S", tsubt: @[
          Arithm(tkind: tkConstant, tval: 0)
        ]),
      ])
    ]),
    rSystem,
    cb
  )
