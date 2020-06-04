import macros
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

when true:
  import hmisc/hterms_callback

  type
    Arithm = object
      case kind: TermKind
      of tkConstant:
        val: int
      of tkVariable:
        name: string
      of tkFunctor:
        sym: string
        subt: seq[Arithm]
      of tkPlaceholder:
        nil

  proc hash(a: Arithm): Hash =
    discard

  proc `==`(lsh, rhs: Arithm): bool =
    discard


  let impl = TermImpl[Arithm, string, int](
    getKind: proc(a: Arithm): TermKind = a.kind
  )

  let s1 = Arithm(kind: tkFunctor, sym: "S", subt: @[
    Arithm(kind: tkConstant, val: 0)
  ])


  let s2 = Arithm(kind: tkFunctor, sym: "S", subt: @[
    Arithm(kind: tkConstant, val: 0)
  ])

  let cb = TermImpl[Arithm, string, int](
    getVName: (t: Arithm) => t.name,
    getKind: (t: Arithm) => t.kind,
    setNth: (t: var Arithm, idx: int, val: Arithm) => (t.subt[idx] = val)
  )

  let rSystem = RedSystem[Arithm](
    rules: {
      # A + 0 -> A
      Arithm(kind: tkFunctor, sym: "+", subt: @[
          Arithm(kind: tkVariable, name: "A"),
          Arithm(kind: tkConstant, val: 0)
        ])
      :
      Arithm(kind: tkVariable, name: "A"),

      # A + S(B) -> S(A + B)
      Arithm(kind: tkFunctor, sym: "+", subt: @[
        Arithm(kind: tkVariable, name: "A"),
        Arithm(kind: tkFunctor, sym: "S", subt: @[
          Arithm(kind: tkVariable, name: "B")
        ])
      ]) :
      Arithm(kind: tkFunctor, sym: "S", subt: @[
        Arithm(kind: tkFunctor, sym: "+", subt: @[
          Arithm(kind: tkVariable, name: "A"),
          Arithm(kind: tkVariable, name: "B")
        ])
      ]),

      # A * 0 -> 0
      Arithm(kind: tkFunctor, sym: "*", subt: @[
          Arithm(kind: tkVariable, name: "A"),
          Arithm(kind: tkConstant, val: 0)
        ])
      :
      Arithm(kind: tkConstant, val: 0),

      # A * S(B) -> A + (A * B)
      Arithm(kind: tkFunctor, sym: "*", subt: @[
        Arithm(kind: tkVariable, name: "A"),
        Arithm(kind: tkFunctor, sym: "S", subt: @[
          Arithm(kind: tkVariable, name: "B")
        ])
      ]) :
      Arithm(kind: tkFunctor, sym: "+", subt: @[
        Arithm(kind: tkVariable, name: "A"),
        Arithm(kind: tkFunctor, sym: "*", subt: @[
          Arithm(kind: tkVariable, name: "A"),
          Arithm(kind: tkVariable, name: "B")
        ])
      ])
    }.toTable()
  )

  echo reduce(
    # S(0) + S(0)
    Arithm(kind: tkFunctor, sym: "+", subt: @[
      Arithm(kind: tkFunctor, sym: "S", subt: @[
        Arithm(kind: tkConstant, val: 0)
      ]),
      Arithm(kind: tkFunctor, sym: "S", subt: @[
        Arithm(kind: tkConstant, val: 0)
      ])
    ]),
    rSystem,
    cb
  )
