import macros
import strformat
import hashes
import sequtils
import tables
import hmisc/hterms


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
