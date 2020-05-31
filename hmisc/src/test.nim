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
  TermImpl = object
    case kind: TermKind
      of tkVariable:
        name: string
        genIdx: int
      of tkFunctor:
        sym: string
        subt: seq[TermImpl]
      of tkConstant:
        value: string
      of tkPlaceholder:
        nil


proc reduction[T: Term](arg: T): T =
  return arg

let t1 = TermImpl()
let t2 = TermImpl()
let env = TermEnv[TermImpl]()

echo reduction(t1)
echo unif(t1, t2, env)
