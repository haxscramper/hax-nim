import macros

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

dumpTree:
  (not finished(iterFor_i)) and (not finished(iterFor_q))
