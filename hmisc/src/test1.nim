import macros
import sugar

type
  Gen[T] = object
    val: seq[(T, T)]

let gen = Gen[int]()

macro `|->`(a, b: untyped): untyped =
  quote do:
    `a` + `b`


echo 12 |-> 22
