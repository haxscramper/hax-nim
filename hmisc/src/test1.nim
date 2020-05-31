type
  Gen[T] = object
    val: seq[(T, T)]

let gen = Gen[int]()
