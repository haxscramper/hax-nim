import callbacks



proc main() =
  var derived = newCppBaseDerived[int]()

  let capture = "hello"

  derived.setBaseMethod proc(this: var CppBaseDerived[int], arg: cint) =
      echo capture
      echo "Override callback with nim implementation", arg

  derived.baseMethod(12)

main()
