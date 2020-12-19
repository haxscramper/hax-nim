import callbacks



proc main() =
  var derived = newCppBaseDerived[int]()

  let capture = "hello"

  derived.setBaseMethod proc(this: var CppBaseDerived[int], arg: cint) =
      echo capture
      echo "Override callback with nim implementation", arg

  echo "Closure implementation parts are nil after setBaseMethod: ",
    derived.baseMethodImpl.impl.isNil(), " ",
    derived.baseMethodImpl.env.isNil(), " "

  derived.baseMethod(12)

main()
