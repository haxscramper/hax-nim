import callbacks

var derived = newCppBaseDerived[int]()

derived.setBaseMethod proc(this: var CppBaseDerived[int], arg: cint) =

    echo "Override callback with nim implementation", arg

derived.baseMethod(12)
