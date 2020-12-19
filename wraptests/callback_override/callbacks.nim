const derivedHeader* = "cppderived.hpp"

{.compile: "cppderived.cpp".}

type
  CppBaseDerivedRaw* {.
    importcpp: "CppBaseDerived",
    header: derivedHeader
  .} = object

    baseMethodImplProc* {.importcpp: "baseMethodImpl".}:
      proc(ni: ptr CppBaseDerivedRaw,
           userData: pointer,
           arg: cint,
           closureEnv: pointer
           ) {.cdecl.}

  ClosureEnv[T] = tuple[
    impl: proc(this: var CppBaseDerived[T], arg: int, env: pointer) {.cdecl.},
    env: pointer
  ]

  CppBaseDerived*[T] = object
    d*: ptr CppBaseDerivedRaw
    userData*: T
    baseMethodImpl*: ClosureEnv[T]


proc setBaseMethod*[T](
    self: var CppBaseDerived[T],
    cb: proc(this: var CppBaseDerived[T], arg: cint)
  ) =


  let implCallback = proc(
    derived: ptr CppBaseDerivedRaw,
    userData: pointer,
    arg: cint,
    closureEnv: pointer
  ): void {.cdecl.} =
    echo "  Executing implementation callback"
    let clos = cast[ptr ClosureEnv[T]](closureEnv)

    echo "  Dereferenced closure environment"
    var v: CppBaseDerived[T]

    echo "  Called callback implementation closure"

    echo "  Closure is nil: ", clos.isNil()
    echo "  Closure environment is nil: ", clos[].env.isNil()
    echo "  Closure implementation is nil: ", clos[].impl.isNIl()
    clos.impl(v, arg, clos.env)


  self.d.baseMethodImplProc = implCallback
  self.baseMethodImpl.env = cb.rawEnv()
  self.baseMethodImpl.impl = cast[proc(this: var CppBaseDerived[T], arg: int, env: pointer) {.cdecl.}](cb.rawProc())

  echo "Closure implementation parts are nil immediately after setting: ",
    self.baseMethodImpl.impl.isNil(), " ",
    self.baseMethodImpl.env.isNil(), " "



proc newCppBaseDerivedRaw(): ptr CppBaseDerivedRaw
  {.
    importcpp: "new CppBaseDerived(@)",
    constructor,
    header: derivedHeader
  .}

proc newCppBaseDerived*[T](): CppBaseDerived[T] =
  CppBaseDerived[T](d: newCppBaseDerivedRaw())

proc baseMethod*[T](derived: var CppBaseDerived[T], arg: int): void =
  echo "Calling base method"
  echo "Closure implementation parts are nil: ",
    derived.baseMethodImpl.impl.isNil(), " ",
    derived.baseMethodImpl.env.isNil(), " "

  echo "Impl pointer is nil: ", derived.d.baseMethodImplProc.isNil()

  proc baseMethod(
    derived: ptr CppBaseDerivedRaw,
    userData: pointer,
    arg: int,
    closureEnv: pointer
  ): void {.importcpp: "#.baseMethodOverride(@)", header: derivedHeader.}

  baseMethod(
    derived.d,
    cast[pointer](addr derived.userData),
    arg,
    cast[pointer](addr derived.baseMethodImpl)
  )


