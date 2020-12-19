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

  CppBaseDerived*[T] = object
    d*: ptr CppBaseDerivedRaw
    userData*: T
    baseMethodImplEnv*: pointer


proc newCppBaseDerivedRaw(): ptr CppBaseDerivedRaw
  {.
    importcpp: "new CppBaseDerived(@)",
    constructor,
    header: derivedHeader
  .}

proc baseMethod(
    derived: ptr CppBaseDerivedRaw,
    userData: pointer,
    arg: int,
    closureEnv: pointer
  ): void {.importcpp: "#.baseMethod(@)", header: derivedHeader.}



proc newCppBaseDerived*[T](): CppBaseDerived[T] =
  CppBaseDerived[T](d: newCppBaseDerivedRaw())

proc setBaseMethod*[T](
    self: var CppBaseDerived[T],
    cb: proc(this: var CppBaseDerived[T], arg: cint)
  ) =

  self.baseMethodImplEnv = cb.rawEnv()

  let implCallback = proc(
    derived: ptr CppBaseDerivedRaw,
    userData: pointer,
    arg: cint,
    closureEnv: pointer
  ): void {.cdecl.} =
    discard


  self.d.baseMethodImplProc = implCallback



proc baseMethod*[T](derived: var CppBaseDerived[T], arg: int): void =
  baseMethod(
    derived.d,
    cast[pointer](addr derived.userData),
    arg,
    cast[pointer](addr derived.baseMethodImplEnv)
  )


