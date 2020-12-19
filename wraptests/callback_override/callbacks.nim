const derivedHeader* = "cppderived.hpp"

{.compile: "cppderived.cpp".}

type
  CppBaseDerivedRaw* {.
    importcpp: "CppBaseDerived",
    header: derivedHeader
  .} = object

    baseMethodImplProc* {.importcpp: "baseMethodImpl".}:
      proc(userData: pointer, arg: cint) {.cdecl.}

  CppBaseDerived*[T] = object
    ## Wrapper object might (in theory) also serve as a way to manage CPP
    ## objects using nim memory management. Destruction heap-allocated object
    ## will be performed on `destroy=` hook. Using composition instead of
    ## pointer to implementation is also possible.

    d*: ptr CppBaseDerivedRaw ## Pointer to raw object implementation

    userData*: T ## Custom user data

    # Callback closure implementation, separated into underlying parts.
    clos: tuple[
      # C function callback, with additional argument for closure environment
      impl: proc(this: var CppBaseDerived[T], arg: int, env: pointer) {.cdecl.},

      # Pointer to environment itself
      env: pointer
    ]


proc setBaseMethod*[T](
    self: var CppBaseDerived[T],
    cb: proc(this: var CppBaseDerived[T], arg: cint)
  ) =

  # `{.cdecl.}` implementation callback that will be passed back to
  # raw derived class
  let implCallback = proc(userData: pointer, arg: cint ): void {.cdecl.} =
    # Uncast pointer to derived class
    var derived = cast[ptr CppBaseDerived[T]](userData)

    # Call closure implementation, arguments and closure environment.
    derived.clos.impl(derived[], arg, derived.clos.env)


  self.d.baseMethodImplProc = implCallback
  self.clos.env = cb.rawEnv()
  self.clos.impl = cast[CppBaseDerived[T].clos.impl](cb.rawProc())

proc newCppBaseDerivedRaw(): ptr CppBaseDerivedRaw
  # Implementation for raw object
  {.
    importcpp: "new CppBaseDerived(@)",
    constructor,
    header: derivedHeader
  .}

proc newCppBaseDerived*[T](): CppBaseDerived[T] =
  ## Wrapper constructor. All implementation detauls for closure will be
  ## set using `setBaseMethod`, so we only initialize base object.
  CppBaseDerived[T](d: newCppBaseDerivedRaw())

proc baseMethod*[T](derived: var CppBaseDerived[T], arg: int): void =
  proc baseMethod(
    impl: ptr CppBaseDerivedRaw,
    userData: pointer,
    arg: int
  ): void {.importcpp: "#.baseMethodOverride(@)", header: derivedHeader.}

  baseMethod(derived.d, cast[pointer](addr derived), arg)


