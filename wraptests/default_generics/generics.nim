{.emit: """/*TYPESECTION*/
template<typename T, typename D = float>
struct Gen {
  int _fld;
  Gen(int arg) : _fld(arg) {};
};
""".}

# Even thought C++ struct has default template parameters,
# nim version overrides all of them.

type
  Gen[T, D] {.importcpp: "Gen<'0, '1>".} = object
    fld {.importcpp: "_fld".}: cint

# Basic constructor proc allows to override only
# non-defaulted template parameters
proc initGen[T](arg: int): Gen[T, cfloat] {.
    importcpp: "Gen<'0>(@)",
    constructor
  .}

# `Base` suffix to override all necessary parameters
proc initGenBase[T, D](arg: int): Gen[T, D] {.
    importcpp: "Gen<'0, '1>(@)",
    constructor
  .}


proc newGenBaseImpl(T, D: typedesc, arg: int):
  ptr Gen[T, D] {.
    importcpp: "new Gen<'*1, '*2'>(@)",
    nodecl,
  .}

proc newGenBase[T, D](arg: int): ptr Gen[T, D] =
  return newGenBaseImpl(T, D, arg)


proc main() =
  let val1 = initGen[string](5)
  let val2 = initGenBase[string, float](4)
  let val3 = newGenBase[float, int](2)
  echo val3.isNil()
  echo val3.fld

main()
