{.emit: """/*INCLUDESECTION*/
#include <stdio.h>
""".}

{.emit: """/*TYPESECTION*/
struct CppBase {
  virtual void baseMethod(int arg) {
    printf("arg from nim - %d -\n", arg);
  }
};

struct CppInterface {
  TNimType* m_type;
};

struct CppBaseDerived : public CppBase, public CppInterface {
  void (*baseMethodImpl)(CppBaseDerived*, int);

  void baseMethod(int arg) override {
    if (this->baseMethodImpl == 0) {
      puts("--- No override used, fallback to default implementation\n");
      CppBase::baseMethod(arg);

    } else {
      puts("--- Using nim implementation\n");
      this->baseMethodImpl(this, arg);

    }
  }
};
""".}







type
  CppBaseDerived {.importcpp: "CppBaseDerived".} = object of RootObj
    baseMethodImpl {.
      importcpp: "baseMethodImpl"
    .}: proc(ni: ptr CppBaseDerived, arg: cint) {.cdecl.}

proc newCppBaseDerived():
  ptr CppBaseDerived
  {.importcpp: "new CppBaseDerived(@)", constructor.}


proc baseMethod(derived: ptr CppBaseDerived, arg: int):
  void {.importcpp: "#.baseMethod(@)".}

var derived = newCppBaseDerived()

derived.baseMethod(12)

derived.baseMethodImpl =
  proc(ni: ptr CppBaseDerived, arg: cint) {.cdecl.} =
    echo "Override callback with nim implementation", arg

derived.baseMethod(12)







type
  FullyNimDerived = object of CppBaseDerived
    fld1: int

proc baseMethod(derived: FullyNimDerived, arg: int):
  void {.importcpp: "#.baseMethod(@)".}

proc toFullyNimDerived(base: ptr CppBaseDerived):
  ptr FullyNimDerived {.noinit.} =
  {.emit:"return (`FullyNimDerived`*)(`base`);".}

var nimder = FullyNimDerived()

nimder.baseMethod(222)

nimder.baseMethodImpl =
  proc(ni: ptr CppBaseDerived, arg: cint) {.cdecl.} =
    echo toFullyNimDerived(ni).fld1
    echo "Fully derived in nim, ", arg

nimder.baseMethod(333)
nimder.baseMethod(444)

echo "Finished"
