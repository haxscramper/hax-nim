#include "cppderived.hpp"


void CppBaseDerived::baseMethod(void* userdata, int arg, void* closureEnv) {
  if (this->baseMethodImpl == 0) {
    puts("--- No override used, fallback to default implementation\n");
    CppBase::baseMethod(arg);

  } else {
    puts("--- Using nim implementation\n");
    this->baseMethodImpl(this, arg, userdata, closureEnv);

  }
}
