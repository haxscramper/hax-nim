#include "cppderived.hpp"


void CppBaseDerived::baseMethodOverride(void* userdata, int arg) {
    if (this->baseMethodImpl == 0) {
        puts("--- No override used, fallback to default implementation\n");
        CppBase::baseMethod(arg);

    } else {
        puts("--- Using nim implementation\n");
        this->baseMethodImpl(userdata, arg);
    }
}
