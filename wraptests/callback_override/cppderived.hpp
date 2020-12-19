#pragma once

#include <stdio.h>
#include "cppbase.hpp"

struct CppBaseDerived : public CppBase {
  void (*baseMethodImpl)(CppBaseDerived*, void*, int, void*);

  void baseMethodOverride(
    void* userData,  /// Custom user data
    int arg,  /// Original argument to method
    void* closureEnv /// Pointer to nim closure environment
  );
};

