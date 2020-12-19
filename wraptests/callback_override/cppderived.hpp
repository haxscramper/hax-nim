#pragma once

#include <stdio.h>
#include "cppbase.hpp"

struct CppBaseDerived : public CppBase {
  // Callback for nim implementation.
  void (*baseMethodImpl)(void*, int);

  void baseMethodOverride(
    void* userData,  /// Custom user data
    int arg  /// Original argument to method
  );
};

