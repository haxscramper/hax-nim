type
  SNested {.importcpp: "Nested", header: "enclass.hpp".} = enum
    valFixup0 = 0
    valFixup1 = 1
    val1 = 2
    val2 = 3

let hello = {val1, val2} + {val1, valFixup0}
echo hello
