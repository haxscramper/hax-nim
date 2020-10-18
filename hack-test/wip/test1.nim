import macros
dumpAstGen:
  type A = B

func foo(n: int): auto = (proc(i: int): int = i + n)

echo foo(12)(10)
