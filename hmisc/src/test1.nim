import macros

proc test[T1, T2](arg: T2): (T1, T2) =
  discard

echo test[int, string]("hello")
