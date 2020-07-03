type
  Var4*[T0, T1, T2, T3] = object
    case idx: range[0 .. 3]:
      of 0: f0: T0
      of 1: f1: T1
      of 2: f2: T2
      of 3: f3: T3

  Var3*[T0, T1, T2] = Var4[T0, T1, T2, void]
  Var2*[T0, T1] = Var4[T0, T1, void, void]

template get*[T0, T1, T2, T3](v: Var4[T0, T1, T2, T3], t: typed): auto =
  var idx: int = 0
  var res: t
  for name, fld in v.fieldPairs():
    when fld is t:
      if v.idx == idx:
        res = fld
      else:
        raiseAssert("Cannot get value for type `" & $typeof(t) &
          "` - current variant index is " & $v.idx & ", but " &
          "value with type `" & $typeof(t) & "` has index " & $idx)

    if name != "idx":
      inc idx

  res

template hasType*[T0, T1, T2, T3](
  invar: Var4[T0, T1, T2, T3], t: typed): bool =
  var res: bool = false
  var idx: int = 0
  for name, fld in invar.fieldPairs():
    if (fld is t) and (invar.idx == idx):
      res = true

    if name != "idx":
      inc idx

  res

func setv*[T0, T1, T2, T3](
  v: var Var4[T0, T1, T2, T3], val: T0 | T1 | T2 | T3) =

  v = when val is T0: Var4[T0, T1, T2, T3](idx: 0, f0: val)
      elif val is T1: Var4[T0, T1, T2, T3](idx: 1, f1: val)
      elif val is T2: Var4[T0, T1, T2, T3](idx: 2, f2: val)
      else:           Var4[T0, T1, T2, T3](idx: 3, f3: val)

var tmp: Var2[int, float]
tmp.setv(1.2)


func add*[T0, T1, T2, T3](
  v: var seq[Var4[T0, T1, T2, T3]], val: T0 | T1 | T2 | T3) =

  when val is T0: v.add Var4[T0, T1, T2, T3](idx: 0, f0: val)
  elif val is T1: v.add Var4[T0, T1, T2, T3](idx: 1, f1: val)
  elif val is T2: v.add Var4[T0, T1, T2, T3](idx: 2, f2: val)
  else:           v.add Var4[T0, T1, T2, T3](idx: 3, f3: val)

# TODO function to concatenate two sequences
# TODO proc to create variant instance

# func concat[T1, T2](
#   s1: openarray[T1], s2: openarray[T2]): seq[Var2[T1, T2]] =
#   for v in s1:
#     result.add Var2[]
