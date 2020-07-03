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
  const idx = when t is T0: 0
              elif t is T1: 1
              elif t is T2: 2
              elif t is T3: 3
              else: {.error("Wrong type for `get`").}

  if v.idx == idx:
    when idx == 0: v.f0
    elif idx == 1: v.f1
    elif idx == 2: v.f2
    else: v.f3
  else:
    raiseAssert("Cannot get value for type `" & $typeof(t) &
      "` - current variant index is " & $v.idx & ", but " &
      "value with type `" & $typeof(t) & "` has index " & $idx)


func idx*[T0, T1, T2, T3](v: Var4[T0, T1, T2, T3]): int = v.idx

template hasType*[T0, T1, T2, T3](
  invar: Var4[T0, T1, T2, T3], t: typed): bool =

  when t is T0: invar.idx == 0
  elif t is T1: invar.idx == 1
  elif t is T2: invar.idx == 2
  elif t is T3: inver.idx == 3
  else:
    {.error("Unexpected type").} # TODO better error

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

template `&=`*[T0, T1, T2, T3](
  v: var seq[Var4[T0, T1, T2, T3]], val: typed) =
  for item in val:
    v.add item

# TODO function to concatenate two sequences
# TODO proc to create variant instance

# func concat[T1, T2](
#   s1: openarray[T1], s2: openarray[T2]): seq[Var2[T1, T2]] =
#   for v in s1:
#     result.add Var2[]
