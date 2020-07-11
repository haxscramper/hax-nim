#========================  sequence operations  ==========================#

func sumjoin*(a: openarray[int], sep: int): int =
  a.sum() + (a.len > 0).tern((a.len() - 1) * sep, 0)

func cumsumjoin*(
  a: openarray[int], sep: int, addFirst: bool = false): seq[int] =
  var curr: int = 0
  if addFirst:
    result.add curr

  for val in a:
    result.add(val + sep + curr)
    curr += val + sep

iterator `..+`*(start: int, offset: int): int =
  for i in start ..< start + offset:
    yield i

func order*[Num](a, b: Num): (Num, Num) =
  if a > b: (b, a)
  else: (a, b)

template last*[T](stack: var seq[T]): var T = stack[^1]
template last*[T](stack: seq[T]): T = stack[^1]

proc max*[T](x: openArray[T], default: T): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  ## use `default` if array is empty
  if x.len == 0:
    result = default
  else:
    for i in x:
      if result < i: result = i

#=======================  single item operations  ========================#

func setMax*[T](a: var T, b: T): void =
  if a < b:
    a = b


proc nthType1*[T1, T2](a: (T1, T2)): T1 =
  ## Helper proc to get first type from tuple. Used as workaround for
  ## `pairs` iterator
  discard

proc nthType2*[T1, T2](a: (T1, T2)): T2 =
  ## Helper proc to get second type from tuple. Used as workaround for
  ## `pairs` iterator
  discard
