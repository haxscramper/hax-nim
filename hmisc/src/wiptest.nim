import hmisc/cl_loop

template notCompiles(body: untyped): untyped =
  not compiles(body)

static:
  try:
    raise CodeError(msg: "Hello")
  except CodeError:
    echo "Exception caught"


echo loop1((lfor i in 0..2; lcoll i))
