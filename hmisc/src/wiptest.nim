import hmisc/cl_loop

{.define(raiseOnSyntaxError).}

template notCompiles(body: untyped): untyped =
  not compiles(body)

echo loop((
  lfor i in 0..2; lfor q in @[1,2,4];
  lcollect i; lcollect "i * q"))

static:
  try:
    raise CodeError(msg: "Hello")
  except CodeError:
    echo "Exception caught"
