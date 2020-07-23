type
  U = object
    case a: bool
    of true: str: string
    of false: discard

  PU = ref object

proc u(): U =
  echo "U"
  result = U(a: true, str: "Hello world")
  echo result

proc pu(): PU = echo "PU"

const uAlias = u
const puAlias = pu

echo "runtime"
let val = uAlias() # Executed at compile-time
echo val

discard u()
discard pu()
discard puAlias()
