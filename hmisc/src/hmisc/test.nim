type
  Test = object
    case kind: bool
      of true:
        f1: int
      of false:
        f2: string

    case kind2: bool
      of true:
        f12: float
      of false:
        case nestedKind: bool
          of true:
            f22: seq[float]
          of false:
            f23: int

for fld, val in Test(kind: true).fieldPairs():
  echo fld
