type
  AstKind = enum
    akList
    akToken
    akNterm

  AstSelector = object
    token: set[uint8]
    nterm: set[uint8]

  Ast[Node; Kind; Kinds: static[AstSelector]] = object
    kind: Kind
    case tkind: AstKind
      of akList, akNterm:
        node: Node
        subnodes: seq[Ast[Node, Kind, Kinds]]
      of akToken:
        token: Node

  AAstKind = enum
    aakNode
    aakLit
    aakFloat


  AAst = object
    case kind: AAstKind
      of aakNode:
        meta: string
        nice: string
      of aakLit:
        data: float
        world: float
      of aakFloat:
        zzz: char

func toUInt8Set[En: enum](enset: set[En]): set[uint8] =
  debugecho enset
  for val in enset:
    result.incl uint8(ord(val))


type
  AstTree = Ast[AAst, AAstKind, AstSelector(
    token: toUInt8Set({aakLit, aakFloat}),
    nterm: toUInt8Set({aakNode})
  )]

let a = AstTree()
echo typeof(a)
