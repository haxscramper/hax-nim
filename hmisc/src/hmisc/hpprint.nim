import hmisc/helpers
import tables

type
  ObjKind = enum
    okConstant
    okSequence
    okTable
    okComposed

  Field = object
    fldType: string
    case isKind: bool
      of true:
        subfields: seq[tuple[name: string, fld: Field]]
      of false:
        name: string
        value: ObjTree

  ObjTree = object
    case kind: ObjKind
      of okConstant:
        constType: string
        strlit: string
      of okSequence:
        itemType: string
        valItems: seq[ObjTree]
      of okTable:
        keyType: string
        valType: string
        valPairs: seq[tuple[key: string, val: ObjTree]]
      of okComposed:
        anonymous: bool
        name: string
        case sectioned: bool
          of false:
            # Simpler representation for object tree without
            # sectioning on different blocks depending on `kind`
            # fields: everything is put into single key-value
            # sequence.
            fldPairs: seq[tuple[name: string, value: ObjTree]]
          of true:
            # Most of the case objects have one `kind` field named
            # 'kind' but this should account for cases with multiple
            # case fields as well as nested ones
            kindBlocks: seq[Field]


proc toSimpleTree[Obj](entry: Obj): ObjTree =
  when compiles(pairs(entry)):
    echo "has pairs()"
    result = ObjTree(
      kind: okTable,
      keyType: $typeof(pairs(entry).nthType1()),
      valType: $typeof(pairs(entry).nthType2())
    )

    for key, val in pairs(entry):
      result.valPairs.add((key, val))

  elif (entry is object) or (entry is tuple):
    echo "object/tuple"
    when entry is object:
      result = ObjTree(kind: okComposed, name: $typeof(Obj), sectioned: false)
    else:
      result = ObjTree(kind: okComposed, anonymous: true, sectioned: false)

    for name, value in entry.fieldPairs():
      result.fldPairs.add((name, toSimpleTree(value)))
  else:
    echo "other"
    discard


type
  Obj1 = object
    f1: int
    f2: seq[int]
    f3: Table[int, string]

echo toSimpleTree(Obj1())

