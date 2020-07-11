## Statically typed nim ast representation

type
  FieldBranch*[Node] = object
    # IDEA three possible parameters: `NimNode` (for compile-time
    # operations), `PNode` (for analysing code at runtime) and.
    when Node is NimNode:
      ofValue*: Node ## Exact AST used in field branch
    else:
      value*: ObjTree[Node] ## Match value for case branch

    flds*: seq[Field[Node]] ## Fields in the case branch
    isElse*: bool

  Field*[Node] = object
    ## More complex representation of object's field - supports
    ## recursive fields with case objects. IMPLEMENT - not currently
    ## supported.
    name*: string
    fldType*: string ## Type of field value
    value*: ObjTree[Node]
    case isKind*: bool
      of true:
        selected*: int ## Index of selected branch
        branches*: seq[FieldBranch[Node]] ## List of all branches as
                                    ## `value-branch` pairs.
      of false:
        discard

type
  ObjKind* = enum
    okConstant ## Literal value
    okSequence ## Sequence of items
    okTable ## List of key-value pairs with single types for keys and
    ## values
    okComposed ## Named list of field-value pairs with possilby
    ## different types for fields (and values). List name is optional
    ## (unnamed object), field name is optional (unnamed fields)

  ObjTree*[Node] = object
    ##[

## Fields

:isPrimitive: Value is primitve or not?

  Primitive value will be added to graphviz node export as part of the
  table (in regular export) as oppposed to non-primitive value (it
  will be rendered as separate node). By default primitive values are
  `int`, `string`, `float` etc. types, tuples/objects that are (1)
  composed only from primitive types (`(int, int)`), (2) have four
  fields or less. Also tables/sequences with four elements or less are
  considered primitive if (1) both keys and values are primitive (2)
  container has four elements or less.

    ]##
    path*: seq[int] ## Path of object in original tree
    objId*: int
    isPrimitive*: bool ## Whether or not value can be considered primitive
    case kind*: ObjKind
      of okConstant:
        constType*: string ## Type of the value
        strlit*: string ## Value representation in string form
      of okSequence:
        itemType*: string ## Type of the sequence item
        valItems*: seq[ObjTree[Node]] ## List of values
      of okTable:
        keyType*: string ## Type of table key
        valType*: string ## TYpe of value key
        valPairs*: seq[tuple[key: string, val: ObjTree[Node]]] ## List of
        ## key-value pairs for table
        # XXXX TODO TEST used `ObjTree` for key too. Non-trivial types
        # can be used. Write unit tests for this functionality.

        # NOTE REFACTOR use `value` for enum field.
      of okComposed:
        namedObject*: bool ## This object's type has a name? (tuples
        ## does not have name for a tyep)
        namedFields*: bool ## Fields have dedicated names? (anonymous
        ## tuple does not have a name for fields)
        name*: string ## Name for an object
        case sectioned*: bool
          of false:
            # Simpler representation for object tree without
            # sectioning on different blocks depending on `kind`
            # fields: everything is put into single key-value
            # sequence.

            # XXX TODO Add field type
            fldPairs*: seq[tuple[name: string, value: ObjTree[Node]]] ## Sequence
            ## of field-value pairs for object representation
          of true:
            # Most of the case objects have one `kind` field named
            # 'kind' but this should account for cases with multiple
            # case fields as well as nested ones
            kindBlocks*: seq[Field[Node]] ## Object field tree. TODO -
            ## currently not implemented

  ValObjTree* = ObjTree[void] ## Object tree used at runtime.
  ValField* = Field[void] ## Field used at runtime
  ValFieldBranch* = FieldBranch[void] ## Field branch used at runtime

#==============================  operators  ==============================#

func `==`*[Node](lhs, rhs: Field[Node]): bool

func `==`*[Node](lhs, rhs: ObjTree[Node]): bool =
  lhs.kind == rhs.kind and
    (
      case lhs.kind:
        of okConstant:
          lhs.constType == rhs.constType and
          lhs.strLit == rhs.strLit
        of okSequence:
          lhs.itemType == rhs.itemType and
          subnodesEq(lhs, rhs, valItems)
        of okTable:
          lhs.keyType == rhs.keyType and
          lhs.valType == rhs.valType and
          zip(lhs.valPairs, rhs.valPairs).allOfIt(
            (it[0].key == it[1].key) and (it[0].val == it[1].val)
          )
        of okComposed:
          lhs.namedObject == rhs.namedObject and
          lhs.namedFields == rhs.namedFields and
          lhs.name == rhs.name and
          lhs.sectioned == rhs.sectioned and
          (
            case lhs.sectioned:
              of true:
                subnodesEq(lhs, rhs, kindBlocks)
              of false:
                zip(lhs.fldPairs, rhs.fldPairs).mapPairs(
                  (lhs.name == rhs.name) and (lhs.value == rhs.value)
                ).foldl(a and b)
          )

    )

func `==`*[Node](lhs, rhs: Field[Node]): bool =
  lhs.isKind == rhs.isKind and
    (
      case lhs.isKind:
        of true:
          lhs.name == rhs.name and
          lhs.fldType == rhs.fldType and
          lhs.value == rhs.value and
          subnodesEq(lhs, rhs, branches)
        of false:
          true
    )
