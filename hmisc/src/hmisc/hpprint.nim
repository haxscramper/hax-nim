# TODO account for control codes in stings

# TODO allow to discard certain elements after they have been
# generated. NOTE to not overcomplicate interface it should be done
# using `isAllowed(val: ObjTree)` instead of generic predicate.

# TODO allow to compress elements back if necessary (rst node
# represents each field as separate leaf)

## Universal pretty-printer

import hmisc/[helpers, defensive, halgorithm]
import tables, sequtils, math, strutils, strformat, macros
import typetraits, macroutils
import hvariant, colors, hmisc_types

import graphviz_ast, html_ast

import gara, with

export tables, halgorithm

type
  Delim* = object
    ## Block delimiters
    content: string ## String for delimiter
    preferMultiline: bool ## Don't try to put delimiter on the same
    ## line with content - always prefer new chunks

  DelimPair* = tuple[
    start: Delim,
    final: Delim
  ]

  PPrintConf* = object
    ##[

Pretty print configuration

    ]##

    maxWidth*: int ## Max allowed width
    identStr*: string ## String to use for indentaion
    # wrapLargerThan*: int ##

    kvSeparator*: string ## String to use when separating key-value
    ## pairs.
    tblWrapper*: DelimPair ## Pair of delimiter around table instance

    objWrapper*: DelimPair ## Pair of delimiters around object instance
    fldNameWrapper*: DelimPair ## Pair of delimiter around table key
    fldSeparator*: string ## String to place between key-value pairs in list
    nowrapMultiline*: bool ## Do not wrap mutliline objects in delimiters
    alignFieldsRight*: bool ## Use right align on fields (default - left)

    seqSeparator*: string ## String to place between items in sequence
    seqPrefix*: string ## Prefix to use for multiline sequece
    ## instance. If empty multiline string will be wrapped in regular
    ## delimiters
    seqWrapper*: DelimPair ## Delimiters for sequence instance
    hideEmptyFields*: bool ## Hide empty fields (seq of zero length,
    ## `nil` references etc.).

  ObjKind* = enum
    okConstant ## Literal value
    okSequence ## Sequence of items
    okTable ## List of key-value pairs with single types for keys and
    ## values
    okComposed ## Named list of field-value pairs with possilby
    ## different types for fields (and values). List name is optional
    ## (unnamed object), field name is optional (unnamed fields)

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

type
  ObjElem = object
    text: string
    color: Color

func makeObjElem(text: string): ObjElem =
  ObjElem(
    text: text
  )

type
  SizePolicy = enum
    spExpanding
    spFixed

  GridCell[T] = object
    valid: bool
    rows: int
    cols: int
    vertPolicy: SizePolicy
    horizPolicy: SizePolicy

    case isItem: bool
      of true:
        item: T
        size: Size ## Item size
      of false:
        grid: BlockGrid[T]

  BlockGrid[T] = object
    grid: SparseGrid[GridCell[T]] ## row[col[cell]]
    maxH: seq[int] ## Max height in each row
    maxW: seq[int] ## Max width in each column

  Range = object
    a: int
    b: int

import hashes
func hash*(r: Range): Hash = hash(r.a) !& hash(r.b)
func width*[T](cell: GridCell[T]): int = cell.size.width
func height*[T](cell: GridCell[T]): int = cell.size.height
func width*[T](grid: BlockGrid[T]): int = grid.maxW.sum()
func height*[T](grid: BlockGrid[T]): int = grid.maxH.sum()
func toRange*(elems: (int, int)): Range = Range(a: elems[0], b: elems[1])
func colRange*[T](
  grid: BlockGrid[T],
  pos: tuple[row, col: int]): Range =
  let start = pos.col
  var finish = pos.col

func isPoint(r: Range): bool = (r.a == r.b)

  # for idx, cell in grid.grid.columns(pos.row):
  #   if idx > pos.col:
  #     if cell.valid:
  #       finish =

func makeCell*[T](
  arg: T, w, h: int,
  sizes: (int, int) = (1, 1),
  policies: (SizePolicy, SizePolicy) = (spExpanding, spExpanding)
                ): GridCell[T] =
  GridCell[T](
    isItem: true,
    item: arg, size: makeSize(w, h),
    rows: sizes[0],
    cols: sizes[1],
    vertPolicy: policies[0],
    horizPolicy: policies[1]
  )

func makeCell*(text: string): GridCell[string] =
  makeCell(arg = text, w = text.len, h = 1)

func makeCell*(text: StrSeq): GridCell[StrSeq] =
  makeCell(
    text,
    w = text.mapIt(it.len).max(),
    h = text.len
  )

func makeGrid*[T](arg: SparseGrid[GridCell[T]]): BlockGrid[T] =
  var maxColw: CountTable[int]
  var maxIdx: int = 0
  for (rowIdx, row) in arg.rows:
    for colIdx, cell in row:
      if maxColw[colIdx] < cell.width:
        maxColw[colIdx] = cell.width

      if colIdx > maxIdx:
        maxIdx = colIdx

  result = BlockGrid[T](
    grid: arg,
    maxW: (0 .. maxIdx).mapIt(maxColw[it]),
    maxH: arg.mapItRows(it.mapPairs(rhs.height).max(0)).mapIt(
      it.val
    )
  )

func makeGrid*[T](arg: SparseGrid[tuple[item: T, w, h: int]]): BlockGrid[T] =
  makeGrid(mapIt2d(arg, it.item.makeCell(it.w, it.h)))

func makeGrid*(arg: SparseGrid[string]): BlockGrid[string] =
  makeGrid(arg.mapIt2d(makeCell(it, it.len, 1)))

func makeGrid*(arg: SparseGrid[seq[string]]): BlockGrid[seq[string]] =
  makeGrid(arg.mapIt2d(makeCell(
    it, it.mapIt(it.len).max(0), it.len
  )))

func addHeader*[T](grid: var BlockGrid[T], cell: GridCell[T]): void =
  var cell = cell
  cell.vertPolicy = spExpanding
  cell.horizPolicy = spExpanding

  grid.grid.prepend(@[cell])

# func makeGridItem[T](arg: T): BlockGrid[T] =
#   BlockGrid[T](isItem: true, item: arg)

func toStringGrid*[T](grid: BlockGrid[T]): BlockGrid[StrSeq] =
  # let newgrid: Seq2d[StrSeq] =
  makeGrid(
    grid.grid.mapIt2d(makeCell(($it).split("\n")))
  )

func toString*(grid: BlockGrid[StrSeq]): string =
  var colSizes: Table[Range, int]
  var rowSizes: Table[Range, int]

  for (rowIdx, row) in grid.grid.rows():
    for colIdx, cell in row:
      let colRange = grid.colRange((rowIdx, colIdx))
      if colRange in colSizes:
        if colRange.isPoint():
          if cell.width > colSizes[colRange]:
            colSizes[colRange] = cell.width



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

proc getFields*(node: NimNode): seq[Field[NimNode]]

proc getBranches(node: NimNode): seq[FieldBranch[NimNode]] =
  assert node.kind == nnkRecCase, &"Cannot get branches from node kind {node.kind}"
  let caseType = $node[0][1]
  for branch in node[1..^1]:
    case branch.kind:
      of nnkOfBranch:
        result.add FieldBranch[NimNode](
          ofValue: branch[0] # ObjTree[NimNode](
            # kind: okConstant, constType: caseType, strLit: $branch[0].toStrLit())
          ,
          flds: branch[1].getFields(),
          isElse: false
        )
      of nnkElse:
        result.add FieldBranch[NimNode](
          flds: branch[0].getFields(), isElse: true
        )
      else:
        raiseAssert(&"Unexpected branch kind {branch.kind}")


proc getFieldDescription(node: NimNode): tuple[name, fldType: string] =
  case node.kind:
    of nnkIdentDefs:
      return (
        name: $node[0],
        fldType: $(node[1].toStrLit)
      )
    of nnkRecCase:
      return getFieldDescription(node[0])

    else:
      raiseAssert(
        &"Cannot get field description from node of kind {node.kind}")

proc getFields*(node: NimNode): seq[Field[NimNode]] =
  case node.kind:
    of nnkObjConstr:
      # echo node.treeRepr()
      return getFields(node[0])
    of nnkSym, nnkCall, nnkDotExpr:
      let kind = node.getTypeImpl().kind
      case kind:
        of nnkBracketExpr:
          let typeSym = node.getTypeImpl()[1]
          # echo "Type symbol: ", typeSym.treeRepr()
          # echo "Impl: ", typeSym.getTypeImpl().treeRepr()
          result = getFields(typeSym.getTypeImpl())
        of nnkObjectTy, nnkRefTy:
          result = getFields(node.getTypeImpl())
        else:
          raiseAssert("Unknown parameter kind: " & $kind)
    of nnkObjectTy:
      return node[2].getFields()
    of nnkRecList:
      for elem in node:
        let descr = getFieldDescription(elem)
        case elem.kind:
          of nnkRecCase: # Case field
            result.add Field[NimNode](
              isKind: true,
              branches: getBranches(elem),
              name: descr.name,
              fldType: descr.fldType
            )

          of nnkIdentDefs: # Regular field definition
            result.add getFields(elem)[0]

          else:
            discard

        # result &= elem.getFields()

    of nnkIdentDefs:
      let descr = getFieldDescription(node)
      result.add Field[NimNode](
        isKind: false,
        name: descr.name,
        fldType: descr.fldType
      )
    else:
      # echo node.getTypeImpl().toStrLit()
      raiseAssert(
        &"Unexpected node kind in `getFields` {node.kind}. Code: `{$node.toStrLit()}`")

proc getKindFields*[Node](flds: seq[Field[Node]]): seq[Field[Node]] =
  for fld in flds:
    if fld.isKind:
      result.add Field[Node](
        isKind: true,
        name: fld.name,
        value: fld.value,
        fldType: fld.fldType,
        branches: fld.branches.mapIt(
          FieldBranch[Node](
            value: it.value,
            isElse: it.isElse,
            flds: it.flds.getKindFields()
          )
        ).filterIt(it.flds.len > 0)
      )

proc discardNimNode(input: seq[Field[NimNode]]): seq[ValField] =
  for fld in input:
    case fld.isKind:
      of true:
        result.add ValField(
          isKind: true,
          name: fld.name,
          fldType: fld.fldType,
          selected: fld.selected,
          branches: fld.branches.mapIt(
            ValFieldBranch(
              value: ValObjTree(
                kind: okConstant,
                constType: (it.isElse).tern("", fld.fldType),
                strLit: (it.isElse).tern("", $it.ofValue.toStrLit())
              ),
              isElse: it.isElse,
              flds: it.flds.discardNimNode()
            )
          )
        )

      of false:
        result.add ValField(
          isKind: false,
          name: fld.name,
          fldType: fld.fldType
        )

macro makeFieldsLiteral*(node: typed): seq[ValField] =
  result = newLit(node.getFields().discardNimNode)


proc unrollFieldLoop(
  flds: seq[Field[NimNode]],
  body: NimNode,
  fldIdx: int,
  genParam: tuple[
    lhsObj, rhsObj, lhsName, rhsName, idxName, isKindName, fldName: string]
     ): tuple[node: NimNode, fldIdx: int] =

  result.node = newStmtList()
  var fldIdx: int = fldIdx
  for fld in flds:
    var tmpRes = newStmtList()
    # echo &"Fld idx: {fldIdx} for {fld.name}"
    let lhsId = ident(genParam.lhsName)
    let rhsId = ident(genParam.rhsName)
    let fldId = ident(fld.name)
    tmpRes.add superquote do:
      let `ident(genParam.isKindName)`: bool = `newLit(fld.isKind)`
      let `ident(genParam.idxName)`: int = `newLit(fldIdx)`
      let `lhsId` = `ident(genParam.lhsObj)`.`fldId`
      let `rhsId` = `ident(genParam.rhsObj)`.`fldId`
      let `ident(genParam.fldName)`: string = `newLit(fld.name)`
      block:
        `body`

    inc fldIdx
    if fld.isKind:
      # TODO check if two field kinds are identical
      var caseBlock = nnkCaseStmt.newTree(
        newDotExpr(ident genParam.lhsObj, ident fld.name)
      )

      for branch in fld.branches:
        let (branchBody, lastIdx) =
          branch.flds.unrollFieldLoop(body, fldIdx, genParam)

        fldIdx = lastIdx
        caseBlock.add nnkOfBranch.newTree(
          branch.ofValue,
          newStmtList(
            branchBody
          )
        )

      tmpRes.add superquote do:
        if `ident(genParam.lhsObj)`.`fldId` == `ident(genParam.rhsObj)`.`fldId`:
          `caseBlock`

    result.node.add quote do:
      block:
        `tmpRes`


  result.node = newBlockStmt(result.node)
  result.fldIdx = fldIdx


macro parallelFieldPairs*(lhsObj, rhsObj: typed, body: untyped): untyped =
  ##[

Iterate two objects in parallel. Works for case objects.

Similar to parallel `fieldPairs` but also works for case objects.
Allows to iterate two objects at once, while keeping track of `kind`
fields for each type. The body is unrolled and variables are injected
for each field.

Injected variables
------------------

name
  name of the current field
lhs, rhs
  value of current fields
fldIdx
  int. Index of current field in the object.
lshObj, rhsObj
  Original objects being iterated. [1]_
isKind
  bool. Whether or not current field is used as case parameter for object


[1] Useful when iterating over results of expression

  ]##

  let genParams = (
    lhsObj: "lhsObj",
    rhsObj: "rhsObj",
    lhsName: "lhs",
    rhsName: "rhs",
    idxName: "fldIdx",
    isKindName: "isKind",
    fldName: "name"
  )

  let (unrolled, _) = getFields(lhsObj).unrollFieldLoop(body, 0, genParams)

  result = superquote do:
    block:
      let `ident(genParams.lhsObj)` = `lhsObj`
      let `ident(genParams.rhsObj)` = `rhsObj`
      `unrolled`

 # echo result.toStrLit()

func isKVpairs(obj: ObjTree): bool =
  ## Check if entry should be printed as list of key-value pairs
  obj.kind == okTable or (obj.kind == okComposed and obj.namedFields)

import json

proc dedicatedConvertMatcher*[Obj](
  val: Obj, conv: proc(obj: Obj): ObjTree): ObjTree =
  ## Helper proc to correctly resolve overloading for pretty print
  ## converters
  return conv(val)

proc prettyPrintConverter(val: JsonNode, path: seq[int] = @[0]): ValObjTree =
  ## Dedicated pretty-print converter implementation for `JsonNode`
  case val.kind:
    of JNull:
      return ValObjTree(
        constType: "nil", kind: okConstant, strLit: "null")
    of JBool:
      return ValObjTree(
        constType: "bool", kind: okConstant, strLit: $val.getBool())
    of JInt:
      return ValObjTree(
        constType: "int", kind: okConstant, strLit: $val.getInt())
    of JFloat:
      return ValObjTree(
        constType: "string", kind: okConstant, strLit: $val.getFloat())
    of JString:
      return ValObjTree(
        constType: "string", kind: okConstant, strLit: &"\"{val.getStr()}\"")
    of JArray:
      return ValObjTree(
        kind: okSequence,
        valItems: val.getElems().mapPairs(
          prettyPrintConverter(rhs, path = path & @[idx])
        )
      )
    of JObject:
      return ValObjTree(
        kind: okComposed,
        namedFields: true,
        namedObject: false,
        sectioned: false,
        fldPairs: val.getFields().mapPairs((
          name: lhs,
          value: prettyPrintConverter(rhs, path = path & @[idx])
        )))

template unref(val: untyped): untyped =
  when val is ref: val[]
  else: val

func checkPrimitive(tree: ObjTree): bool =
  ##[

Check if tree can be considered primitive.

NOTE: values are not checked for primitivenes recursively. Instead
`isPrimitive` field is used.

  ]##
  case tree.kind:
    of okConstant:
      tree.constType in @["string", "int", "float"]
    of okSequence:
      (tree.valItems.len < 5) and
      tree.valItems.allOfIt(it.isPrimitive)
    of okTable:
      (tree.valPairs.len < 5) and
      tree.valPairs.allOfIt(it.val.isPrimitive)
    of okComposed:
      if tree.sectioned:
        # NOTE IMPLEMENT objects with sectioned case fields are not
        # currently supported.
        tree.kindBlocks.len < 5 and
        true
      else:
        (tree.fldPairs.len < 5) and
        tree.fldPairs.allOfIt(it.value.isPrimitive)


proc toSimpleTree*[Obj](
  entry: Obj,
  idCounter: var iterator(): int,
  conf: PPrintConf = PPrintConf(),
  path: seq[int] = @[0]): ValObjTree =
  ## Top-level dispatch for pretty-printing
  ##
  ## Generic implementation for pretty-print conveter for types not
  ## implementing dedicated `prettyPrintConverter`
  mixin prettyPrintConverter
  defer:
    result.isPrimitive = result.checkPrimitive()

  when compiles(prettyPrintConverter(entry, path = path)):
    # If dedicated implementation exists, use it
    return prettyPrintConverter(entry, path = path)
  elif not (
      (entry is seq) or
      (entry is array) or
      (entry is openarray) or
      (entry is string)
    ) and compiles(for k, v in pairs(entry): discard):

    result = ValObjTree(
      kind: okTable,
      keyType: $typeof((pairs(entry).nthType1)),
      valType: $typeof((pairs(entry).nthType2)),
      path: path,
      objId: idCounter()
    )

    for key, val in pairs(entry):
      result.valPairs.add(($key, toSimpleTree(val,
          idCounter
      )))

  elif not (
      (entry is string)
    ) and (
    (compiles(for i in items(entry): discard)) or
    (compiles(for i in items(entry[]): discard))
  ):
    result = ValObjTree(
      kind: okSequence,
      itemType: $typeof(items(unref entry)),
      objId: (entry is ref).tern(
        cast[int](unsafeAddr entry),
        idCounter()
      )
    )

    var idx: int = 0
    for it in items(unref entry):
      result.valItems.add(toSimpleTree(
        it, path = path & @[idx],
        idCounter = idCounter
      ))
      inc idx

  elif (entry is object) or
       (entry is ref object) or
       (entry is tuple):
    let id = when entry is ref: addr entry else: idCounter()
    when (entry is object) or (entry is ref object):
      result = ValObjTree(
        kind: okComposed,
        name: $typeof(Obj),
        sectioned: false,
        namedObject: true,
        namedFields: true,
        objId: id
      )
    elif isNamedTuple(Obj):
      result = ValObjTree(
        kind: okComposed,
        name: $typeof(Obj),
        sectioned: false,
        namedObject: false,
        namedFields: true,
        objId: id
      )
    else:
      result = ValObjTree(
        kind: okComposed,
        sectioned: false,
        namedFields: false,
        namedObject: false,
        objId: id
      )

    result.path = path

    when (entry is ref object):
      if entry == nil:
        result = ValObjTree(
          kind: okConstant,
          constType: $(typeof(Obj)),
          strLit: "nil",
          objId: idCounter()
        )
      else:
        var idx: int = 0
        for name, value in entry[].fieldPairs():
          result.fldPairs.add((name, toSimpleTree(
            value, path = path & @[idx],
            idCounter = idCounter
          )))
          inc idx
    else:
      var idx: int = 0
      for name, value in entry.fieldPairs():
        result.fldPairs.add((name, toSimpleTree(
          value,
          path = path & @[idx],
          idCounter = idCounter
        )))
        inc idx


  elif (entry is proc):
    result = ValObjTree(
      kind: okConstant,
      constType: $(typeof(Obj)),
      strLit: $(typeof(Obj)),
      path: path,
      objId: idCounter()
    )
  else:
    when entry is string:
      let val = "\"" & entry & "\""
    elif entry is pointer:
      let val = "<pointer>"
    else:
      let val = $entry

    result = ValObjTree(
      kind: okConstant,
      constType: $typeof(Obj),
      strLit: val,
      path: path,
      objId: idCounter()
    )



type

  Chunk* = object
    content: seq[string] ## Lines for chunk
    maxWidth: int ## Max line lenght in chunk



type
  RelPos* = enum
    ## Relative position of label to chunk
    rpBottomRight
    # [|||||||]
    # [|||||||] <label>
    rpTopLeftAbove
    # <label>
    #   [|||||||]
    #   [|||||||]
    rpTopLeftLeft
    # <label> [|||||||]
    #         [|||||||]
    rpBottomLeft
    #   [|||||||]
    #   [|||||||]
    # <label>
    rpPrefix
    # <label>[|||||||]
    # <label>[|||||||]

proc lineCount(c: Chunk): int = c.content.len()
proc `$`*(c: Chunk): string = c.content.join("\n")

func multiline(chunk: Chunk): bool = chunk.content.len > 1
func empty(conf: Delim): bool = conf.content.len == 0
func makeDelim*(str: string, multiline: bool = false): Delim =
  Delim(
    # appendNew: str.startsWith('\n'),
    # prependNew: str.endsWith('\n'),
    content: str.strip(chars = {'\n'}),
    preferMultiline: multiline
  )

proc makeChunk*(content: seq[string]): Chunk =
  ## Create chunk from list of lines
  Chunk(
    content: content,
    maxWidth: content.mapIt(it.len()).max(0)
  )

proc makeChunk*(content: string): Chunk =
  ## Create chunk from string
  Chunk(content: @[content], maxWidth: content.len)

proc makeChunk*(other: seq[Chunk]): Chunk =
  ## Create chunk from lines in other chunks
  result = Chunk(
    content: other.mapIt(it.content).concat()
  )

  result.maxWidth = result.content.mapIt(it.len()).max(0)

type
  ChunkLabels* = Table[RelPos, tuple[text: string, offset: int]]


proc relativePosition*(
  chunk: Chunk,
  labelsIn:
    ChunkLabels |
    seq[(RelPos, tuple[text: string, offset: int])],
  ignored: set[RelPos] = {}): Chunk =
  ## Position mutliple labels arounc chunk. Labels on `ignored`
  ## positions will be ingored

  let labels: ChunkLabels =
    when labelsIn is Table:
      labelsIn
    else:
      labelsIn.toTable()

  var
    leftPad = 0
    rightPad = 0
    topPad = 0
    bottomPad = 0

  let prefixOverride = (rpPrefix in labels)

  if prefixOverride and (rpTopLeftLeft in labels or rpBottomRight in labels):
    raiseAssert("Incompatible chunk labels - prefix&top-left-left or prefix&bottom-right")

  for pos, label in labels:
    with label:
      match(pos):
        rpBottomRight:
          rightPad = text.len
        rpTopLeftAbove:
          topPad = 1
          leftPad = max(offset, leftPad)
        rpTopLeftLeft:
          leftPad = max(leftPad, text.len)
        rpBottomLeft:
          bottomPad = 1
          leftPad = max(leftPad, offset)
        rpPrefix:
          leftPad = text.len

  let resWidth = chunk.maxWidth + leftPad + rightPad
  let resHeight = chunk.lineCount + topPad + bottomPad

  var resLines: seq[string]
  if rpTopLeftAbove in labels:
    resLines.add labels[rpTopLeftAbove].text

  let pref =
    if prefixOverride:
      labels[rpPrefix].text
    else:
      " ".repeat(leftPad)

  for idx, line in chunk.content:
    if idx == 0 and (rpTopLeftLeft in labels):
      resLines.add(labels[rpTopLeftLeft].text & line)
    else:
      resLines.add(pref & line)

  if (rpBottomRight in labels) and
     (rpBottomRight notin ignored) and
     (resLines.len > 0):
    resLines[^1] &= labels[rpBottomRight].text

  if rpBottomLeft in labels:
    resLines.add labels[rpBottomLeft].text

  return makeChunk(resLines.mapIt(it.strip(leading = false)))


template echov(variable: untyped, other: varargs[string, `$`]): untyped =
  let pref = "  ".repeat(getStackTraceEntries().len)

  when variable is string:
    echo pref, astToStr(variable), ": \"", variable, "\" ", other.join(" ")
  else:
    echo pref, astToStr(variable), ": ", variable, " ", other.join(" ")

proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): Chunk

proc getWrapperConf(current: ObjTree, conf: PPrintConf): tuple[start, final: Delim] =
  let (wrapBeg, wrapEnd) = # Delimiters at the start/end of the block
    case current.kind:
      of okComposed:
        if current.namedObject:
          var objWrap = conf.objWrapper
          objWrap.start.content = &"{current.name}{objWrap.start.content}"
          objWrap
        else:
          conf.objWrapper
      of okSequence:
        conf.seqWrapper
      of okTable:
        # TODO use configurable wrapper begin/end
        conf.tblWrapper
      else:
        assert false, "Cannot arrange kv pair in constant"
        (makeDelim("."), makeDelim("."))

  return (wrapBeg, wrapEnd)


proc getLabelConfiguration(
  conf: PPrintConf, current: ValObjTree, ident: int): tuple[
  item, blocks: ChunkLabels,
  widthconf: (int, int)] =
  ## Get label configuration

  let (wrapBeg, wrapEnd) = getWrapperConf(current, conf)
  var subIdent = 0 # Required right margin for field values
  var maxWidth = 0 # Max allowed withd
  var itemLabels: ChunkLabels
  var blockLabels: ChunkLabels
  let offset = conf.identStr.len # Internal offset between prefix
                                 # delimiter and chunk body
  let prefixOverride = (current.kind == okSequence) and (conf.seqPrefix.len > 0)
  match((prefixOverride, wrapBeg.preferMultiline, wrapEnd.preferMultiline)):
    (true, _, _):
      subIdent = conf.seqPrefix.len
      maxWidth = conf.maxWidth
      itemLabels[rpTopLeftLeft] = (text: conf.seqPrefix, offset: 0)
    (_, true, true):
      # (prefix delimiter)
      # <field> [ block block
      #           block block ]
      # (suffix delimiter)
      subIdent = ident
      maxWidth = conf.maxWidth
      itemLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: offset)
      itemLabels[rpBottomLeft] = (text: wrapEnd.content, offset: offset)
    (_, true, false):
      # (prefix delimiter)
      # <field> [ block block
      #           block block ] (suffix delimiter)
      subIdent = ident
      maxWidth = conf.maxWidth - wrapEnd.content.len()
      itemLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: offset)
      itemLabels[rpBottomRight] = (text: wrapEnd.content, offset: 0)
    (_, false, true):
      # (prefix delimiter) <field> [ block block
      #                              block block ]
      # (suffix delimiter)
      subIdent = ident + wrapBeg.content.len()
      # IMPLEMENT account for sequence prefix, kvSeparator etc.
      maxWidth = conf.maxWidth
      itemLabels[rpTopLeftLeft] = (text: wrapBeg.content, offset: 0)
      itemLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 0)
    (_, false, false):
      # (prefix delimiter) <field> [ block block
      #                              block block ] (suffix delimiter)
      subIdent = ident + wrapBeg.content.len()
      maxWidth = conf.maxWidth - wrapEnd.content.len()
      case current.kind:
        of okSequence:
          itemLabels[rpBottomRight] = (text: conf.seqSeparator, offset: 0)
          blockLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: 2)
          blockLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 2)
        of {okTable, okComposed}:
          itemLabels[rpBottomRight] = (text: conf.fldSeparator, offset: 0)
          # TODO more fine-grained configuration for different
          # wrapping settings.
          if not conf.nowrapMultiline:
            blockLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: 2)
            blockLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 2)
          elif current.kind == okComposed and current.namedObject:
            blockLabels[rpTopLeftAbove] = (text: current.name, offset: 2)
        else:
          discard

  return (
    item: itemLabels,
    blocks: blockLabels,
    widthconf: (subIdent, offset)
  )



proc arrangeKVPairs(
  input: seq[tuple[name: string, val: Chunk]],
  conf: PPrintConf, current: ObjTree, ident: int): Chunk =
  ## Layout sequence of key-value pairs. `name` field in tuple items
  ## might be empty if `current.kind` is `okSequence`.

  let (wrapBeg, wrapEnd) = getWrapperConf(current, conf)

  let trySingleLine = (not input.anyOfIt(it.val.multiline()))

  if trySingleLine:
    var singleLine =
      if current.kind == okSequence or (
        current.kind == okComposed and (not current.namedFields)):
        input.mapIt(&"{it.val.content[0]}").join(conf.seqSeparator)
      else:
        input.mapIt(&"{it.name}{conf.kvSeparator}{it.val.content[0]}").join(
          conf.seqSeparator)

    singleLine = wrapBeg.content & singleLine & wrapEnd.content

    if singleLine.len < (conf.maxWidth - ident):
      return makeChunk(content = @[singleLine])

  # Try positioning on multiple lines
  let fldsWidth =
    # Width of the larges field
    if current.isKVPairs():
      input.mapIt(it.name.len()).max() + conf.kvSeparator.len()
    else:
      0

  proc makeFldName(it: tuple[name: string, val: Chunk]): (RelPos, (string, int)) =
    let pos = case current.kind:
      of okSequence: rpTopLeftLeft
      of okTable, okComposed: (it.val.multiline()).tern(rpTopLeftAbove, rpTopLeftLeft)
      of okConstant: rpPrefix

    let text = case current.kind:
      of okComposed, okTable:
        if conf.alignFieldsRight:
          align(it.name & conf.kvSeparator, fldsWidth)
        else:
          alignLeft(it.name & conf.kvSeparator, fldsWidth)
      of okSequence:
        ""
      of okConstant:
        raiseAssert("Invalid current kind: constant")

    return (pos, (text, conf.identStr.len()))

  let (itemLabels, blockLabels, widthConf) =
    getLabelConfiguration(conf = conf, current = current, ident = ident)

  return input.enumerate().mapIt(
    relativePosition(it[1].val, (@[makeFldName(it[1])])).
    relativePosition(
      itemLabels,
      ignored = (it[0] == input.len() - 1).tern(
        {rpBottomRight},
        {})))
  .makeChunk()
  .relativePosition(blockLabels)



proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): Chunk =
  case current.kind:
    of okConstant:
      return makeChunk(content = @[ current.strLit ])
    of okComposed:
      if not current.sectioned:
        let maxFld = current.fldPairs.mapIt(
          it.name.len() + conf.fldNameWrapper.start.content.len() +
          conf.fldNameWrapper.start.content.len()
        ).max()

        result = current.fldPairs.mapIt(
          (
            conf.fldNameWrapper.start.content & it.name &
              conf.fldNameWrapper.final.content,
            pstringRecursive(it.value, conf, maxFld + ident)
          )
        ).arrangeKVPairs(conf, current, ident + maxFld)
    of okTable:
      let maxFld = current.valPairs.mapIt(it.key.len()).max(0)
      return current.valPairs.mapIt(
        (it.key, pstringRecursive(it.val, conf, maxFld + ident))
      ).arrangeKVPairs(conf, current, ident + maxFld)
    of okSequence:
      let tmp = current.valItems.mapIt(
        ("", pstringRecursive(it, conf, ident + conf.seqPrefix.len()))
      )
      result = tmp.arrangeKVPairs(conf, current, ident)

proc prettyString*(tree: ObjTree, conf: PPrintConf, ident: int = 0): string =
  ## Convert object tree to pretty-printed string using configuration `conf`
  var newConf = conf
  newConf.maxWidth = conf.maxWidth - ident
  pstringRecursive(tree, newConf, ident = ident).content.mapIt(
    " ".repeat(ident) & it).join("\n")


const objectPPrintConf = PPrintConf(
  maxWidth: 80,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("("), makeDelim(")")),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": ",
  nowrapMultiline: true
)

type
  DotGenConfig = object
    f1: int

proc toGrid*(obj: ObjTree, topId: NodeId): tuple[
  grid: BlockGrid[ObjElem],
  edges: seq[(NodeId, NodeId)]] =
  # If object is primitive convert it into grid. All non-primitive
  # child objects will be converted into port nodes - no content is
  # present, only port for connecting to external node. For each such
  # node edge is added from current node's output port to other node.

  # assert obj.isPrimitive
  echo &"converting object {obj.objId} to grid"
  # echo obj
  case obj.kind:
    of okConstant:
      if obj.isPrimitive:
        # REVIEW handle non-primitive constants
        result.grid = makeGrid(
          @[@[
            (
              item: makeObjElem(obj.constType),
              w: obj.constType.len,
              h: 1
            ), # First cell - object type
            (
              item: makeObjElem(obj.strLit),
              w: obj.strLit.len,
              h: 1
            ) # Secon cell - object value
          ]])
    else:
      discard

  discard

# fold object into grid, export grid into html table, convert html
# table into graphviz object.


proc toHtml*(grid: SparseGrid[GridCell[ObjElem]]): HtmlElem =
  discard

proc toTable*(grid: BlockGrid[ObjElem]): HtmlElem
proc toHtml*(cell: GridCell[ObjElem]): HtmlElem =
  case cell.isItem:
    of true:
      HtmlElem(
        kind: hekCell,
        cellBgColor: cell.item.color,
        elements: @[
          HtmlElem(
            kind: hekText,
            textStr: cell.item.text)])
    of false:
      cell.grid.toTable()

proc toTable*(grid: BlockGrid[ObjElem]): HtmlElem =
  grid.grid.toHtml()

proc foldObject(obj: ObjTree): tuple[node: Node, edges: seq[Edge]] =
  ##[

Recurisvely convert `ObjTree` into graphviz html-like node.

All primitive subitems are embedded into resulting node. All other
elements as converted into edges.

  ]##
  let (grid, edges) = obj.toGrid(obj.objId)
  result.node = Node(
    shape: nsaPlaintext,
    id: obj.objId,
    htmlLabel: grid.toTable()
  )

  for (src, to) in edges:
    result.edges.add Edge(
      src: src,
      to: @[ to ]
    )

func getSubnodes*(it: ObjTree): seq[ObjTree] =
  debugecho &"get subnodes for tree id {it.objId}"
  case it.kind:
    of okConstant: raiseAssert("No sub for `okConstant`")
    of okSequence: it.valItems
    of okTable: it.valPairs.mapIt(it.val)
    of okComposed:
      case it.sectioned:
        of false: it.fldPairs.mapIt(it.value)
        of true: raiseAssert(
          "IMPLEMENT Sectioned objects are not supported RN")

proc toDotGraph*[Obj](obj: Obj, conf: DotGenConfig = DotGenConfig()): Graph =
  var counter =
    iterator(): int {.closure.} =
      var cnt: int = 0
      while true:
        yield cnt
        inc cnt

  let tree = toSimpleTree(obj, counter)
  # TO whoever reading this: I had to use life support system to not
  # die of brain overload. Just saying.
  var folded = tree.mapItTreeDFS(
    outType = seq[Var2[Edge, Node]],
    hasSubnodes = (it.kind != okConstant),
    subnodeCall = it.getSubnodes(),
    op =
      block:
        let (node, edges) = it.foldObject()
        @[ toVar2[Edge, Node](node) ] &
          toVar2[Edge, Node](edges) &
          subt.concat()
  )

  # if tree.isPrimitive: # If toplevel object is primitive add
  #   let (node, edges) = tree.foldObject()
  #   folded.add node
  #   folded &= edges

  result = Graph(
    nodes: folded.filterIt(it.hasType(Node)).mapIt(it.get(Node)),
    edges: folded.filterIt(it.hastype(Edge)).mapIt(it.get(Edge))
  )



proc pstring*[Obj](obj: Obj, ident: int = 0, maxWidth: int = 80): string =
  var counter =
    iterator(): int {.closure.} =
      var cnt: int = 0
      while true:
        yield cnt
        inc cnt

  var conf = objectPPrintConf
  conf.maxWidth = maxWidth
  prettyString(toSimpleTree(obj, counter), conf, ident)

proc pprint*[Obj](obj: Obj, ident: int = 0, maxWidth: int = 80): void =
  echo pstring(obj, ident,  maxWidth)
  # var conf = objectPPrintConf
  # conf.maxWidth = maxWidth
  # echo prettyString(toSimpleTree(obj), conf, ident)
