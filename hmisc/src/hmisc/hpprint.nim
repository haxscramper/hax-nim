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
    ## Pretty print configuration
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

            # TODO Add field type
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
    of nnkSym:
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
      raiseAssert(
        &"Unexpected node kind in `getFields` {node.kind}")

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
fields for each type. The body is unrolled and correct variables are
injected for each field.

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

proc prettyPrintConverter(val: JsonNode): ValObjTree =
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
        valItems: val.getElems().map(prettyPrintConverter)
      )
    of JObject:
      return ValObjTree(
        kind: okComposed,
        namedFields: true,
        namedObject: false,
        sectioned: false,
        fldPairs: val.getFields().mapPairs((
          name: lhs,
          value: prettyPrintConverter(rhs)
        )))


proc toSimpleTree*[Obj](
  entry: Obj, conf: PPrintConf = PPrintConf()): ValObjTree =
  ## Generic implementation for pretty-print conveter for types not
  ## implementing dedicated `prettyPrintConverter`
  mixin prettyPrintConverter
  when compiles(prettyPrintConverter(entry)):
    # If dedicated implementation exists, use it
    return prettyPrintConverter(entry)
  elif not (
      (entry is seq) or
      (entry is array) or
      (entry is openarray) or
      (entry is string)
    ) and compiles(for k, v in pairs(entry): discard):
    result = ValObjTree(
      kind: okTable,
      keyType: $typeof((pairs(entry).nthType1)),
      valType: $typeof((pairs(entry).nthType2))
    )

    for key, val in pairs(entry):
      result.valPairs.add(($key, toSimpleTree(val)))

  elif not (
      (entry is string)
    ) and (compiles(for i in items(entry): discard)):
    result = ValObjTree(
      kind: okSequence,
      itemType: $typeof(items(entry))
    )

    for it in items(entry):
      result.valItems.add(toSimpleTree(it))

  elif (entry is object) or
       (entry is ref object) or
       (entry is tuple):
    when (entry is object) or (entry is ref object):
      result = ValObjTree(
        kind: okComposed,
        name: $typeof(Obj),
        sectioned: false,
        namedObject: true,
        namedFields: true
      )
    elif isNamedTuple(Obj):
      result = ValObjTree(
        kind: okComposed,
        name: $typeof(Obj),
        sectioned: false,
        namedObject: false,
        namedFields: true
      )
    else:
      result = ValObjTree(
        kind: okComposed,
        sectioned: false,
        namedFields: false,
        namedObject: false
      )

    when (entry is ref object):
      if entry == nil:
        result = ValObjTree(
          kind: okConstant,
          constType: $(typeof(Obj)),
          strLit: "nil")
      else:
        for name, value in entry[].fieldPairs():
          result.fldPairs.add((name, toSimpleTree(value)))
    else:
      for name, value in entry.fieldPairs():
        result.fldPairs.add((name, toSimpleTree(value)))


  elif (entry is proc):
    result = ValObjTree(
      kind: okConstant,
      constType: $(typeof(Obj)),
      strLit: $(typeof(Obj))
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
      strLit: val
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
    maxWidth: content.mapIt(it.len()).max()
  )

proc makeChunk*(content: string): Chunk =
  ## Create chunk from string
  Chunk(content: @[content], maxWidth: content.len)

proc makeChunk*(other: seq[Chunk]): Chunk =
  ## Create chunk from lines in other chunks
  result = Chunk(
    content: other.mapIt(it.content).concat()
  )

  result.maxWidth = result.content.mapIt(it.len()).max()

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

  if rpBottomRight in labels and (rpBottomRight notin ignored):
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


proc pstring*[Obj](obj: Obj, ident: int = 0, maxWidth: int = 80): string =
  var conf = objectPPrintConf
  conf.maxWidth = maxWidth
  prettyString(toSimpleTree(obj), conf, ident)

proc pprint*[Obj](obj: Obj, ident: int = 0, maxWidth: int = 80): void =
  echo pstring(obj, ident,  maxWidth)
  # var conf = objectPPrintConf
  # conf.maxWidth = maxWidth
  # echo prettyString(toSimpleTree(obj), conf, ident)
