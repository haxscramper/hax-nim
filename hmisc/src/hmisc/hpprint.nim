# TODO account for control codes in stings

import hmisc/[helpers, defensive]
import tables, sequtils, math, strutils, strformat
import typetraits

import gara, with

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
        namedObject: bool
        namedFields: bool
        name: string
        case sectioned: bool
          of false:
            # Simpler representation for object tree without
            # sectioning on different blocks depending on `kind`
            # fields: everything is put into single key-value
            # sequence.

            # TODO Add field type
            fldPairs: seq[tuple[name: string, value: ObjTree]]
          of true:
            # Most of the case objects have one `kind` field named
            # 'kind' but this should account for cases with multiple
            # case fields as well as nested ones
            kindBlocks: seq[Field]

func isKVpairs(obj: ObjTree): bool =
  obj.kind == okTable or (obj.kind == okComposed and obj.namedFields)

import json

proc dedicatedConvertMatcher[Obj](
  val: Obj, conv: proc(obj: Obj): ObjTree): ObjTree =
  return conv(val)

proc prettyPrintConverter(val: JsonNode): ObjTree =
  case val.kind:
    of JNull:
      return ObjTree(
        constType: "nil", kind: okConstant, strLit: "null")
    of JBool:
      return ObjTree(
        constType: "bool", kind: okConstant, strLit: $val.getBool())
    of JInt:
      return ObjTree(
        constType: "int", kind: okConstant, strLit: $val.getInt())
    of JFloat:
      return ObjTree(
        constType: "string", kind: okConstant, strLit: $val.getFloat())
    of JString:
      return ObjTree(
        constType: "string", kind: okConstant, strLit: &"\"{val.getStr()}\"")
    of JArray:
      return ObjTree(
        kind: okSequence,
        valItems: val.getElems().map(prettyPrintConverter)
      )
    of JObject:
      return ObjTree(
        kind: okComposed,
        namedFields: true,
        namedObject: false,
        sectioned: false,
        fldPairs: val.getFields().mapPairs((
          name: lhs,
          value: prettyPrintConverter(rhs)
        )))


proc toSimpleTree[Obj](entry: Obj): ObjTree =
  when compiles(dedicatedConvertMatcher[Obj](entry, prettyPrintConverter)):
    return dedicatedConvertMatcher[Obj](entry, prettyPrintConverter)
  elif not (
      (entry is seq) or
      (entry is array) or
      (entry is openarray) or
      (entry is string)
    ) and compiles(for k, v in pairs(entry): discard):
    result = ObjTree(
      kind: okTable,
      keyType: $typeof((pairs(entry).nthType1)),
      valType: $typeof((pairs(entry).nthType2))
    )


    for key, val in pairs(entry):
      result.valPairs.add(($key, toSimpleTree(val)))

  elif not (
      (entry is string)
    ) and (compiles(for i in items(entry): discard)):
    result = ObjTree(
      kind: okSequence,
      itemType: $typeof(items(entry))
    )

    for it in items(entry):
      result.valItems.add(toSimpleTree(it))

  elif (entry is object) or (entry is tuple):
    when entry is object:
      result = ObjTree(
        kind: okComposed,
        name: $typeof(Obj),
        sectioned: false,
        namedObject: true,
        namedFields: true
      )
    elif isNamedTuple(Obj):
      result = ObjTree(
        kind: okComposed,
        name: $typeof(Obj),
        sectioned: false,
        namedObject: false,
        namedFields: true
      )
    else:
      result = ObjTree(
        kind: okComposed,
        sectioned: false,
        namedFields: false,
        namedObject: false
      )

    for name, value in entry.fieldPairs():
      result.fldPairs.add((name, toSimpleTree(value)))
  else:
    when entry is string:
      let val = "\"" & entry & "\""
    else:
      let val = $entry

    result = ObjTree(
      kind: okConstant,
      constType: $typeof(Obj),
      strLit: val
    )


type
  Delim = object
    content: string ## String for delimiter
    # appendNew: bool ## Append delimiter as new chunk after or suffix
    # ## to existing one?

    # prependNew: bool ## Prepend delimiter as new chunk before or
    # ## prefix to existing one?

    preferMultiline: bool ## Don't try to put delimiter on the same
    ## line with content - always prefer new chunks

  DelimPair = tuple[
    start: Delim,
    final: Delim
  ]

  PPrintConf = object
    maxWidth: int
    identStr: string
    wrapLargerThan: int

    kvSeparator: string
    tblWrapper: DelimPair

    objWrapper: DelimPair
    fldNameWrapper: DelimPair
    fldSeparator: string

    seqSeparator: string
    seqPrefix: string
    seqWrapper: DelimPair

  Chunk = object
    content: seq[string]
    maxWidth: int



type
  RelPos = enum
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
proc `$`(c: Chunk): string = c.content.join("\n")

func multiline(chunk: Chunk): bool = chunk.content.len > 1
func empty(conf: Delim): bool = conf.content.len == 0
func makeDelim(str: string, multiline: bool = false): Delim =
  Delim(
    # appendNew: str.startsWith('\n'),
    # prependNew: str.endsWith('\n'),
    content: str.strip(chars = {'\n'}),
    preferMultiline: multiline
  )

proc makeChunk(content: seq[string]): Chunk =
  Chunk(
    content: content,
    maxWidth: content.mapIt(it.len()).max()
  )

proc makeChunk(content: string): Chunk =
  Chunk(content: @[content], maxWidth: content.len)

proc makeChunk(other: seq[Chunk]): Chunk =
  result = Chunk(
    content: other.mapIt(it.content).concat()
  )

  result.maxWidth = result.content.mapIt(it.len()).max()

type
  ChunkLabels = Table[RelPos, tuple[text: string, offset: int]]

proc relativePosition(
  chunk: Chunk,
  labelsIn:
    ChunkLabels |
    seq[(RelPos, tuple[text: string, offset: int])],
  ignored: set[RelPos] = {}): Chunk =

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

  return makeChunk(resLines)


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
  conf: PPrintConf, current: ObjTree, ident: int): tuple[
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
          blockLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: 0)
          blockLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 0)
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

  # echo "arranging sequence of items"
  # echov input
  # echov current.kind
  # defer:
  #   echov result

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

  proc makeFldName(name: string): string =
    case current.kind:
      of okComposed, okTable:
        align(name & conf.kvSeparator, fldsWidth)
      of okSequence:
        ""
      of okConstant:
        raiseAssert("Invalid current kind: constant")

  let (itemLabels, blockLabels, widthConf) =
    getLabelConfiguration(conf = conf, current = current, ident = ident)

  return input.enumerate().mapIt(
    relativePosition(
      it[1].val,
      (@{
        rpTopLeftLeft : (text: makeFldName(it[1].name), offset: 0)
      })
    ).
    relativePosition(
      itemLabels,
      ignored = (it[0] == input.len() - 1).tern(
        {rpBottomRight},
        {}
      )
    )
  )
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
      let maxFld = current.valPairs.mapIt(it.key.len()).max()
      return current.valPairs.mapIt(
        (it.key, pstringRecursive(it.val, conf, maxFld + ident))
      ).arrangeKVPairs(conf, current, ident + maxFld)
    of okSequence:
      let tmp = current.valItems.mapIt(
        ("", pstringRecursive(it, conf, ident + conf.seqPrefix.len()))
      )
      result = tmp.arrangeKVPairs(conf, current, ident)

proc prettyString(tree: ObjTree, conf: PPrintConf, ident: int = 0): string =
  ## Convert object tree to pretty-printed string
  pstringRecursive(tree, conf).content.join("\n")

type
  Obj1 = object
    f1: int
    f2: seq[int]
    f3: Table[int, string]

import unittest

suite "Library parts unit tests":
  template test(
    labels: untyped, chunkLines: seq[string] = @["[|||]"]): untyped =
    relativePosition(
      chunk = makeChunk(chunkLines), labels)

  test "Chunk label on left":
    assertEq $(test(
      @{ rpTopLeftLeft : (text: "<>", offset: 0) })), "<>[|||]"

  test "Chunk label on top left":
    assertEq $(test(
      @{ rpTopLeftAbove : (text: "<>", offset: 2) })), "<>\n  [|||]"

  test "Chunk label on bottom right":
    assertEq $(test(
      @{ rpBottomRight : (text: "<>", offset: 2) })), "[|||]<>"

  test "Chunk label on bottom left":
    assertEq $(test(
      @{ rpBottomLeft : (text: "<>", offset: 2) })), "  [|||]\n<>"

  test "Top-above & bottom left":
    assertEq $(test(
      @{ rpTopLeftAbove : (text: "{{", offset: 2),
         rpBottomLeft : (text: "}}", offset: 2)})),
         """
         {{
           [|||]
         }}""".dedent


  test "Multiline block compact":
    assertEq $(test(
      @{ rpBottomRight: (text: "}}", offset: 2),
         rpTopLeftLeft: (text: "{{", offset: 2)
         # Top left left offset should be ignored
       },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{[||||]
           [||||]}}""".dedent


  test "Multiline block expanded":
    assertEq $(test(
      @{ rpBottomLeft: (text: "}}", offset: 2),
         rpTopLeftAbove: (text: "{{", offset: 2)
         # Top left left offset should be ignored
       },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{
           [||||]
           [||||]
         }}""".dedent


  test "Multiline block expanded with prefix":
    assertEq $(test(
      @{ rpBottomLeft: (text: "}}", offset: 2),
         rpTopLeftAbove: (text: "{{", offset: 2),
         rpPrefix: (text: "- ", offset: 2)
         # Top left left offset should be ignored
       },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{
         - [||||]
         - [||||]
         }}""".dedent


  test "Invalid prefix assertion":
    try:
      discard test(@{
        rpTopLeftLeft: (text: "==", offset: 2),
        rpPrefix: (text: "--", offset: 2)
      })

      fail("Unreachable code")
    except AssertionError:
      assert getCurrentExceptionMsg().startsWith("Incompatible chunk labels")
    except:
      fail("Wrong exception")

var conf = PPrintConf(
  maxWidth: 40,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("(", multiline = true),
               makeDelim(")", multiline = false)),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": ",
  wrapLargerThan: 10
)

template pstr(arg: untyped): untyped =
  toSimpleTree(arg).prettyString(conf)

suite "Simple configuration":
  test "integer":
    assertEq pstr(12), "12"

  test "string":
    assertEq pstr("112"), "\"112\""

  test "Anonymous tuple":
    assertEq pstr((12, "sdf")), "(12, \"sdf\")"

  test "Named tuple":
    assertEq pstr((a: "12", b: "222")), "(a: \"12\", b: \"222\")"

  test "Narrow sequence":
    conf.maxWidth = 6
    assertEq @[1,2,3,4].pstr(), "- 1\n- 2\n- 3\n- 4"

  test "Wide sequence":
    conf.maxWidth = 80
    assertEq @[1,2,3].pstr(), "[1, 2, 3]"

  test "int-int table":
    assertEq {2: 3, 4: 5}.toOrderedTable().pstr(), "{2: 3, 4: 5}"

  test "Sequence of tuples":
    assertEq @[(1, 3), (4, 5)].pstr(), "[(1, 3), (4, 5)]"

  type
    T = object
      f1: int

  test "Simple object":
    assertEq T(f1: 12).pstr(), "T(f1: 12)"

  test "Sequence of objects":
    assertEq @[T(f1: 12), T(f1: -99)].pstr(), "[T(f1: 12), T(f1: -99)]"

  type
    C = object
      case kind: bool
      of true: f90: string
      of false: f09: (float, string)

  test "Case object":
    assertEq C(kind: true, f90: "12").pstr(), "C(kind: true, f90: \"12\")"
    assertEq C(kind: false, f09: (1.2, "12")).pstr(),
         "C(kind: false, f09: (1.2, \"12\"))"

suite "Deeply nested types":
  test "8D sequence":
    assertEq @[@[@[@[@[@[@[@[1]]]]]]]].pstr(),
         "[[[[[[[[1]]]]]]]]"

  test "4x4 seq":
    assertEq @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstr(), "[[1, 2, 3, 4], [5, 6, 7, 8], [9, 1, 2, 3], [4, 5, 6, 7]]"


  test "Narrow 4x4 seq":
    conf.maxWidth = 20
    assertEq @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstr(),  """
      - [1, 2, 3, 4]
      - [5, 6, 7, 8]
      - [9, 1, 2, 3]
      - [4, 5, 6, 7]""".dedent
    conf.maxWidth = 80


  test "Super narrow 2x2 seq":
    conf.maxWidth = 7
    assertEq @[
      @[1, 2, 4],
      @[5, 6, 8],
    ].pstr(), """
      - - 1
        - 2
        - 4
      - - 5
        - 6
        - 8""".dedent

    conf.maxWidth = 80

import json

suite "Printout json as object":
  test "Json named tuple":
    let jsonNode = parseJson("""{"key": 3.14}""")
    assertEq jsonNode.pstr(), "(key: 3.14)"

  test "Json array":
    assertEq parseJson("""{"key": [1, 2, 3]}""").pstr(),
        "(key: [1, 2, 3])"

  test "Json nested array":
    assertEq parseJson("""{"key": [[1, 2, 3], [1, 2, 3]]}""").pstr(),
        "(key: [[1, 2, 3], [1, 2, 3]])"


var jsonConf = PPrintConf(
  maxWidth: 80,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("{\n"), makeDelim("\n}")),
  fldNameWrapper: (makeDelim("\""), makeDelim("\"")),
  fldSeparator: ",",
  kvSeparator: ": ",
  wrapLargerThan: 10
)

template pjson(arg: untyped): untyped =
  toSimpleTree(arg).prettyString(jsonConf)

suite "Json pretty printing":
  test "Reparse int":
    let jsonNode = parseJson("""{"key": 3.14}""")
    let pretty = jsonNode.pjson()
    let reparsed = pretty.parseJson()
    assertEq jsonNode, reparsed

  test "Nested JSON":
    let jsonNode = parseJson """
       {
        "name":"John",
        "age":30,
        "cars": {
          "car1":"Ford",
          "car2":"BMW",
          "car3":"Fiat"
        }
       }""".dedent()

    let formatted = jsonNode.pjson()
    assertEq formatted, """
        {
          "name": "John",
           "age": 30,
          "cars": {"car1": "Ford", "car2": "BMW", "car3": "Fiat"}
        }""".dedent()

  test "Large JSON reparse":
      let jsonNode = parseJson """
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}        """

      assertEq jsonNode, jsonNode.pjson().parseJson()


var treeConf = PPrintConf(
  maxWidth: 40,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("("), makeDelim(")")),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": ",
  wrapLargerThan: 10
)

template treeStr(arg: untyped): untyped =
  toSimpleTree(arg).prettyString(treeConf)

suite "Large object printout":
  test "Large JSON as treeRepr":
    let jsonNode = parseJson """
      {"widget": {
          "debug": "on",
          "window": {
              "title": "Sample Konfabulator Widget",
              "name": "main_window",
              "width": 500,
              "height": 500
          },
          "image": {
              "src": "Images/Sun.png",
              "name": "sun1",
              "hOffset": 250,
              "vOffset": 250,
              "alignment": "center"
          },
          "text": {
              "data": "Click Here",
              "size": 36,
              "style": "bold",
              "name": "text1",
              "hOffset": 250,
              "vOffset": 100,
              "alignment": "center",
              "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
          }
      }}"""

    echo treeStr(jsonNode)
