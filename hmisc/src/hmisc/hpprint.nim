# TODO account for control codes in stings

import hmisc/helpers
import tables, sequtils, math, strutils, strformat
import typetraits

import gara

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
    appendNew: bool ## Append delimiter as new chunk after or suffix
    ## to existing one?

    prependNew: bool ## Prepend delimiter as new chunk before or
    ## prefix to existing one?

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

    seqSeparator: string
    seqPrefix: string
    seqWrapper: DelimPair

  Chunk = object
    content: seq[string]
    maxWidth: int

func multiline(chunk: Chunk): bool = chunk.content.len > 1
func empty(conf: Delim): bool = conf.content.len == 0
func makeDelim(str: string): Delim =
  Delim(
    appendNew: str.startsWith('\n'),
    prependNew: str.endsWith('\n'),
    content: str.strip(chars = {'\n'})
  )

proc makeChunk(content: seq[string]): Chunk =
  Chunk(
    content: content,
    maxWidth: content.mapIt(it.len()).max()
  )

proc makeChunk(other: seq[Chunk]): Chunk =
  result = Chunk(
    content: other.mapIt(it.content).concat()
  )

  result.maxWidth = result.content.mapIt(it.len()).max()

template echov(variable: untyped, other: varargs[string, `$`]): untyped =
  let pref = "  ".repeat(getStackTraceEntries().len)

  when variable is string:
    echo pref, astToStr(variable), ": \"", variable, "\" ", other.join(" ")
  else:
    echo pref, astToStr(variable), ": ", variable, " ", other.join(" ")

proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): Chunk

proc arrangeKVPair(
  name: string, chunk: Chunk, conf: PPrintConf,
  nameWidth: int, header: string = "", kind: ObjKind = okComposed): Chunk =
  let prefWidth =
    case kind:
      of okComposed: nameWidth + conf.kvSeparator.len()
      else: nameWidth + conf.seqPrefix.len()

  let pref =
    case kind:
      of okComposed: name & conf.kvSeparator
      else: conf.seqPrefix

  if nameWidth > conf.wrapLargerThan or
     (chunk.maxWidth + prefWidth) > conf.maxWidth:
    # Put chunk and name on separate lines
    return makeChunk(@[pref] &
      chunk.content.mapIt(" ".repeat(prefWidth) & it)
    )
  else:
    # Put chunk on the same line as name
    return makeChunk(@[
      (" ".repeat(prefWidth - pref.len())) & pref & chunk.content[0]
    ] &
      chunk.content[1..^1].mapIt(" ".repeat(prefWidth) & it)
    )


proc arrangeKVPairs(
  input: seq[tuple[name: string, val: Chunk]],
  conf: PPrintConf, current: ObjTree, ident: int): Chunk =
  ## Layout sequence of key-value pairs. `name` field in tuple items
  ## might be empty if `current.kind` is `okSequence`.
  let (wrapBeg, wrapEnd) = # Delimiters at the start/end of the block
    case current.kind:
      of okComposed:
        if current.namedObject:
          var objWrap = conf.objWrapper
          objWrap.start.content = "{current.name}{objWrap.start.content}"
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

  let tryMultiline = (not wrapBeg.preferMultiline) and (wrapEnd.preferMultiline) and
    (not input.anyOfIt(it.val.multiline()))

  if tryMultiline:
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
    if current.kind in {okComposed, okTable}:
      input.mapIt(it.name.len()).max() + conf.kvSeparator.len()
    else:
      0

  var subIdent = 0 # Required right margin for field values
  var maxWidth = 0 # Max allowed withd
  match (wrapBeg.preferMultiline, wrapEnd.preferMultiline):
    (true, true):
      # (prefix delimiter)
      # <field> [ block block
      #           block block ]
      # (suffix delimiter)
      subIdent = ident + fldsWidth
      maxWidth = conf.maxWidth
    (true, false):
      # (prefix delimiter)
      # <field> [ block block
      #           block block ] (suffix delimiter)
      subIdent = ident + fldsWidth
      maxWidth = conf.maxWidth - wrapEnd.content.len()
    (false, true):
      # (prefix delimiter) <field> [ block block
      #                              block block ]
      # (suffix delimiter)
      subIdent = ident + wrapBeg.content.len() + fldsWidth
      # IMPLEMENT account for sequence prefix, kvSeparator etc.
      maxWidth = conf.maxWidth
    (false, false):
      # (prefix delimiter) <field> [ block block
      #                              block block ] (suffix delimiter)
      subIdent = ident + wrapBeg.content.len() + fldsWidth
      maxWidth = conf.maxWidth - wrapEnd.content.len()

    # case current.kind:
    #   of okComposed:
    #     result = @[
    #       makeChunk(input.mapIt(
    #         arrangeKVPair(
    #           it.name, it.val, conf, maxFld + 2,
    #           header = current.name, kind = current.kind)
    #       ))
    #     ]

    #     if wrapBeg.content.len != 0:
    #       result = @[makeChunk(@[wrapBeg])] & result

    #     if wrapEnd.content.len != 0:
    #       result = result & @[makeChunk(@[wrapEnd])]

    #   else:
    #     result = input.mapIt(arrangeKVPair(
    #       it.name, it.val, conf, maxFld, kind = current.kind))

  # else:
  #   case current.kind:
  #     of okSequence:
  #       result = input.mapIt(arrangeKVPair(
  #         name = "",
  #         chunk = it.val,
  #         conf = conf,
  #         nameWidth = 0,
  #         kind = okSequence
  #       ))

  #     else:
  #       result = input.mapIt(arrangeKVPair(
  #         name = it.name,
  #         chunk = it.val,
  #         conf = conf,
  #         nameWidth = maxFld,
  #         kind = okSequence
  #       ))



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
      result = current.valItems.mapIt(
        ("", pstringRecursive(it, conf, ident + conf.seqPrefix.len()))
      ).arrangeKVPairs(conf, current, ident)

proc prettyString(tree: ObjTree, conf: PPrintConf, ident: int = 0): string =
  ## Convert object tree to pretty-printed string
  pstringRecursive(tree, conf).content.join("\n")

type
  Obj1 = object
    f1: int
    f2: seq[int]
    f3: Table[int, string]

import unittest

var conf = PPrintConf(
  maxWidth: 40,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("("), makeDelim(")")),
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
      @[1, 2],
      @[5, 6],
    ].pstr(), """
      - - 1
        - 2
      - - 5
        - 6""".dedent

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
  objWrapper: (makeDelim("{"), makeDelim("}")),
  fldNameWrapper: (makeDelim("\""), makeDelim("\"")),
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
          "name": "John"
           "age": 30
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

      echo jsonNode.pjson()
      # assertEq jsonNode, jsonNode.pjson().parseJson()
