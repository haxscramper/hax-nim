# TODO account for control codes in stings

import hmisc/helpers
import tables, sequtils, math, strutils, strformat
import typetraits

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
  PPrintConf = object
    maxWidth: int
    identStr: string
    wrapLargerThan: int

    kvSeparator: string
    objWrapper: (string, string)

    seqSeparator: string
    seqPrefix: string
    seqWrapper: (string, string)

  Chunk = object
    content: seq[string]
    maxWidth: int

proc multiline(chunk: Chunk): bool = chunk.content.len > 1

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
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): seq[Chunk]

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
    assert false
  else:
    # Put chunk on the same line as name
    return makeChunk(@[
      (" ".repeat(prefWidth - pref.len())) & pref & chunk.content[0]
    ] &
      chunk.content[1..^1].mapIt(" ".repeat(prefWidth) & it)
    )


proc arrangeKVPairs(
  input: seq[tuple[name: string, val: Chunk]],
  conf: PPrintConf, current: ObjTree, ident: int): seq[Chunk] =
  let maxFld = input.mapIt(it.name.len()).max()

  if not input.anyOfIt(it.val.multiline()):
    # No multiline chunks present, can theoretically fit on a single line
    let (wrapBeg, wrapEnd) =
      case current.kind:
        of okComposed:
          if current.namedObject:
            (&"{current.name}{conf.objWrapper[0]}", &"{conf.objWrapper[1]}")
          else:
            (&"{conf.objWrapper[0]}", &"{conf.objWrapper[1]}")
        of okSequence:
          (conf.seqWrapper[0], conf.seqWrapper[1])
        of okTable:
          # TODO use configurable wrapper begin/end
          ("{", "}")
        else:
          assert false, "Cannot arrange kv pair in constant"
          (".", ".")


    var singleLine =
      if current.kind == okSequence or (
        current.kind == okComposed and (not current.namedFields)):
        input.mapIt(&"{it.val.content[0]}").join(conf.seqSeparator)
      else:
        input.mapIt(&"{it.name}{conf.kvSeparator}{it.val.content[0]}").join(
          conf.seqSeparator)

    singleLine = wrapBeg & singleLine & wrapEnd


    if singleLine.len < (conf.maxWidth - ident):
      return @[makeChunk(content = @[singleLine])]

    case current.kind:
      of okComposed:
        return
          makeChunk(@[current.name]) &
          input.mapIt(
            arrangeKVPair(
              it.name, it.val, conf, maxFld + 2,
              header = current.name, kind = current.kind)
          )
      else:
        result = input.mapIt(arrangeKVPair(
          it.name, it.val, conf, maxFld, kind = current.kind))

  else:
    case current.kind:
      of okSequence:
        result = input.mapIt(arrangeKVPair(
          name = "",
          chunk = it.val,
          conf = conf,
          nameWidth = 0,
          kind = okSequence
        ))

      else:
        discard


proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): seq[Chunk] =
  case current.kind:
    of okConstant:
      return @[makeChunk(content = @[ current.strLit ])]
    of okComposed:
      if not current.sectioned:
        let maxFld = current.fldPairs.mapIt(it.name.len()).max()
        result = current.fldPairs.mapIt(
          (it.name, pstringRecursive(
            it.value, conf, maxFld + ident).makeChunk())
        ).arrangeKVPairs(conf, current, ident + maxFld)
    of okTable:
      let maxFld = current.valPairs.mapIt(it.key.len()).max()
      return current.valPairs.mapIt(
        (it.key, pstringRecursive(it.val, conf, maxFld + ident).makeChunk())
      ).arrangeKVPairs(conf, current, ident + maxFld)
    of okSequence:
      result = current.valItems.mapIt(
        ("", pstringRecursive(it, conf, ident + conf.seqPrefix.len()).makeChunk())
      ).arrangeKVPairs(conf, current, ident)

proc prettyString(tree: ObjTree, conf: PPrintConf, ident: int = 0): string =
  ## Convert object tree to pretty-printed string
  pstringRecursive(tree, conf).mapIt(it.content.join("\n")).join("\n")

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
  seqWrapper: ("[", "]"),
  objWrapper: ("(", ")"),
  kvSeparator: ": ",
  wrapLargerThan: 10
)

template pstr(arg: untyped): untyped =
  toSimpleTree(arg).prettyString(conf)


suite "Simple configuration":
  test "integer":
    assert pstr(12) == "12"

  test "string":
    assert pstr("112") == "\"112\""

  test "Anonymous tuple":
    assert pstr((12, "sdf")) == "(12, \"sdf\")"

  test "Named tuple":
    assert pstr((a: "12", b: "222")) == "(a: \"12\", b: \"222\")"

  test "Narrow sequence":
    conf.maxWidth = 6
    assert @[1,2,3,4].pstr() == "- 1\n- 2\n- 3\n- 4"

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
