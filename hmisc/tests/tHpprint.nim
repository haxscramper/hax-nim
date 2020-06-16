import unittest

import hmisc/[helpers, defensive]
import strutils

import hmisc/hpprint

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
      @{ rpTopLeftAbove : (text: "<-->", offset: 2) })), "<-->\n  [|||]"

  test "Chunk label on bottom right":
    assertEq $(test(
      @{ rpBottomRight : (text: "<>", offset: 2) })), "[|||]<>"

  test "Chunk label on bottom left":
    assertEq $(test(
      @{ rpBottomLeft : (text: "<>", offset: 2) })), "  [|||]\n<>"

  test "Top-above & bottom left":
    assertEq $(test(
      @{ rpTopLeftAbove : (text: "{{{", offset: 2),
         rpBottomLeft : (text: "}}}", offset: 2)})),
         """
         {{{
           [|||]
         }}}""".dedent


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

type
  Obj1 = object
    f1: int
    f2: seq[int]
    f3: Table[int, string]

var conf = PPrintConf(
  maxWidth: 40,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("(", multiline = true),
               makeDelim(")", multiline = false)),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": "
  # wrapLargerThan: 10
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
  objWrapper: (makeDelim("{"), makeDelim("}")),
  fldNameWrapper: (makeDelim("\""), makeDelim("\"")),
  fldSeparator: ",",
  kvSeparator: ": ",
  # wrapLargerThan: 10,
  alignFieldsRight: true
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
  # wrapLargerThan: 10,
  nowrapMultiline: true
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

    assertEq jsonNode.treeStr(), """
      widget:
        debug:  "on"
        window:
          title:  "Sample Konfabulator Widget"
          name:   "main_window"
          width:  500
          height: 500
        image:
          src:       "Images/Sun.png"
          name:      "sun1"
          hOffset:   250
          vOffset:   250
          alignment: "center"
        text:
          data:      "Click Here"
          size:      36
          style:     "bold"
          name:      "text1"
          hOffset:   250
          vOffset:   100
          alignment: "center"
          onMouseUp: "sun1.opacity = (sun1.opacity / 100) * 90;"""".dedent()
