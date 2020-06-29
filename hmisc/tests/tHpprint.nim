import unittest, shell

import hmisc/[helpers, defensive]
import strutils

import hmisc/hpprint

suite "Block labeling":
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

suite "Compile-time object passthrough":
  test "Regular const value":
    discard

suite "Case object field iteration":
  discard

  test "No case fields":
    type
      U = object
        f1: int

    assert U.makeFieldsLiteral() == @[
      Field(name: "f1", fldType: "int", isKind: false)]

  test "Multiple fields on the same level":
    type
      U = object
        f1: int
        f2: float
        f3: char
        f4: string

    let generated = U.makeFieldsLiteral()
    let expected = @[
      Field(name: "f1", fldType: "int", isKind: false),
      Field(name: "f2", fldType: "float", isKind: false),
      Field(name: "f3", fldType: "char", isKind: false),
      Field(name: "f4", fldType: "string", isKind: false)
    ]

    if generated != expected:
      # "/tmp/generated.nim".writeFile(pstring generated)
      # "/tmp/expected.nim".writeFile(pstring expected)
      # shell:
      #   cwdiff /tmp/expected.nim /tmp/generated.nim

      quit 1

  test "Single case field":
    type
      U = object
        case kind: bool
          of true:
            f1: int
          of false:
            f2: float

    let lhs = U.makeFieldsLiteral()
    let rhs = @[
      Field(fldType: "bool", name: "kind", isKind: true, branches: @[
        FieldBranch(
          value: ObjTree(kind: okConstant, constType: "bool", strLit: "true"),
          flds: @[ Field(fldType: "int", isKind: false, name: "f1") ],
          isElse: false
        ),
        FieldBranch(
          value: ObjTree(kind: okConstant, constType: "bool", strLit: "false"),
          flds: @[ Field(fldType: "float", isKind: false, name: "f2") ],
          isElse: false
        ),
      ]
    )]

    if lhs != rhs:
      raiseAssert "Fail"

  test "Multiple case fields":
    type
      U = object
        case kind1: bool
          of true: f11: int
          of false: f21: float

        case kind2: char
          of 'a':
            f12: int
          else:
            f22: float

    let generated = U.makeFieldsLiteral()
    let expected  = @[
      Field(fldType: "bool", name: "kind1", isKind: true, branches: @[
        FieldBranch(
          value: ObjTree(kind: okConstant, constType: "bool", strLit: "true"),
          flds: @[ Field(fldType: "int", isKind: false, name: "f11") ],
          isElse: false
         ),
        FieldBranch(
          value: ObjTree(kind: okConstant, constType: "bool", strLit: "false"),
          flds: @[ Field(fldType: "float", isKind: false, name: "f21") ],
          isElse: false
         ),
      ]),
      Field(fldType: "char", name: "kind2", isKind: true, branches: @[
        FieldBranch(
          value: ObjTree(kind: okConstant, constType: "char", strLit: "'a'"),
          flds: @[ Field(fldType: "int", isKind: false, name: "f12") ],
          isElse: false
        ),
        FieldBranch(
          value: ObjTree(),
          flds: @[ Field(fldType: "float", isKind: false, name: "f22") ],
          isElse: true
        ),
      ])
    ]


    if generated != expected:
      raiseAssert "Fail"

  test "Nested case fields":
    type
      U = object
        case kind1: bool
          of true: f11: int
          of false:
            case kind2: char
              of 'a':
                f12: int
              else:
                f22: float

    let generated = U.makeFieldsLiteral()
    let expected = @[
      Field(fldType: "bool", name: "kind1", isKind: true, branches: @[
        FieldBranch(
          value: ObjTree(kind: okConstant, constType: "bool", strLit: "true"),
          flds: @[ Field(fldType: "int", isKind: false, name: "f11") ],
          isElse: false
         ),
        FieldBranch(
          value: ObjTree(kind: okConstant, constType: "bool", strLit: "false"),
          flds: @[
            Field(fldType: "char", name: "kind2", isKind: true, branches: @[
              FieldBranch(
                value: ObjTree(
                  kind: okConstant, constType: "char", strLit: "'a'"),
                flds: @[ Field(fldType: "int", isKind: false, name: "f12") ],
                isElse: false
              ),
              FieldBranch(
                value: ObjTree(),
                flds: @[ Field(fldType: "float", isKind: false, name: "f22") ],
                isElse: true
              ),
            ])
          ],
         isElse: false
         ),
      ]),
    ]

    if generated != expected:
      raiseAssert "Fail"

  test "Get fields inside of generic proc":
    proc generic[T](a: T): void =
      let generated = T.makeFieldsLiteral()
      let expected = @[
        Field(name: "f1", fldType: "int", isKind: false),
        Field(name: "f2", fldType: "char", isKind: false)
      ]

      if generated != expected:
        # "/tmp/generated.nim".writeFile(pstring generated)
        # "/tmp/expected.nim".writeFile(pstring expected)
        raiseAssert "Fail"


    type
      U = object
        f1: int
        f2: char

    generic(U())


  test "Get all kind fields":
    type
      U = object
        case kind1: bool
          of true: f11: int
          of false:
            case kind2: char
              of 'a':
                f12: int
              else:
                f22: float

        case kind3: bool
          of true:
            f31: float
          of false:
            f32: seq[seq[seq[seq[seq[set[char]]]]]]


    let generated = makeFieldsLiteral(U).getKindFields()
    let expected = @[
      Field(
        name: "kind1", fldType: "bool", isKind: true, branches: @[
          FieldBranch(
            value: ObjTree(
              kind: okConstant, constType: "bool", strLit: "false"),
            flds: @[
              Field(name: "kind2", fldType: "char", isKind: true)
            ]
          )
        ]
      ),
      Field(name: "kind3", fldType: "bool", isKind: true)
    ]

    if generated != expected:
      "/tmp/generated.nim".writeFile(pstring generated)
      "/tmp/expected.nim".writeFile(pstring expected)
      shell:
        cwdiff /tmp/expected.nim /tmp/generated.nim

      quit 1


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

template pstr(arg: untyped, ident: int = 0): untyped =
  toSimpleTree(arg).prettyString(conf, ident)

suite "Simple configuration":
  test "integer":
    assertEq pstr(12), "12"

  test "indentation":
    assertEq pstr(12, 3), "   12"

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
