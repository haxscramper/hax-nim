import macros, options, sequtils, strutils
import compiler/ast
import hmisc/macros/matching
import hmisc/other/[hjson, hshell, oswrap, colorlogger]
import hnimast
import hmisc/hexceptions
import hmisc/algo/[clformat, halgorithm, hstring_algo]
import hpprint, strutils
import htreesitter

let data = "src/node-types.json".parseFile

type
  TreeChildren = object
    multiple: bool
    required: bool
    types: seq[tuple[ttype: string, named: bool]]

  Tree = object
    ttype: string
    named: bool
    children: Option[TreeChildren]

  NodeSpec = seq[Tree]

func toTree(js: JsonNode): Tree =
  result = Tree(
    ttype: js["type"].asStr(),
    named: js["named"].asBool()
  )

  if js.matches({"children": @ch}):
    result.children = some TreeChildren(
      multiple: ch["multiple"].asBool(),
      required: ch["required"].asBool(),
      types: ch["types"].mapIt((
        ttype: it["type"].asStr(),
        named: it["named"].asBool())))


func toNtermName(str: string): string =
  if str.validIdentifier():
    str.splitCamel().joinCamel()
  else:
    str.toNamedMulticharJoin()

func ntermName(elem: Tree, lang: string): string =
  result = lang & elem.ttype.toNtermName().capitalizeAscii()
  if not elem.named:
    result &= "Tok"

func makeNodeName(lang: string): string = lang.capitalizeAscii() & "Node"
func makeNodeKindName(lang: string): string = lang.makeNodeName() & "Kind"



func makeKindEnum(spec: NodeSpec, lang: string): PEnum =
  result = PEnum(name: lang.makeNodeKindName(), exported: true)
  for elem in spec:
    result.values.add makeEnumField[PNode](
      elem.ntermName(lang), comment = elem.ttype)

func newPProcDecl(
  name: string,
  args: openarray[(string, NType[PNode])] = @[],
  rtyp: Option[NType[PNode]] = none(NType[PNode]),
  impl: PNode = nil,
  exported: bool = true,
  pragma: PPragma = PPRagma()
     ): ProcDecl[PNode] =
  result.name = name
  result.exported = exported
  result.signature = NType[PNode](
    kind: ntkProc,
    arguments: toNIdentDefs(args)
  )

  result.signature.pragma = pragma
  if rtyp.isSome():
    result.signature.setRtype rtyp.get()

  result.impl = impl


func makeGetKind(spec: NodeSpec, lang: string): ProcDecl[PNode] =
  result = newPProcDecl(
    "kind",
    {"node" : newPType(lang.makeNodeName())},
    some newPType(lang.makeNodeKindName())
  )

  var impl = makeTree[PNode]:
    CaseStmt[
      DotExpr[
        == newPIdent("node"),
        == newPIdent("tsNodeType")
      ]]

  for elem in spec:
    impl.add makeTree[PNode](OfBranch[
        == newPLit(elem.ttype),
        == newPIdent(elem.ntermName(lang))
    ])

  impl.add makeTree[PNode](Else[
    Call[
      == newPIdent("raiseAssert"),
      == newPLit("Invalid element name")
    ]
  ])

  result.impl = impl

func camelCase(str: varargs[string, `$`]): string =
  str.joinCamel()

func pascalCase(str: varargs[string, `$`]): string =
  result = str.joinCamel()
  result[0] = result[0].toUpperAscii()

func makeLangParserName(lang: string): string =
  pascalCase(lang, "parser")


dumpTree:
  result = TomlParser(ts_parser_new())
  ts_parser_set_language(result, tree_sitter_toml())

func id(str: string): PNode = newPident(str)
proc lit(arg: string | int): PNode = newPLit(arg)

func makeImplTsFor(lang: string): PNode =
  result = nnkStmtList.newPTree()
  let
    parser = lang.makeLangParserName()
    nodeType = lang.makeNodeName()


  result.add newPProcDecl(
    "tsNodeType",
    {"node" : newPtype(nodeType)},
    some newPType("string"),
    makeTree[PNode](Call[
      == newPIdent("nodeType"),
      Call[
        == id("TSNode"),
        == id("node")]])
  ).toNNode()

  result.add newPProcDecl(
    "tree_sitter_" & lang, @[],
    some newPType("PtsLanguage"),
    exported = false,
    pragma = newPPRagma(@["importc", "cdecl"])
  ).toNNode()


  result.add newPProcDecl(
    camelCase("new", lang, "parser"), @[],
    some newPType(parser),
    makeTree[PNode](
      StmtList[
        Asgn[== id("result"),
             Call[== id(parser),
                  Call[== id("ts_parser_new")]]],
        Call[== newPIdent("ts_parser_set_language"),
             Call[== id("PtsParser"), == newPIdent("result")],
             Call[== newPIdent("tree_sitter_toml")]]])
  ).toNNode()

  result.add newPProcDecl(
    "parseString", {
      "parser": newPType(parser),
      "str": newPType("string")
    },
    some newPType(nodeType),
    makeTree[PNode](
      StmtList[
        Call[== id("echo"), == lit("22")],
        Call[
          == id(nodeType),
          Call[
            == id("tsParseString"),
            Call[== id("PtsParser"), == id("parser")],
            == id("str")
          ]]])
  ).toNNode()


let spec = data.getElems().mapIt(it.toTree())

const inputLang = "toml"

var buf: seq[string]

buf.add $makeTree[PNode](ImportStmt[== newPIdent("htreesitter")])
buf.add $makeKindEnum(spec, inputLang).toNNode(standalone = true)

func makeDistinctType(baseType, aliasType: NType[PNode]): PNode =
  makeTree[PNode]:
    TypeSection[
      TypeDef[
        Postfix[
          == id("*"),
          == aliasType.toNNode(),
        ],
        Empty(),
        DistinctTy[== baseType.toNNode()]]]


buf.add $makeDistinctType(
  newPType("TSNode"),
  newPType(makeNodeName(inputLang))
)


buf.add $makeDistinctType(
  newPType("PtsParser"),
  newPType(makeLangParserName(inputLang))
)


buf.add $makeImplTsFor(inputLang)
buf.add $makeGetKind(spec, inputLang).toNNode()


let file = inputLang & "_parser.nim"

file.writeFile(buf.join("\n\n\n"))

execShell makeGnuCmd("clang").withIt do:
  it.arg "src/parser.c"
  it - "c"
  it - ("o", "", inputLang & "-parser-lib.o")

rmDir "cache.d"

# template do

startColorLogger()

try:
  let (stdout, stderr, code) = runShell makeNimCmd("nim").withIt do:
    it.subCmd "c"
    it - ("nimcache", "cache.d")
    it - ("forceBuild", "on")
    it - ("passL", "toml-parser-lib.o")
    it.arg "parser_user.nim"
except ShellError:
  for line in getCEx(ShellError).errstr.split("\n"):
    if line.contains(["undefined reference", "external"]):
      once:
        err "Missing linking with external scanners"
      info line.split(" ")[^1][1..^2]
    elif line.contains(["/bin/ld", "ld returned"]):
      discard
    else:
      echo line
  # printShellError()
