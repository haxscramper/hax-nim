import macros, options, sequtils
import compiler/ast
import hmisc/macros/matching
import hmisc/other/[hjson, hshell]
import hnimast
import hmisc/algo/[clformat, halgorithm]
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

dumpTree:
  nodeType(TSTree(node))

func makeImplTsFor(lang: string): PNode =
  result = nnkStmtList.newPTree()

  result.add newPProcDecl(
    "tsNodeType",
    {"node" : newPType(lang.makeNodeName())},
    some newPType("string"),
    makeTree[PNode](Call[
      == newPIdent("nodeType"),
      Call[
        == newPident("TSNode"),
        == newPident("node")]])
  ).toNNode()

  result.add newPProcDecl(
    "tree_sitter_toml", @[],
    some newPType("PtsLanguage"),
    exported = false,
    pragma = newPPRagma(@["importc", "cdecl"])
  ).toNNode()


let spec = data.getElems().mapIt(it.toTree())

var buf: seq[string]

buf.add $makeTree[PNode](ImportStmt[== newPIdent("htreesitter")])
buf.add $makeKindEnum(spec, "toml").toNNode(standalone = true)

let tree = makeTree[PNode]:
  TypeSection[
    TypeDef[
      Postfix[
        == newPident("*"),
        == makeNodeName("toml").newPident(),
      ],
      Empty(),
      DistinctTy[== newPIdent("TSNode")]]]

buf.add $tree


buf.add $makeImplTsFor("toml")
buf.add $makeGetKind(spec, "toml").toNNode()


let file = "toml_parser.nim"

file.writeFile(buf.join("\n\n\n"))

execShell makeNimCmd("nim").withIt do:
  it.subCmd "c"
  it.arg file
