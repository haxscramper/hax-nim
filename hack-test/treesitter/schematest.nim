import macros, options, sequtils, strutils
import compiler/ast
import hmisc/macros/matching
import hmisc/other/[hjson, hshell, oswrap, colorlogger]
import hnimast
import hmisc/[hexceptions, hdebug_misc]
import hmisc/algo/[clformat, halgorithm, hstring_algo]
import hpprint, strutils
import htreesitter

{.experimental: "caseStmtMacros".}

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


func newPTree(kind: NimNodeKind, val: string): PNode =
  result = PNode(kind: kind.toNk())
  result.strVal = val

func newPTree(kind: NimNodeKind, val: SomeInteger): PNode =
  result = PNode(kind: kind.toNK())
  result.intVal = BiggestInt(val)




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



func id(str: string): PNode = newPident(str)
proc lit(arg: string | int): PNode = newPLit(arg)

macro pquote*(mainBody: untyped): untyped =
  ## `quote` macro to generate `PNode` builder
  func aux(body: NimNode): NimNode =
    result = newCall("newPTree", ident $body.kind)
    case body.kind:
      of nnkAccQuoted:
        if body[0].kind == nnkIdent and
           not body[0].strVal().validIdentifier(): # Special case
           # operators `[]` - most of the time you would want to
           # declare `` proc `[]` `` rather than interpolate `[]`
           # (which is not valid variable name even)
          let bodyLit = newLit body[0].strVal()
          return quote do:
            newPTree(nnkAccQuoted, newPIdent(`bodyLit`))
        else:
          return body[0]
      of nnkStrKinds:
        result.add newLit body.strVal
      of nnkFloatKinds:
        result.add newLit body.floatVal
      of nnkIntKinds:
        result.add newLit body.intVal
      of nnkIdent, nnkSym:
        result = newCall("newPIdent", newLit(body.strVal))
      else:
        for subnode in body:
          result.add aux(subnode)

  result = aux(mainBody)


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


func makeGetKind(spec: NodeSpec, lang: string): ProcDecl[PNode] =
  result = newPProcDecl(
    "kind",
    {"node" : newPType(lang.makeNodeName())},
    some newPType(lang.makeNodeKindName())
  )

  let nameGet = makeTree[PNode](
    DotExpr[== id("node"), == id("tsNodeType")])
  var impl = makeTree[PNode](CaseStmt[== nameGet])

  for elem in spec:
    impl.add makeTree[PNode](OfBranch[
        == newPLit(elem.ttype),
        == newPIdent(elem.ntermName(lang))
    ])

  let assrt = pquote do:
    raiseAssert("Invalid element name '" & `nameGet` & "'")

  impl.add makeTree[PNode](Else[== assrt])

  result.impl = impl

func camelCase(str: varargs[string, `$`]): string =
  str.joinCamel()

func pascalCase(str: varargs[string, `$`]): string =
  result = str.joinCamel()
  result[0] = result[0].toUpperAscii()

func makeLangParserName(lang: string): string =
  pascalCase(lang, "parser")


func makeImplTsFor(lang: string): PNode =
  result = nnkStmtList.newPTree()
  let
    parser: PNode = lang.makeLangParserName().newPType().toNNode()
    nodeType: PNode = lang.makeNodeName().newPType().toNNode()
    langLen: PNode = newPLit(lang.len)
    langImpl: PNode = newPIdent("tree_sitter_" & lang)

  result.add pquote do:
    proc `langImpl`(): PtsLanguage {.importc, cdecl.}
    proc tsNodeType*(node: `nodeType`): string =
      $ts_node_type(TSNode(node))

    proc newTomlParser*(): `parser` =
      result = `parser`(ts_parser_new())
      discard ts_parser_set_language(PtsParser(result), `langImpl`())

    proc parseString*(parser: `parser`; str: string): `nodeType` =
      `nodeType`(
        ts_tree_root_node(
          ts_parser_parse_string(
            PtsParser(parser), nil, str.cstring, uint32(len(str)))))

  startHaxComp()
  result.add pquote do:
    func `[]`*(node: `nodeType`,
               idx: int, withUnnamed: bool = false): `nodeType` =
      if withUnnamed:
        `nodeType`(ts_node_child(TSNode(node), uint32(idx)))
      else:
        `nodeType`(ts_node_named_child(TSNode(node), uint32(idx)))

    func len*(node: `nodeType`, withUnnamed: bool = false): int =
      if withUnnamed:
        int(ts_node_child_count(TSNode(node)))
      else:
        int(ts_node_named_child_count(TSNode(node)))

    proc isNil*(node: `nodeType`): bool =
      ts_node_is_null(TsNode(node))

    iterator items*(node: `nodeType`,
                   withUnnamed: bool = false): `nodeType` =
      for i in 0 .. node.len(withUnnamed):
        yield node[i, withUnnamed]

    proc slice*(node: `nodeType`): Slice[int] =
      ts_node_start_byte(TsNode(node)).int ..<
      ts_node_end_byte(TsNode(node)).int

    proc treeRepr*(mainNode: `nodeType`,
                   instr: string,
                   withUnnamed: bool = false): string =
      proc aux(node: `nodeType`, level: int): seq[string] =
        if not(node.isNil()):
          result = @["  ".repeat(level) & ($node.kind())[`langLen`..^1]]
          if node.len(withUnnamed) == 0:
            result[0] &= " " & instr[node.slice()]

          for subn in items(node, withUnnamed):
            result.add subn.aux(level + 1)

      return aux(mainNode, 0).join("\n")


var spec = "src/node-types.json".parseFile().getElems().mapIt(it.toTree())
let grammar = "src/grammar.json".parseFile()

for extra in grammar["extras"]:
  if extra.matches({"name": @name}):
    spec.add Tree(ttype: name.asStr(), named: true)


const inputLang = "toml"

var buf: seq[string]

proc add(strBuf: var seq[string], node: PNode) =
  strBuf.add $node

buf.add pquote do:
  import htreesitter, sequtils, strutils

# buf.add $makeTree[PNode](ImportStmt[== newPIdent("htreesitter")])
buf.add $makeKindEnum(spec, inputLang).toNNode(standalone = true)


func makeDistinctType(baseType, aliasType: NType[PNode]): PNode =
  let
    aliasType = aliasType.toNNode()
    baseType = baseType.toNNode()

  pquote do:
    type `aliasType`* = distinct `baseType`


buf.add $makeDistinctType(
  newPType("TSNode"),
  newPType(makeNodeName(inputLang))
)


buf.add $makeDistinctType(
  newPType("PtsParser"),
  newPType(makeLangParserName(inputLang))
)

let langId = newPident makeNodeName(inputLang)

buf.add pquote do:
  proc tsNodeType*(node: `langId`): string

buf.add $makeGetKind(spec, inputLang).toNNode()
buf.add $makeImplTsFor(inputLang)


let file = inputLang & "_parser.nim"

file.writeFile(buf.join("\n\n\n"))

let srcFiles = ["parser.c", "scanner.c"]

for file in srcFiles:
  execShell makeGnuCmd("clang").withIt do:
    it.arg "src/" & file
    it - "c"
    it - ("o", "", file.dashedWords() & ".o")

rmDir "cache.d"

# template do

startColorLogger()

try:
  let (stdout, stderr, code) = runShell makeNimCmd("nim").withIt do:
    it.subCmd "c"
    it - "r"
    it - ("nimcache", "cache.d")
    it - ("forceBuild", "on")
    for file in srcFiles:
      # Link parser and external scanners
      it - ("passL", file.dashedWords() & ".o")

    # Link tree-sitter
    it - ("passL", "-ltree-sitter")

    it.arg "parser_user.nim"

  echo stdout
  echo stderr
except ShellError:
  let ex = getCEx(ShellError)
  echo ex.outstr
  for line in ex.errstr.split("\n"):
    if line.contains(["undefined reference"]):
      if line.contains("external"):
        once: err "Missing linking with external scanners"
        info line.split(" ")[^1][1..^2]
      elif line.contains("ts_"):
        once: err "Missing linking with tree-sitter library"
        info line
      else:
        once: err "Missing linking with other library"
        info line

    elif line.contains(["/bin/ld", "ld returned"]):
      discard
    else:
      echo line
