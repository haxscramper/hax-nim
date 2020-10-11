import macros, options, sequtils
import hmisc/macros/matching
import hmisc/other/hjson
import hpprint
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



for elem in data.getElems():
  let tree = elem.toTree()
  pprint tree
