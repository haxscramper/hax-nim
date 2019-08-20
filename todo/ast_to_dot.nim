import macros, tables, strformat, strutils, sequtils

import strutils

proc truncate(value: string, length = 9, ellipsis = "..."): string =
  if value.len > length:
    value[0..length - 1] & ellipsis
  else:
    value

proc surround(input: string, surrWith: (string, string)): string =
  surrWith[0] & input & surrWith[1]

proc surround(input: string, surrWith: string): string =
  surrWith & input & surrWith

proc surround1(input: string, surrWith: (string, string)): string =
  if input.len == 0: ""
  else: input.surround(surrWith)

proc escapeHTML(input: string): string =
  input.multiReplace([
    (">", "&gt;"),
    ("<", "&lt;"),
    ("&", "&amp;"),
    ("\"", "&quot;")
  ])



macro dumpDotAst(head: string, body: untyped) =
  var idx = 0
  var descrTable: Table[int, NimNode]
  var graph: seq[string]

  proc toDot(node: NimNode): (int, string) =
    inc idx
    result[0] = idx
    result[1] = "n$# -> " % $idx

    var subnodes: seq[int]
    descrTable[idx] = node

    for sub in node:
      let res = sub.toDot()
      subnodes.add(res[0])

    result[1] &= "{ $# }" % subnodes.mapIt("n" & $it).join(",")
    graph.add(result[1])

  for node in body:
    discard node.toDot()


  var resTotal: seq[string]

  for id, node in descrTable:
    var label: string = $node.kind
    var text: string =
      case node.kind:
        of nnkIdent: node.strVal.escape()
        of nnkStrLit..nnkTripleStrLit:
          $node.strVal.truncate().surround("\"")
        of nnkCharLit .. nnkUInt64Lit: $node.intVal
        else: ""

    var color: string =
      case node.kind:
        of nnkStmtList: "azure2"
        of nnkIdent: "brown2"
        of nnkStrLit: "green"
        of nnkVarSection, nnkLetSection, nnkIdentDefs: "magenta2"
        else: "cyan2"

    var res = "n$# [label = <$#<br/>$#>, fillcolor = $#, style = filled];" % [
      $id,
      label.surround1(("<i>", "</i>")),
      text
        .escapeHTML()
        .surround1(("<b>", "</b>"))
        .surround1(("<font face='courier'>", "</font>")),
      color
    ]

    res = res.replace("\n","")
    resTotal.add(res)


  let filename = head.strVal
  let resultString = """
digraph G {
node[shape=box];
$#
$#
}
""" %
    [
      resTotal.join("\n"),
      graph.join("\n")]
  if filename.len == 0:
    let resStrNode = newStrLitNode(resultString)
    result = quote do:
      echo `resStrNode`
  else:
    head.strVal.writeFile(resultString)







# Put any valid code under macro and add name of the file in the macro
# argument. If string is empty result will be printed to stdout. Empty
# does not mean the sting is optional!
dumpDotAst "test.dot":
  loop -> seq[(int, float)]:
    lfor x in @[1,2,2]
    lfor e in @[0.01]
    linitially:
      echo "Start-test"
    ldo:
      lcollect (x * e) into mults
      lcollect x into nums
    lfinally:
      lreturn zip(nums, mults)
