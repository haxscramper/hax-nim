import pegs
import options
import ../../lib/termformat, ../../lib/argparse, ../../lib/helpers
import os, strutils, sequtils, macros
import ../../lib/colecho_lib
import ../../lib/colecho_types
import strformat
import tables
import deques

# XXX remove this
var level = 0

const pegGrammar = readFile("grammar.pegs").string
let pegAst = parsePeg(pegGrammar)



var lvl = 0

let parse = pegAst.eventParser:
  pkNonTerminal:
    enter:
      let ident = "  ".repeat(lvl)
      inc lvl
    leave:
      dec lvl

      let ident = "  ".repeat(lvl)
      if length > 0:
        case p.nt.name:
          of "for_loop", "while_loop", "cond_stmt", "if_stmt":
            echo ident, p.nt.name, " end"

var dbgLvl = 0
var dbgStr = ""
let ignored = @["ws"] # XXX move closer to first use

# TODO FIXME dump code generated by template/macro inocation
# macro debugInvocation(head, body: typed): typed =
#   let ast = quote do:
#     `body`

#   result = body

# IDEA write parsing stages to file with moving arrow
# IDEA generate terminal animation from file parsing
# XXX add s2cond version thst could also show matched range
# XXX add colors
proc showArrow(indent: string, topString: string, pos: int, annot: string = "") =
  echo indent, topString
  echo indent, " ".repeat(pos - 1), "^ ", annot



let dumpAll = false
# XXX wrap an proc
#~#== Debug parser
let debugParse =
  eventParser(pegAst):
    pkNonTerminal:
      enter:
        if (p.nt.name notin ignored or dumpAll):
          let ident = "|   ".repeat(dbgLvl)
          echo ident, &"{p.nt.name}"
          inc dbgLvl
      leave:
        if (p.nt.name notin ignored or dumpAll):
          dec dbgLvl
          let ident = "|   ".repeat(dbgLvl)
          let matchStr = dbgStr.substr(start, start + length - 1)
          if length > 0:
            showArrow(ident, s, start + length, annot =
                      &"[ {p.nt.name} ] " & $toGreen("ok"))
          else:
            echo ident, &"[ {p.nt.name} ] " & $toRed("fail")
    pkChar:
      leave:
        let ident = "    ".repeat(dbgLvl)
        if length > 0:
          echo ident, "[ '", p.ch, "' ]", $toGreen(" ok")
        else:
          echo ident, "[ '", p.ch, "' ]", $toRed(" fail")
    pkTerminal:
      leave:
        let ident = "    ".repeat(dbgLvl)
        if length > 0:
          echo ident, " \"", p.term, "\" ", $toGreen("ok")
        else:
          echo ident, " \"", p.term, "\" ", $toRed("fail")



proc test(str: string, andParse: bool = false, dbg = false): void =
  let res = $(if str.match(pegAst): "true".toGreen else: "false".toRed)
  echo &"{res:<15}: {str}"
  if dbg:
    dbgStr = str
    discard debugParse(str)
  elif andParse:
    discard parse(str)


type
  NodeKind = enum
    cnkIf # NOTE Single-text
    cnkElif
    cnkWhile
    cnkElse
    cnkFor # NOTE three-text
    cnkCond
    cnkExpr # NOTE Placed as-is
    cnkAssgn
    cnkUndef
    cnkTerminal



  Node = ref object
    children: seq[Node] ## Child nodes. I case of for/while... some of
                        ## them might be used for node description
    kind: NodeKind



proc toNodeKind(str: string): NodeKind =
  case str:
    of "if_stmt": cnkIf
    of "elif_stmt": cnkElif
    of "else_stmt": cnkElse
    of "for_loop": cnkFor
    of "while_loop": cnkWhile
    of "cond_stmt": cnkCond
    of "assgn": cnkAssgn
    of "expr": cnkExpr
    else: cnkUndef

proc toNode(str: string): Node = Node(kind: str.toNodeKind)

proc `$`(node: Node): string =
  result = node.mapItBFStoSeq(
    children,
    " ".repeat(lv) & $(it.kind)
  ).join("\n")



type
  Scope = ref object
    kind: NodeKind
    name: string
    text: string
    children: seq[Scope] ## Scopes enclosed by this one. In case of
                         ## if/for... some of them might be used as
                         ## condition/init for drawing nodes.



proc placeChildNode(stack: var seq[Scope], child: Scope) =
  # NOTE we do not differentiate between expressions etc. when placing
  # child nodes. The difference is only used when drawing final graph
  # --- then we put some of the child scopes as description for nodes.
  stack[^1].children.add(child)


func getName(st: seq[Scope]): string =
  tern(st.len == 0, "start_", st[^1].name & $st[^1].children.len)

proc chartBuilder(str: string): Option[Scope] =
  var done: bool = false
  var stack: seq[Scope]
  let allowed = @["if_stmt", "while_loop", "expr", "assgn",
                  "for_loop", "elif_stmt", "else_stmt", "cond_stmt"]
  #~# Flowchart parser
  let chartBuilderImpl = pegAst.eventParser:
    pkNonTerminal:
      enter:
        if not done and p.nt.name in allowed:
          # echoi stack.len, &"add {p.nt.name} to stack"
          stack.add(Scope(
            name: getName(stack) & p.nt.name & "_",
            kind: p.nt.name.toNodeKind
          ))
        # elif p.nt.name notin allowed:
        #   echoi 1, "Skipping", p.nt.name
      leave:
        if not done and p.nt.name in allowed:
          if length > 0:
            var child = stack.pop()
            if stack.len > 0:
              child.text = s.substr(start, start + length - 1)
              stack.placeChildNode(child)
            else:
              done = true
              stack.add(child)
          else:
            discard stack.pop()

  let res = chartBuilderImpl(str)
  if res > 0:
    return stack[^1]

type
  # IDEA treat this as DAG
  # XXX target nodes are statemrnts that are located
  # inside the scope of top node. If top node is cond
  # then tsrgets are if/else/elif statements under it.
  # there should be no other targets for cond other than
  # that.
  GVizNode = ref object
    name: string
    targets: seq[GVizNode] ## inner nodes for scope
    style: string ## Node style modifier
    text: seq[string] ## predicate for conditions/loops etc.
    kind: NodeKind


proc compressActions(inseq: seq[GVizNode]): seq[GVizNode] =
  let compressable = cnkExpr .. cnkAssgn
  if inseq.len > 1:
    result.add(inseq[0])
    var added = 0
    for i, item in inseq[1..^1]:
      if inseq[i].kind in compressable and
         item.kind in compressable:
        if added < 5:
          result[^1].text &= item.text[0..^2] & @[item.text[^1] & ";"]
          inc added
        else:
          result.add(item)
          added = 0
      else:
        result.add(item)
  else:
    result = inseq


func enumerate(scopes: seq[Scope]): seq[Scope] =
  toSeq(pairs(scopes)).mapIt(
    block:
      var res = it.val
      res.name = res.name & "_" & $it.key
      res
  )



proc `$`(node: GVizNode): string =
  let targets = node.targets.mapIt(&"\t\t- {it.name}").join("\n")
  &"""
name: {node.name}
text: {node.text}
kind: {node.kind}
trgs: ({targets.len})
{targets}
  """

proc joinScope(str, startWith, endWith: string): string =
  result &= startWith & "_body" & "[label=\"" & str & "\"" & ",shape=box];\n"

var tmp = 0
proc scopeToGviz(inScope: Scope): GVizNode =
  inc tmp
  defer: dec tmp

  let kind = inScope.kind
  GVizNode(
    kind: kind,
    name: inScope.name,
    text: case kind:
      # TEMP assuming that for loop is correctly filled
      of cnkFor: inScope.children[0..2].mapIt(it.text)
      of cnkIf .. cnkWhile: @[inScope.children[0].text]
      of cnkExpr, cnkAssgn: @[inScope.text]
      else: @[],
    # XXX ignore children if thisnis expression or
    # assignment
    targets:
      block:
        let children =
          case kind:
            of cnkFor: inScope.children[2..^1]
            of cnkIf .. cnkWhile: inScope.children[1..^1]
            else: inScope.children

        # echoi tmp, &"Under {inScope.name.togreen} ({inScope.kind})"
        # echo $children.enumerate.mapIt(
        #   "  ".repeat(tmp) &
        #     it.name & ": " &
        #     it.text.replace('\n', ' ')
        # ).joinl
        children.enumerate.mapIt(it.scopeToGviz).compressActions
  )

# TODO
# template forEachLeaf(topNode: typed, subNode: untyped, op: untyped): untyped =
#   ## Call operator on each leaft node (i.e. subnode which has empty
#   ## subNode) does not guarantee to visit each leaf once

proc getStyle(gv: GVizNode, start: bool = true): string =
  @[
    case gv.kind:
      of cnkIf, cnkElif: "shape=diamond"
      of cnkExpr, cnkAssgn: "shape=box"
      of cnkFor: tern(start, "shape=trapezium", "shape=invtrapezium")
      else: ""
  ].filterIt(it.len > 0).join(",")

# TODO
# macro if1(head, body: untyped): untyped =
#   ## Block-style ternary expression. Evaluate `t:` part if head is
#   ## true otherwise evaluate `f:`.
#   quote do:
#     if `head`:

proc label(gv: GVizNode, start: bool = true): string =
  case gv.kind:
    of cnkIf .. cnkElif, cnkExpr .. cnkAssgn: gv.text.joinl()
    of cnkFor: tern(start, gv.text[0..^2], gv.text[2..2]).joinl()
    else: @[gv.name, $gv.kind].joinl()


proc getTargetConnections(gv, gvTop: GVizNode, start: string): string =
  var prev = start
  for targ in gv.targets[1..^1]:
    let nextName =
      case targ.kind:
        of cnkElse, cnkCond: gv.targets[^1].targets[0].name
        else: targ.name

    let labelStr =
      case targ.kind:
        of cnkElif, cnkElse: "[xlabel=no]"
        else: ""


    result &= &"{prev} -> {nextName}{labelStr};\n"
    # XXX Draw edges to the target items.
    prev = targ.name

proc getConnectionBlock(gv, gvTop: GVizNode, connectList, start: string): string =
  case gv.kind:
    of cnkCond:
        &"""
/* start */
{{
/* edge connection {gv.name} */
rank=same;
{connectList}
}}
/* end */
"""
    of cnkElif:
      &"{gv.name} -> {gv.targets[0].name}[xlabel=yes];"
    of cnkElse:
      ""
    else:
        &"""
/* start */
/* True branch for {gv.name} */
{gv.name} -> {start}[xlabel=yes];
{connectList}
/* end */
"""



proc getEdgeConnections(gv, gvTop: GVizNode): string =
  defer:
    result &= "\n\n\n"
  case gv.kind:
    of cnkExpr .. cnkAssgn: ""
    of cnkCond, cnkIf .. cnkFor:
      let start = gv.targets[0].name
      var connectList = getTargetConnections(gv, gvTop, start)
      getConnectionBlock(gv, gvTop, connectList, start)

    else:
      # if this is for|while loop add two edges: first for
      # input to toplevel node, second for output from it.
      # This weay if 'it' is for loop it will have edge
      # coming in its head, and edge comint from its tail.
      let res = gv.targets.mapIt(&"{gv.name} -> {it.name};" ).join("\n")
      &"""
/* start */
{res}
/* end */
"""




proc getNodeDescription(gv, gvTop: GVizNode): string =
  case gv.kind:
    of cnkElse: ""
    of cnkExpr .. cnkAssgn, cnkIf .. cnkElif:
       &"{gv.name}[{gv.getStyle},label=\"{gv.label}\"];\n"
    of cnkFor: &"{gv.name}[{gv.getStyle},label=\"{gv.label}\"];\n"
    else:
      gv.targets.filterIt(it.kind notin @[cnkElse]).mapIt(
          &"{it.name}[{it.getStyle},label=\"{it.label}\"];"
      ).join("\n")

proc gvizToDot(gv, gvTop: GVizNode): tuple[dotBody: string, outNodes: seq[string]]

proc finalGVizToDot(gv, gvTop: GVizNode): (string, seq[string]) =
  (&"{gv.name}[{gv.getStyle},label=\"{gv.label}\"];\n",
    @[gv.name])


proc nestedGVizToDot(gv, gvTop: GVizNode): tuple[
  dotBody: string, outNodes: seq[string]] =
  #% Generate edge connection between child nodes
  let edges = getEdgeConnections(gv, gvTop)
  #% Generte description (label, style) for current node
  let node = getNodeDescription(gv, gvTop)
  #% Recurse into target nodes, generate descriptions and get lists of
  #% final nodes in sequence.
  let underTargets: seq[(string, seq[string])] =
    gv.targets.mapIt(it.gvizToDot(gv))


  var outNodes: seq[string] = #% Generate output node names
    case gv.kind:
      of cnkExpr .. cnkAssgn: @[gv.name]
      of cnkFor: @[gv.name & "_end"]
      of cnkIf: underTargets[^1][1]
      else:
        case gvTop.kind:
          of cnkCond, cnkIf: underTargets.mapIt(it[1]).concat()
          else: @[]


  return (
    edges & #% edges withing curent toplevel
    node & #% node description for current node
    underTargets.mapIt(it[0]).join("\n"), #% recursively applied to get the same for target nodes
    outNodes)


# XXX return tuple with list of last nodes and
# graohviz body itself
proc gvizToDot(gv: GVizNode, gvTop: GVizNode): tuple[
  dotBody: string, outNodes: seq[string]] =
  let (body, outNodes) =
    case gv.kind:
      of cnkExpr .. cnkAssgn: finalGVizToDot(gv, gvTop)
      else: nestedGVizToDot(gv, gvTop)

  return (body, outNodes)

proc topGVizToDot(body: GVizNode): string =
  # XXX create start node and pass it as top-level
  # ro builder
  var start = body
  let res: tuple[dotBody: string, outNodes: seq[string]] =
      gvizToDot(start, start)

  let stNode =
    if start.kind == cnkCond:
      start.targets[0].name
    else:
      start.name

  return &"start -> {stNode};\n" & res.dotBody & res.outNodes.mapIt(&"{it} -> end;").join("\n")



proc runTestCases() =
  let body: Option[Scope] = chartBuilder("""""")
  if body.isSome:
    let gviz = scopeToGviz(body.get)
    let conf = "splines=ortho;nodesep=0.5;ranksep=0.7;\n"
    let res = gviz.topGVizToDot()
    writeFile("graph.tmp.dot", "digraph G {\n" & conf & $res & "}")


# echo parse("for (;;) { while () {}}")

parseArgs:
  opt:
    name: "input-file"
    opt: ["--input", "+takes_value"]
    help: "Input file name"
  opt:
    name: "output-file"
    opt: ["--output", "+takes_value"]
    help: "output file name"
  opt:
    name: "test-line"
    opt: ["--test-line", "+takes_value"]
    help: "test single line"
  opt:
    name: "builtin-tests"
    opt: ["--builtin-tests"]
    help: "run builtin tests"
  opt:
    name: "debug-parse"
    opt: ["--debug-parse", "+takes_value"]
    help: "run debug parse on the statement"
  opt:
    name: "dump-tree"
    opt: ["--dump-tree"]
    help: "Dump syntax tree of input file to output file instead of generatig dot diagram"

if hasErrors:
  quit(1)


if "input-file".kp and "output-file".kp:
  let inputFile = "input-file".k.toStr()
  let outputFile = "output-file".k.toStr()

  let body: Option[Scope] = chartBuilder(readFile(inputFile).string)
  if body.isSome:
    if "dump-tree".kp:
      let scopeDump = body.get.mapItBFStoSeq(
        children,
        block:
          let ident = "  ".repeat(lv)
          let comm = tern(it.kind in cnkExpr .. cnkAssgn, it.text, "")
          ident & $it.kind & ":  " & comm
      ).join("\n")

      writeFile(outputFile, scopeDump)
    else:
      let gviz = scopeToGviz(body.get)
      let conf = "splines=spline;nodesep=0.5;ranksep=0.7;\n"
      let res = gviz.topGVizToDot()
      writeFile(outputFile, "digraph G { graph [ dpi = 300 ]; \n" & conf & $res & "}")
  else:
    if "dump-tree".kp:
      writeFile(outputFile, "Failed to parse input file")
    else:
      writeFile(outputFile, "digraph G {  graph [ dpi = 300 ];  fail[label=\"failed to parse\"]; }")

elif "test-line".kp():
  test("test-line".k.toStr())

elif "debug-parse".kp:
  let instr = "debug-parse".k.toStr
  let res = debugParse(instr)
  ceUserInfo2(&"parsing {instr}")
  if res > 0:
    showArrow("",instr, res, &"parse: ok ({res})")
  else:
    ceUserInfo0("parse: fail")

elif "builtin-tests".kp():
  runTestCases()
