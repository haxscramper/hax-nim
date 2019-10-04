import pegs
import options
import termformat, argparse, helpers
import os, strutils, sequtils, macros
import colecho_lib
import colecho_types
import strformat
import tables
import deques

# XXX remove this
var level = 0

let pegGrammar = readFile("grammar.pegs").string
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
    cnkIf
    cnkElif
    cnkWhile
    cnkElse
    cnkFor
    cnkCond
    cnkAssgn
    cnkExpr
    cnkUndef
    cnkTerminal



  Node = ref object
    childNodes: seq[Node]
    case kind: NodeKind:
      of cnkIf .. cnkWhile: cond: Option[string]
      # XXX replace with seq. There is no need tonstore so
      # detailed information. This woukd also simplify
      # child placement code, by reducing number of special
      # cases.
      of cnkFor:
        init: Option[string]
        bound: Option[string]
        postAct: Option[string]
      else: nil



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
    childNodes,
    " ".repeat(lv) & $(it.kind)
  ).join("\n")



type
  Scope = ref object
    node: Node
    name: string
    text: string
    children: seq[Scope]



proc placeChildNode(stack: var seq[Scope], child: Scope) =
  var top = stack.pop()
  var tn = top.node
  let tkind = top.node.kind
  var addAsChild = false
  echoi 1, "Placing child: ", child.text
  # XXX reverse handling of the child nodes. instewd of selecting
  # based on node type do this based on top level node type.
  # if top is for loop append expressio|assignment to node expr
  # until length is three. rverything else goes to child nodes.
  # to remove another specisl case we could just get number of
  # 'internal' nodes.
  #[
  let internal = case tn.kind:
  	of cnkFor: 3
  	of cnkIf .. cnkWhile: 1
  	else: 0
  ]#

  case child.node.kind:
    of cnkAssgn:
      if tkind == cnkFor and tn.init.isNone:
        tn.init = child.text
      else:
        addAsChild = true
    of cnkExpr:
      case tkind:
        of cnkFor:
          if tn.bound.isNone:
            tn.bound = child.text
          elif tn.postAct.isNone:
            tn.postAct = child.text
          else:
            addAsChild = true
        of cnkIf .. cnkWhile:
          if tn.cond.isNone:
            tn.cond = child.text
          else:
            addAsChild = true
        else:
          addAsChild = true

    else: addAsChild = true

  top.node = tn

  if addAsChild:
    echoi 2, "As child node for top"
    top.children.add(child)

  stack.add(top)

func getName(st: seq[Scope]): string = tern(st.len == 0, "start_", st[^1].name)

proc chartBuilder(str: string): Option[Scope] =
  echoi "Building chart for: ", str
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
            node: toNode(p.nt.name)
          ))
        # elif p.nt.name notin allowed:
        #   echoi 1, "Skipping", p.nt.name
      leave:
        if not done and p.nt.name in allowed:
          if length > 0:
            var child = stack.pop()
            if stack.len > 0:
              child.text = s.substr(start, start + length - 1)
              echoi "found", child.text, child.node.kind
              stack.placeChildNode(child)
            else:
              done = true
              stack.add(child)
          else:
            discard stack.pop()
            # echoi stack.len, &"remove {p.nt.name} from stack"

  let res = chartBuilderImpl(str)
  if res > 0:
    showArrow("", str, res, &"parse: ok ({res})")
    return stack[^1]


# echo parse("if ( )")
# echo parse("for (i = 0;;)")

# proc testr(str: rule, std: text): void =
#   let pnt = pegAst.nt(str)

type
  ScopeDescr = object
    start: string
    final: string
    body: string
    node: Node


type
  # IDEA treat this as DAG
  # XXX target nodes are statemrnts that are located
  # inside the scope of top node. If top node is cond
  # then tsrgets are if/else/elif statements under it.
  # there should be no other targets for cond other than
  # that.
  GVizNode = ref object
    name: string
    targets: seq[GVizNode]
    style: string
    text: seq[string]
    kind: NodeKind
    isStart: bool


func enumerate(scopes: seq[Scope]): seq[Scope] =
  toSeq(pairs(scopes)).mapIt(
    block:
      var res = it.val
      res.name = res.name & "_" & $it.key
      res
  )


proc makeDotNode(stmt: ScopeDescr, name: string, start: bool = true): string =
  echoi level, "Make node for", name
  let shape = case stmt.node.kind:
    of cnkFor:
      "shape=" & (if start: "trapezium" else: "invtrapezium")
    of cnkWhile:
      "shape=" & (if start: "trapezium" else: "point")
    of cnkIf:
      "shape=" & (if start: "diamond" else: "point")
    of cnkElif:
      "shape=" & tern(start, "diamond", "point")
    of cnkElse, cnkCond:
      "shape=point"
    else: ""

  let style = case stmt.node.kind:
    of cnkFor: ""
    else: tern(start, "", "style=bold")

  let label = case stmt.node.kind:
    of cnkIf .. cnkWhile: &"label=\"{stmt.node.cond.get()}\""
    of cnkFor:
      block:
        let nd = stmt.node
        if start:
          if nd.init.isSome and nd.bound.isSome:
            &"label=\"{nd.init.get()}\n{nd.bound.get()}\""
          else: ""
        else:
          if stmt.node.postAct.isSome:
            &"label=\"{stmt.node.postAct.get()}\""
          else: ""
    else: &"label=\"{name}\""

  result = &"{name}[" &
     join(@[shape, label, style
            , &"xlabel=\"{name}\""
     ].filterIt(it != ""), ",") &
     "];\n"

proc joinScope(str, startWith, endWith: string): string =
  result &= startWith & "_body" & "[label=\"" & str & "\"" & ",shape=box];\n"

proc joinScope(
  statements: seq[ScopeDescr],
  startWith: string, endWith: string,
  top: Node
     ): string =
  var prevEnd = startWith
  echoi level, "Joining scope under", startWith
  echoi level, "To", endWith
  inc level
  if top.kind == cnkCond:
    for stmt in statements:
      if stmt.node.kind notin @[cnkIf, cnkElif, cnkElse, cnkCond]:
        ceUserError0("stmt child is not if/else/elif")


    var branches: string

    for stmt in statements:
      result &= stmt.makeDotNode(stmt.start, true)
      result &= stmt.makeDotNode(stmt.final, false)
      let fromPrev = prevEnd & " -> " & stmt.start & "[xlabel=no];\n" & stmt.body
      # if stmt.node.kind in @[cnkIf, cnkElif, cnkElse]:
      #   branches &= fromPrev
      # else:
      result &= fromPrev

      result &= stmt.final & " -> " & endWith & "[xlabel=yes];\n"

      prevEnd = stmt.start

    result = "//Child nodes for cond node at " &
      startWith &
      "\n{\nrank=same;\n" & branches & "}\n" & result

  else:
    for stmt in statements:
      result &= stmt.makeDotNode(stmt.start, true)
      result &= stmt.makeDotNode(stmt.final, false)
      result &= prevEnd & " -> " & stmt.start & ";\n" & stmt.body
      prevEnd = stmt.final

  result &= prevEnd & " -> " & endWith & ";\n"


proc scopeToGraph(scope: Scope): ScopeDescr =
  let start = scope.name & "_st"
  let final = scope.name & "_en"

  echo "converting scope ", scope.name
  echo scope.text

  let inner = case scope.node.kind:
    of cnkExpr, cnkAssgn:
      scope.text.joinScope(start, final)
    else:
      scope.children.mapIt(it.scopeToGraph()).joinScope(start, final, scope.node)

  let publicStart = case scope.node.kind:
    of cnkExpr, cnkAssgn: start & "_body"
    of cnkCond: scope.children[0].name & "_st"
    else: start

  let publicFinal = case scope.node.kind:
    of cnkExpr, cnkAssgn: start & "_body"
    else: final


  return ScopeDescr(
    start: publicStart,
    final: publicFinal,
    body: inner,
    node: scope.node
  )

func enumerate(scope: Scope): Scope =
  result = scope
  result.children = scope.children.enumerate()

proc scopeToDot(inScope: Scope): string =
  let scope = enumerate(inScope)
  let top = scopeToGraph(scope)
  let toplevel = Node(kind: cnkTerminal)
  joinScope(@[top], "start", "end", toplevel)

proc scopeToGviz(inScope: Scope): GVizNode =
  let kind = inScope.node.kind
  let nd = inScope.node
  GVizNode(
    kind: kind,
    name: inScope.name,
    text: case kind:
      of cnkFor: @[nd.init, nd.bound, nd.postAct].mapIt(it.get)
      of cnkIf .. cnkWhile: @[nd.cond.get]
      of cnkExpr, cnkAssgn: @[inScope.text]
      else: @[],
    # XXX ignore children if thisnis expression or
    # assignment
    targets: inScope.children.mapIt(it.scopeToGviz)
  )

# TODO
# template forEachLeaf(topNode: typed, subNode: untyped, op: untyped): untyped =
#   ## Call operator on each leaft node (i.e. subnode which has empty
#   ## subNode) does not guarantee to visit each leaf once

proc getStyle(gv: GVizNode): string =
  @[
    &"label=\"{gv.name}\"",
    tern(gv.kind in @[cnkIf, cnkElif], "shape=diamond", "")
  ].filterIt(it.len > 0).join(",")

# TODO
# macro if1(head, body: untyped): untyped =
#   ## Block-style ternary expression. Evaluate `t:` part if head is
#   ## true otherwise evaluate `f:`.
#   quote do:
#     if `head`:

# XXX return tuple with list of last nodes and
# graohviz body itself
proc gvizToDot(gv: GVizNode, gvTop: GVizNode): string =
  let edges = block:
    if gv.kind != cnkCond:
      # if this is for|while loop add two edges: first for
      # input to toplevel node, second for output from it.
      # This weay if 'it' is for loop it will have edge
      # coming in its head, and edge comint from its tail.
      gv.targets.mapIt(&"{gv.name} -> {it.name};" ).join("\n")
    else:
      var prev = gv.targets[0].name
      let start = prev
      var res: string
      for targ in gv.targets[1..^1]:
        if targ.kind != cnkElse:
          res &= &"{prev} -> {targ.name}[xlabel=no];\n"
          prev = targ.name
        else:
          res &= &"{prev} -> {targ.targets[0].name}[label=no];\n"

      &"{gv.name} -> {start};\n{{\nrank=same;\n{res}}}\n"

  let nodes = gv.targets.mapIt(&"{it.name}[{it.getStyle}];").join("\n")
  # convert returned seq of pairs into pair of sequences.
  # if we were working with for/while... we add all last
  # nodes to the end target node. connection from last node
  # if hor loop is handled by node that is one level above
  let underTargets = gv.targets.mapIt(it.gvizToDot(gv)).join("\n")
  # XXX we need to return listbof last nodes to upper
  # so tgat would be connected to whatever comes after tgem
  # if toplevel is a loop it would add new node and export
  # it. if this is a cond we would pass everything forward.
  # if this is an expression of assignment we should
  # passthrough nodes as well.
  return edges & nodes & underTargets

proc topGVizToDot(body: GVizNode): string =
  # XXX create start node and pass it as top-level
  # ro builder
  var start = body
  return gvizToDot(start, start)



proc runTestCases() =
  let body: Option[Scope] = chartBuilder(
    "if (a) { int a = 0; } else if (c) { int c = 0; } else { int b = 0; };")
  if body.isSome:
    let gviz = scopeToGviz(body.get)
    let conf = "splines=ortho;nodesep=1;ranksep=1;\n"
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

if hasErrors:
  quit(1)


if "input-file".kp():
  let inputFile = "input-file".k.toStr()
  let outputFile = "output-file".k.toStr()
  # echo "Out file: ", outputFile
  # echo "In  file: ", inputFile

  # let inputString = readFile(inputFile).string

  # echo "In file content: \n\n", inputString

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
