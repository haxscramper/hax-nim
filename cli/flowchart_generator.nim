import pegs
import options
import termformat, argparse, helpers
import os, strutils, sequtils, macros
import colecho_lib
import colecho_types
import strformat
import tables
import deques



# let parser = peg "document":
#   document <- +code_block * Skip
#   code_block <- +( if_stmt | for_loop | while_loop )

#   if_stmt <- "if" * Skip * "(" * Skip * expr_primary * Skip * ")" * Skip * brace_stmt:
#     echo "Found if statement"

#   for_loop <- "for" * Skip * "(" * exp * ";" * exp * ";" * exp * ")" * Skip * brace_stmt:
#     echo "Found for loop"

#   while_loop <- "while" * Skip * "(" * Skip * exp * Skip * ")" * Skip * brace_stmt:
#     echo "Found while loop"

#   funccall <- ident * "()" * Skip

#   exp <- expr_primary # To reduce typing
#   brace_stmt <- "{" * Skip * *( code_block ) * Skip * "}"
#   Skip <- *( Blank | Space | '\n' )

#   expr_primary <- expr | ident | constant | expr_decl | "(" * expr * ")"
#   var_decl <- type_spec * Skip * expr_assign

#   expr <- expr_assign | expr_binary | expr_unary | funccall
#   expr_decl <- type_spec * expr_assign
#   expr_assign <- Skip * +Alnum * Skip * oper_assign * Skip * expr
#   expr_binary <- Skip * expr * Skip * oper_binary * Skip * expr
#   expr_unary <- oper_unary * expr
#   type_spec <- ident # TODO

#   ident <- Skip * +Alnum * Skip
#   constant <- Skip * +Digit * Skip
#   oper_assign <- Skip * "=" * Skip
#   oper_unary <- Skip * ( "&" | "*" | "+" | "-" | "~" | "!" ) * Skip
#   oper_binary <- Skip * ( "+" | "<" ) * Skip

# echo parser.match(inputString)

# let pegAst = """
# document <- code_block*
# code_block <- for_loop
# for_loop <- "for" "(" expression ";" expression ";" expression ")" "{" *code_block "}"
# expression <- assign_expr | compare_expr
# assign_expr <- \ident "="
# """

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
let ignored = @["ws"]

# TODO FIXME dump code generated by template/macro inocation
macro debugInvocation(head, body: typed): typed =
  let ast = quote do:
    `body`

  result = body

# IDEA write parsing stages to file with moving arrow
# IDEA generate terminal animation from file parsing
proc showArrow(indent: string, topString: string, pos: int, annot: string = "") =
  echo indent, topString
  echo indent, " ".repeat(pos - 1), "^ ", annot

let dumpAll = false

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
          echo ident, "[ ", p.ch, " ]", $toGreen(" ok")
        else:
          echo ident, "[ ", p.ch, " ]", $toRed(" fail")
    pkTerminal:
      leave:
        let ident = "    ".repeat(dbgLvl)
        if length > 0:
          echo ident, p.term, " ", $toGreen("ok")
        else:
          echo ident, p.term, " ", $toRed("fail")



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



  Node = ref object
    childNodes: seq[Node]
    case kind: NodeKind:
      of cnkIf .. cnkWhile: cond: string
      of cnkFor:
        init: Option[string]
        bound: Option[string]
        postAct: Option[string]
      else: nil



proc toNodeKind(str: string): NodeKind =
  case str:
    of "if_stmt": cnkIf
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

proc echoi(indent: int, message: varargs[string, `$`]): void =
  echo "  ".repeat(indent), message.join(" ")


type
  Scope = ref object
    node: Node
    name: string
    text: string
    children: seq[Scope]


func getName(st: seq[Scope]): string = st.mapIt(it.name).join("_")

proc placeChildNode(stack: var seq[Scope], child: Scope) =
  var top = stack.pop()
  let tkind = top.node.kind
  var addAsChild = false
  case child.node.kind:
    of cnkAssgn:
      if tkind == cnkFor and top.node.init.isNone:
        top.node.init = child.text
      else:
        addAsChild = true
    of cnkExpr:
      if tkind == cnkFor:
        if top.node.bound.isNone:
          top.node.bound = child.text
        elif top.node.postAct.isNone:
          top.node.postAct = child.text
        else:
          addAsChild = true
    else: addAsChild = true

  if addAsChild:
    top.children.add(child)

  stack.add(top)


proc chartBuilder(str: string): Option[Scope] =
  var done: bool = false
  var stack: seq[Scope]
  let allowed = @["if_stmt", "while_loop", "expr", "assgn", "for_loop"]
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
            # echoi stack.len, &"remove {p.nt.name} from stack"

  if chartBuilderImpl(str) > 0:
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

func enumerate(scopes: seq[Scope]): seq[Scope] =
  toSeq(pairs(scopes)).mapIt(
    block:
      var res = it.val
      res.name = res.name & "_" & $it.key
      debugEcho res.name
      res
  )


proc makeDotNode(stmt: ScopeDescr, name: string, start: bool = true): string =
  let shape = case stmt.node.kind:
    of cnkFor:
      "shape=" & (if start: "trapezium" else: "invtrapezium")
    of cnkWhile:
      "shape=" & (if start: "trapezium" else: "point")
    of cnkIf:
      "shape=" & (if start: "diamond" else: "point")
    else: ""

  let label = case stmt.node.kind:
    of cnkIf .. cnkWhile: &"label=\"{stmt.node.cond}\""
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
     tern(shape.len > 0, shape & ",", "") &
     tern(label.len > 0, label & ",", "") &
    "];\n"

proc joinScope(
  statements: seq[ScopeDescr],
  startWith: string, endWith: string
     ): string =
  var prevEnd = startWith
  for stmt in statements:
    echo stmt.node.kind
    result &= stmt.makeDotNode(stmt.start, true)
    result &= stmt.makeDotNode(stmt.final, false)
    result &= prevEnd & " -> " & stmt.start & ";\n" & stmt.body
    prevEnd = stmt.final

  result &= prevEnd & " -> " & endWith & ";\n"


proc scopeToGraph(scope: Scope): ScopeDescr =
  let start = scope.name & "_startScp"
  let final = scope.name & "_endScp"

  let inner = scope.children.mapIt(it.scopeToGraph()).joinScope(start, final)

  return ScopeDescr(
    start: start,
    final: final,
    body: inner,
    node: scope.node
  )

func enumerate(scope: Scope): Scope =
  result = scope
  result.children = scope.children.enumerate()

proc scopeToDot(inScope: Scope): string =
  let scope = enumerate(inScope)
  let top = scopeToGraph(scope)
  joinScope(@[top], "start", "end")

proc runTestCases() =
  let body: Option[Scope] = chartBuilder(
    "for (int i = 0; i < 10; ++i) { }")
  if body.isSome:
    let conf = "splines=ortho;\n"
    let res = body.get.scopeToDot
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
    ceUserInfo0(&"parse: ok ({res})")
  else:
    ceUserInfo0("parse: fail")

elif "builtin-tests".kp():
  runTestCases()
