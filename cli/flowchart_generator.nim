import pegs
import stacks
import npeg
import termformat, argparse, helpers
import os, strutils, sequtils, macros
import strformat
import tables

parseArgs:
  opt:
    name: "input-file"
    opt: ["--input", "+takes_value"]
    help: "Input file name"
  opt:
    name: "output-file"
    opt: ["--output", "+takes_value"]
    help: "output file name"

if hasErrors:
  quit(1)


let inputFile = "input-file".k.toStr()
let outputFile = "output-file".k.toStr()

echo "Out file: ", outputFile
echo "In  file: ", inputFile

let inputString = readFile(inputFile).string

echo "In file content: \n\n", inputString

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
let debugParse = pegAst.eventParser:
  pkNonTerminal:
    enter:
      let ident = "| ".repeat(dbgLvl)
      echo ident, &"{p.nt.name}"
      inc dbgLvl
    leave:
      dec dbgLvl
      let ident = "  ".repeat(dbgLvl)
      let matchStr = dbgStr.substr(start, start + length - 1)
      if length > 0:
        echo ident, &"{p.nt.name} ok"
      else:
        echo ident, &"{p.nt.name} fail"
  pkChar:
    leave:
      let ident = "  ".repeat(dbgLvl)
      if start < dbgStr.len:
        let matchChar = dbgStr[start]
        if length > 0:
          echo ident, matchChar, " ok"
        else:
          echo ident, "[ ", matchChar, " ]", " fail"



proc test(str: string, andParse: bool = false, dbg = false): void =
  echo fmt"{str:<40}: {str.match(pegAst)}"
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
    cnkCond
    cnkFor
    cnkUndef



  Node = ref object
    childNodes: seq[Node]
    case kind: NodeKind:
      of cnkIf .. cnkWhile: cond: string
      else: nil


proc strToNode(str: string): Node =
  case str:
    of "if_stmt": Node(kind: cnkIf)
    else: Node(kind: cnkUndef)

var s: Stack[ref Node]

let chartBuilder = pegAst.eventParser:
  pkNonTerminal:
    enter:
      s.push(strToNode(p.nt.name))
      echo &"add {p.nt.name} to stack"
    leave:
      if length > 0:
        echo &"add {p.nt.name} to prev node"
      else:
        echo &"remove {p.nt.name} from stack"


# echo parse("if ( )")
# echo parse("for (i = 0;;)")

# proc testr(str: rule, std: text): void =
#   let pnt = pegAst.nt(str)

test("{ }")
test("if () {}")
test("if ( );")
test("for (;;); ")
test("for (i = 0;;);")
test("for (int i = 0;;);")
test("for (;i < 10;);")
test("for (;;++i);")
test("for (int i = 0; i < 10; ++i);")
test("while () {}")
test("while ();")
test("for (;;) { }")
test("for (;;) { while () {} }")

test("for (int i = 0; i < 10; ++i) { while () { if () {} } }")

echo chartBuilder("if () {}")

# echo parse("for (;;) { while () {}}")
