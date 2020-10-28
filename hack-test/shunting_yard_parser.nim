import strutils, sequtils, strformat

# Nim code is mostly adapted from
# https://rosettacode.org/wiki/Parsing/Shunting-yard_algorithm#Nim Ast
# construction added using logic from
# https://www.klittlepage.com/2013/12/22/twelve-days-2013-shunting-yard-algorithm/

type
  Lexer = object
    buf: seq[string]
    idx: int


func `[]`(l: Lexer, idx: int = 0): string =
  result = l.buf[l.idx + idx]

func next(l: var Lexer) = inc l.idx
func finished(l: Lexer): bool = l.idx == l.buf.len

type
  Ast = object
    head: string
    subn: seq[Ast]

  TokKind = enum
    tkOpen ## Open group in expression
    tkClose ## Close group in expression
    tkOp ## Operator token
    tkValue ## Start of value
    tkExprEnd ## End expression parsing before lexer is explicitly
              ## finished

proc ptree(ast: Ast, level: int = 0): void =
  let pref = "  ".repeat(level)
  echo pref, ast.head
  for sn in ast.subn:
    ptree(sn, level + 1)


func addNode*(st: var seq[Ast], op: string) =
  st.add Ast(subn: @[st.pop, st.pop], head: op)

proc shuntAst(
  lex: var Lexer,
  classify: proc(tk: string): TokKind,
  prec: proc(op1: string): int,
  isRassoc: proc(op: string): bool,
  parseValue: proc(lex: var Lexer): Ast): Ast =
  ## :parseValue: Custom parser logic for syntax parts that are not
  ##   expressions. When encountering `tkValue` elemen in token stream
  ##   control loop calls `parseValue` callback which can move lexer
  ##   as far ahead as necessary.
  ## :classify: Determine kind of token at current position;
  ## :prec: Get precedence value for operator
  ## :isRassoc: Whether or not operator is right-associative
  var operatorStack: seq[string]
  var operandStack: seq[Ast]
  while not lex.finished:
    let token = lex[]
    case classify(token)
      of tkOpen:
        operatorStack.add token
        lex.next

      of tkClose:
        while operatorStack.len > 0:
          let op = operatorStack.pop()
          if classify(op) == tkOpen:
            break

          operandStack.addNode(op)

        lex.next
      of tkOp:
        while operatorStack.len > 0:
          let op = operatorStack[^1]
          if classify(op) != tkOp:
            break

          if prec(token) > prec(op) or
            (isRassoc(token) and prec(token) == prec(op)):
            break
          discard operatorStack.pop()
          operandStack.addNode op
        operatorStack.add token
        lex.next

      of tkValue:
        operandStack.add parseValue(lex)

      of tkExprEnd:
        break

  while operatorStack.len > 0:
    operandStack.addNode(operatorStack.pop)

  return operandStack.pop


func classify(tk: string): TokKind =
  case tk[0]:
    of '(': tkOpen
    of ')': tkClose
    of '0' .. '9': tkValue
    of '*', '+', '-', '~', '^', '/': tkOp
    of '[', '{': tkValue
    else: tkValue

func prec(op: string): int =
  case op[0]:
    of '^': 4
    of '*', '/': 3
    of '+', '-': 2
    else: 0

func isRassoc(op: string): bool =
  (op == "^")

func parseValue(lex: var Lexer): Ast =
  let tok = lex[]
  if tok[0] in {'0' .. '9'}:
    lex.next
    return Ast(head: tok)
  elif tok[0] in {'[', '{'}:
    let op = if tok == "[": "]" else: "}"
    result = Ast(head: tok & op)
    lex.next
    while lex[] != op:
      result.subn.add parseValue(lex)

    lex.next

  else:
    raiseAssert(&"Invalid value: {lex[]}")

let input = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ [ 3 4 5 ]"
var lex = Lexer(buf: input.strip.split)

ptree shuntAst(
  lex,
  classify = classify,
  isRassoc = isRassoc,
  parseValue = parseValue,
  prec = prec
)
