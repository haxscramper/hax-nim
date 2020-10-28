import tables, strutils, sequtils, strformat

import hpprint

type operator = tuple[prec:int, rassoc:bool]

let ops = newTable[string, operator](
  [ ("^", (4, true )),
    ("*", (3, false)),
    ("/", (3, false)),
    ("+", (2, false)),
    ("-", (2, false)) ])

type
  Ast = object
    head: string
    subn: seq[Ast]

  Stack = seq[Ast]

func addNode*(st: var Stack, op: string) =
  st.add Ast(subn: @[st.pop, st.pop], head: op)

proc shuntAst(tokens: seq[string]): Ast =
  var operatorStack: seq[string]
  var operandStack: seq[Ast]
  for token in tokens:
    case token
    of "(": operatorStack.add token
    of ")":
      while operatorStack.len > 0:
        let op = operatorStack.pop()
        if op == "(":
          break

        operandStack.addNode(op)
    else:
      if token in ops:
        while operatorStack.len > 0:
          let op = operatorStack[^1]  # peek stack top
          if not (op in ops): break
          if ops[token].prec > ops[op].prec or
             (ops[token].rassoc and (ops[token].prec == ops[op].prec)):
            break
          discard operatorStack.pop()
          operandStack.addNode op
        operatorStack.add token
      else:
        operandStack.add Ast(head: token)

  while operatorStack.len > 0:
    operandStack.addNode(operatorStack.pop)

  return operandStack.pop

let input = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

pprint shuntAst(input.strip.split)
