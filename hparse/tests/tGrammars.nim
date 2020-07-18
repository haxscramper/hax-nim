import sugar, strutils, sequtils, strformat
import hparse/grammars

include test_grammar

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest
const nt = nterm[TokenKind]

suite "EBNF -> BNF convesion":
  test "wee":
    let ebnf = andP(
      nt("element"),
      zeroP(andP(
        tok(tkComma),
        nt("element")
      ))
    )

    echo ebnf.exprRepr()
    let (top, newrules) = ebnf.toBNF("X")
    echo "toprule: ", top.exprRepr()
    for rule in newrules:
      echo rule.exprRepr()
