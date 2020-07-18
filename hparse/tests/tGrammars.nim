import sugar, strutils, sequtils, strformat
import hparse/grammars

include test_grammar

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest
const nt = nterm[TokenKind]

suite "EBNF -> BNF convesion":
  test "wee":
    echo andP(
      nt("element"),
      zeroP(andP(
        tok(tkComma),
        nt("element")
      ))
    ).exprRepr()
