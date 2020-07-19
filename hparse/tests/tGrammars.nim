import sugar, strutils, sequtils, strformat
import hparse/grammars

include test_grammar

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest
const nt = nterm[TokenKind]

suite "EBNF -> BNF convesion":
  proc conv[Tk](patt: Patt[Tk]): void =
    echo "\n\e[41mVVVVVVVVVVVVVVVVVVVV\e[0m"
    let ebnf = rule("X", patt)

    echo "\e[42minput:\e[0m"
    echo ebnf.exprRepr()
    block:
      echo "\e[42mregular:\e[0m"
      let newrules = ebnf.toBNF()
      for rule in newrules:
        echo rule.exprRepr()

    block:
      echo "\e[42mflattened:\e[0m"
      let newrules = ebnf.toBNF(true)
      for rule in newrules:
        echo rule.exprRepr()

  test "wee":
    conv(andP(nt("EEE"), tok(tkComma)))

    conv(andP(
      nt("element"),
      zeroP(andP(
        oneP(tok(tkComma)),
        zeroP(nt("element"))
      )),
      optP(tok(tkCloseBrace))))
