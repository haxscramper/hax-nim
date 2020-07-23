import sugar, strutils, sequtils, strformat


#===========================  implementation  ============================#

import hparse/[
  ll1_table,
  grammars,
  bnf_grammars, # FIXME HACK not necessary but removeing it generates
                # `rule cannot be called` compilation error.
  lexer
]

import hmisc/algo/hseq_mapping # HACK fixes `undeclared field 'nthType1'`

include test_grammar

#================================  tests  ================================#

import unittest

suite "Predictive LL(1)":
  let nt = nterm[TokenKind]
  test "Simple grammar":
    let nte = nt("element")
    let grammar = {
      # list ::= '[' <elements> ']'
      "list" : andP(tok(tkOpBrace), nt("elements"), tok(tkCloseBrace)),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nte,
        zeroP(andP(tok(tkComma), nt("element")))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(tok(tkIdent), nt("list"))
    }.toGrammar()

    let tableParser = newLL1TableParser(grammar, retainGenerated = false)

    var stream = mapString("[a,b,c,d,e]").makeStream()
    let tree = tableParser.parse(stream)
    tree.topng("/tmp/tree.png")
