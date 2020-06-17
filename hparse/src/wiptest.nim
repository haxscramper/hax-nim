import ll1_gen, grammars, macros

type
  TokenKind = enum
    tkOpBrace
    tkCloseBrace
    tkIdent
    tkComma

  Token = object
    kind: TokenKind

  TPatt = Patt[TokenKind]



import unittest, sequtils

import hmisc/hpprint

suite "Token streams":
  template newtoks(): untyped =
    var ts {.inject.} = makeStream(@[
      Token(kind: tkOpBrace),
      Token(kind: tkIdent),
      Token(kind: tkComma),
      Token(kind: tkIdent),
      Token(kind: tkCloseBrace)
    ])


  test "Get all tokens":
    newtoks()
    assertEq toSeq(ts).mapIt(it.kind), @[
      tkOpBrace, tkIdent, tkComma, tkIdent, tkCloseBrace
    ]


macro grammarTest(): untyped =
  let grammar = {
    # list ::= '[' <elements> ']'
    "list" : TPatt(kind: pkConcat, patts: @[
      TPatt(kind: pkTerm, tok: tkOpBrace),
      TPatt(kind: pkNTerm, sym: "elements"),
      TPatt(kind: pkTerm, tok: tkCloseBrace)
    ]),
    # elements ::= <element> (',' <element>)*
    "elements" : TPatt(kind: pkConcat, patts: @[
      TPatt(kind: pkNterm, sym: "element"),
      TPatt(kind: pkZeroOrMore, opt: TPatt(kind: pkConcat, patts: @[
        TPatt(kind: pkTerm, tok: tkComma),
        TPatt(kind: pkNTerm, sym: "element")
      ]))
    ]),
    # element ::= 'ident' | <list>
    "element" :  TPatt(kind: pkAlternative, patts: @[
      TPatt(kind: pkTerm, tok: tkIdent),
      TPatt(kind: pkNTerm, sym: "list")
    ])
  }.toGrammar()

  let compGrammar = computeGrammar(grammar)
  # pprint compGrammar
  let impl = makeGrammarParser(compGrammar)

  result = impl

grammarTest()

var testStream = makeStream(@[
  Token(kind: tkOpBrace),
  Token(kind: tkIdent),
  Token(kind: tkComma),
  Token(kind: tkIdent),
  Token(kind: tkComma),
  Token(kind: tkIdent),
  Token(kind: tkCloseBrace)
])

parseList(testStream)
