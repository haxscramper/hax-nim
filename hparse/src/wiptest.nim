import ll1_gen, grammars

type
  TokenKind = enum
    tkOpBrace
    tkCloseBrace
    tkIdent
    tkComma

  Token = object
    kind: TokenKind

  TPatt = Patt[TokenKind]


macro grammarTest() =
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
